{-# LANGUAGE OverloadedStrings #-}
module IR.Constraint.Constrain.Annotation where

import qualified Common.Compiler               as Compiler
import qualified Common.Identifiers            as Ident
import qualified Control.Monad.Except          as Except
import           Data.Foldable                  ( foldrM )
import qualified Data.Map.Strict               as Map
import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Monad            ( TC
                                                , getKind
                                                , throwError
                                                )
import           IR.Constraint.Type            as Type


-- | CONSTRAIN ANNOTATION

type RigidMap = Map.Map Ident.TVarId Variable

data State = State
  { _rigidMap :: RigidMap
  , _flex     :: [Variable]
  }

add :: Can.Annotation -> TC (State, Type)
add ann = do
  let canType = Can.annToType ann
      state   = emptyState
  addTypeWithHoles canType state

addTypeWithHoles :: Can.Type -> State -> TC (State, Type)
addTypeWithHoles canType state@(State rigidMap flexes) = case canType of
  Can.TVar "_" -> do
    var <- mkFlexVar
    return (State rigidMap (var : flexes), TVarN var)
  Can.TVar name -> case Map.lookup name rigidMap of
    Just var -> return (State rigidMap flexes, TVarN var)
    Nothing  -> do
      var <- mkRigidVar
      return (State (Map.insert name var rigidMap) flexes, TVarN var)
  Can.TCon tcon args -> do
    maybeKind <- getKind tcon
    case maybeKind of
      Just kind -> if length args == kind
        then do
          (state', ts) <- foldrM
            (\a (st, ts) -> do
              (st', t) <- addTypeWithHoles a st
              return (st', t : ts)
            )
            (state, [])
            args
          return (state', TConN tcon ts)
        else throwError $ "Wrong arity in type annotation for " ++ show tcon
      Nothing ->
        throwError
          $  "In type annotation, type constructore does not exists: "
          ++ show tcon

emptyState :: State
emptyState = State Map.empty []
