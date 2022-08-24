{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Types.Unification
  ( Type
  , Scheme
  , pattern TVar
  , pattern TCon
  , applyBindings
  , freeze
  , unfreeze
  , freezeScheme
  , unfreezeScheme
  , HasFreeVars(..)
  , InferM
  , MonadReader(..)
  , MonadError(..)
  , (=:=)
  , fresh
  , generalize
  , instantiate
  , runInfer
  , isTuple
  , tuple
  , pattern U8
  , pattern I8
  , pattern U32
  , pattern I32
  , pattern U64
  , pattern I64
  , pattern Time
  , pattern List
  , pattern Ref
  , pattern Unit
  , pattern Arrow
  , foldArrow
  , unfoldArrow
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( Identifiable(..)
                                                , TConId(..)
                                                , TVarId(..)
                                                , fromString
                                                , showId
                                                )
import qualified IR.Types.Type                 as T
import           IR.Types.Type                  ( SchemeOf(Forall) )

import           Control.Unification            ( BindingMonad(freeVar)
                                                , Fallible(..)
                                                , UTerm(..)
                                                , Unifiable
                                                , getFreeVars
                                                )
import qualified Control.Unification           as U
import           Control.Unification.IntVar     ( IntBindingT
                                                , IntVar(..)
                                                , evalIntBindingT
                                                )

import           Control.Monad                  ( forM )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(..)
                                                , runExceptT
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import           Data.Set                       ( (\\) )
import           GHC.Generics                   ( Generic1 )

deriving instance Ord IntVar
deriving instance Unifiable []

data TypeF a = TConF TConId [a] | TVarF TVarId
  deriving (Eq, Functor, Foldable, Traversable, Generic1, Unifiable)

type Type = UTerm TypeF IntVar
type Scheme = T.SchemeOf Type

pattern TVar :: TVarId -> Type
pattern TVar v = UTerm (TVarF v)

pattern TCon :: TConId -> [Type] -> Type
pattern TCon tc ts = UTerm (TConF tc ts)

instance Show a => Show (TypeF a) where
  show (TConF "->" [a, b]) = show a <> " -> " <> show b
  show (TConF "[]" [a]) = "[" <> show a <> "]"
  show (TConF "&" [a]) = "&" <> show a
  show (TConF tc ts) = unwords (show tc : map show ts)
  show (TVarF v) = show v

-- | Promote a 'T.Type' to a 'Type'.
unfreeze :: T.Type -> Type
unfreeze (T.TCon tc ts) = UTerm $ TConF tc $ fmap unfreeze ts
unfreeze (T.TVar v    ) = UTerm $ TVarF v

-- | Demote a 'Type' to a regular 'T.Type'.
--
-- Fails if the unification type contains more than just contructors and
-- variables.
freeze :: Type -> InferM ctx T.Type
freeze (TCon tc ts) = mapM freeze ts <&> T.TCon tc
freeze (TVar v    ) = return $ T.TVar v
freeze t =
  throwError
    $  Compiler.UnexpectedError
    $  fromString
    $  "Could not freeze: "
    ++ show t

unfreezeScheme :: T.Scheme -> Scheme
unfreezeScheme (T.Scheme s) = fmap unfreeze s

freezeScheme :: Scheme -> InferM ctx T.Scheme
freezeScheme s = T.Scheme <$> mapM freeze s

-- | Catamorphism over unification types; useful for term rewriting.
ucata :: Functor t => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar  v) = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

substU :: M.Map (Either TVarId IntVar) Type -> Type -> Type
substU m = ucata f g
 where
  f v = fromMaybe (UVar v) $ M.lookup (Right v) m
  g (TVarF v) = fromMaybe (TVar v) (M.lookup (Left v) m)
  g t         = UTerm t

instance Fallible TypeF IntVar Compiler.Error where
  occursFailure v t =
    Compiler.TypeError
      $  fromString
      $  "Infinite type: "
      <> show v
      <> " ~ "
      <> show t
  mismatchFailure t1 t2 =
    Compiler.TypeError
      $  fromString
      $  "Cannot unify: "
      <> show t1
      <> " ~ "
      <> show t2

class HasFreeVars a where
   freeVars :: a -> InferM ctx (S.Set IntVar)

instance HasFreeVars Type where
  freeVars = fmap S.fromList . lift . lift . getFreeVars

instance HasFreeVars Scheme where
  freeVars (Forall _ _ t) = freeVars t

-- | Inference monad, build on top of the unification algorithm.
type InferM ctx
  = ReaderT ctx (ExceptT Compiler.Error (IntBindingT TypeF Compiler.Pass))

fresh :: InferM ctx Type
fresh = UVar <$> lift (lift freeVar)

infix 4 =:=
(=:=) :: Type -> Type -> InferM ctx ()
s =:= t = lift $ s U.=:= t >> return ()

applyBindings :: Type -> InferM ctx Type
applyBindings = lift . U.applyBindings

instantiate :: Scheme -> InferM ctx Type
instantiate (Forall (S.toList -> xs) _ uty) = do
  subs <- forM xs $ \v -> do
    fv <- fresh
    return (Left v, fv)
  return $ substU (M.fromList subs) uty

generalize :: HasFreeVars ctx => Type -> InferM ctx Scheme
generalize uty = do
  uty'   <- applyBindings uty
  ctx    <- ask
  tmfvs  <- freeVars uty'
  ctxfvs <- freeVars ctx
  let fvs  = S.toList $ tmfvs \\ ctxfvs
      xs   = take (length fvs) tvarNames
      subs = zip (map Right fvs) (map TVar xs)
      bty  = substU (M.fromList subs) uty'
  return $ Forall (S.fromList xs) T.CTrue bty
 where
  -- TODO: generate prettier names
  tvarNames :: [TVarId]
  tvarNames = map (("a" <>) . showId) [(1 :: Int) ..]

runInfer :: ctx -> InferM ctx a -> Compiler.Pass a
runInfer ctx m =
  m & (`runReaderT` ctx) & runExceptT & evalIntBindingT >>= \case
    Left  err -> Compiler.throwError err
    Right res -> return res

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b = UTerm (TConF "->" [a, b])

unfoldArrow :: Type -> ([Type], Type)
unfoldArrow (Arrow a b) = let (as, rt) = unfoldArrow b in (a : as, rt)
unfoldArrow t           = ([], t)

foldArrow :: ([Type], Type) -> Type
foldArrow (a : as, rt) = a `Arrow` foldArrow (as, rt)
foldArrow ([]    , t ) = t

pattern Unit :: Type
pattern Unit = UTerm (TConF "()" [])

pattern Ref :: Type -> Type
pattern Ref a = UTerm (TConF "&" [a])

pattern List :: Type -> Type
pattern List a = UTerm (TConF "[]" [a])

pattern Time :: Type
pattern Time = UTerm (TConF "Time" [])

pattern I64 :: Type
pattern I64 = UTerm (TConF "Int64" [])

pattern U64 :: Type
pattern U64 = UTerm (TConF "UInt64" [])

pattern I32 :: Type
pattern I32 = UTerm (TConF "Int32" [])

pattern U32 :: Type
pattern U32 = UTerm (TConF "UInt32" [])

pattern I8 :: Type
pattern I8 = UTerm (TConF "Int8" [])

pattern U8 :: Type
pattern U8 = UTerm (TConF "UInt8" [])

tuple :: [Type] -> Type
tuple ts = UTerm $ TConF (tupleId $ length ts) ts

-- | Tests whether a 'Type' is a tuple of some arity.
isTuple :: Type -> Bool
isTuple (UTerm (TConF n ts)) | length ts >= 2 = n == tupleId (length ts)
isTuple _ = False

-- | Construct the name of the built-in tuple type of given arity.
--
-- Fails if arity is less than 2.
tupleId :: (Integral i, Identifiable v) => i -> v
tupleId i
  | i >= 2    = fromString $ "(" ++ replicate (fromIntegral i - 1) ',' ++ ")"
  | otherwise = error $ "Cannot create tuple of arity: " ++ show (toInteger i)
