module IR.Constraint.Constrain.Pattern where

import qualified Common.Identifiers            as Ident
import           Control.Monad                  ( foldM
                                                , unless
                                                )
import           Data.Bifunctor                 ( second )
import qualified Data.Map.Strict               as Map
import qualified IR.Constraint.Canonical       as Can
import qualified IR.Constraint.Instantiate     as Inst
import           IR.Constraint.Monad            ( TC
                                                , freshVar
                                                , getDConInfo
                                                , throwError
                                                )
import           IR.Constraint.Type            as Type
import qualified IR.IR                         as I


-- | CONSTRAIN ALT

type Header = Map.Map Ident.Identifier Type

data State = State
  { _headers :: Header
  , _vars    :: [Variable]
  , _revCons :: [Constraint]
  }

add :: I.Alt -> Type -> State -> TC State
add alt expected state = case alt of
  I.AltDefault binder -> do
    var <- binderToVarId binder
    return $ addToHeaders (Ident.fromId var) expected state
  I.AltLit lit      -> return $ addLit lit expected state
  I.AltData dcon bs -> do
    maybeInfo <- getDConInfo dcon
    case maybeInfo of
      Just (_, typeName, typeVarNames, argTypes) ->
        addData typeName typeVarNames dcon argTypes bs expected state
      Nothing ->
        -- this should already be found in scope checking
        throwError $ "Pattern: data constructor does not exist - " ++ show dcon

emptyState :: State
emptyState = State Map.empty [] []

addToHeaders :: Ident.Identifier -> Type -> State -> State
addToHeaders name tipe (State headers vars revCons) =
  let newHeaders = Map.insert name tipe headers
  in  State newHeaders vars revCons

addData
  :: Ident.TConId
  -> [Ident.TVarId]
  -> Ident.DConId
  -> [Can.Type]
  -> [I.Binder]
  -> Type
  -> State
  -> TC State
addData typeName typeVarNames ctorName ctorArgTypes bs expected state = do
  unless (length ctorArgTypes == length bs)
    $  throwError
    $  "Pattern: wrong number of argument - "
    ++ show ctorName
  varPairs <- mapM (\var -> (,) var <$> mkFlexVar) typeVarNames
  let typePairs   = map (second TVarN) varPairs
  let freeVarDict = Map.fromList typePairs
  let as          = map I.AltDefault bs -- TODO: this is a small placeholder before recursive alts
  (State headers vars revCons) <- foldM
    (\st (a, aCanType) -> addDataArg freeVarDict aCanType a st)
    state
    (zip as ctorArgTypes)
  let ctorType = TConN typeName (map snd typePairs)
  let ctorCon  = CPattern ctorType expected
  return $ State { _headers = headers
                 , _vars    = map snd varPairs ++ vars
                 , _revCons = ctorCon : revCons
                 }

addDataArg
  :: Map.Map Ident.TVarId Type -> Can.Type -> I.Alt -> State -> TC State
addDataArg freeVarDict canType arg state = do
  tipe <- Inst.fromScheme freeVarDict canType
  add arg tipe state

addLit :: I.Literal -> Type -> State -> State
addLit lit expected state =
  let litCon = case lit of
        I.LitIntegral _ -> CPattern Type.i32 expected
        I.LitEvent      -> CPattern Type.unit expected
  in  state { _revCons = (litCon : _revCons state) }

-- | BINDER HELPERS


binderToVarId :: I.Binder -> TC Ident.VarId
binderToVarId Nothing    = freshVar
binderToVarId (Just var) = return var
