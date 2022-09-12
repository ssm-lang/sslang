{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module IR.Types.Constraint.TypingEnvironment where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( DConId(..)
                                                , TVarId(..)
                                                , fromString
                                                )
import           Control.Monad                  ( filterM
                                                , foldM
                                                )
import           Control.Monad.ST.Trans
import           Data.Foldable.Extra            ( findM )
import           Data.Maybe                     ( isNothing )
import           IR.Types.Constraint.Algebra    ( resultType
                                                , typConArgs
                                                )
import           IR.Types.Constraint.CoreAlgebra
                                                ( ARTerm(..)
                                                , changeARTermVars
                                                )
import           IR.Types.Constraint.Env        ( Env
                                                , envAdd
                                                , envEmpty
                                                , envLookup
                                                )
import           IR.Types.Constraint.Inference  ( InferM )
import           IR.Types.Constraint.MultiEquation
                                                ( CRTerm
                                                , Rigidity(..)
                                                , Variable
                                                , isConstant
                                                , isRigid
                                                , variable
                                                , variableName
                                                )

type AlgebraicDatatype s = [(DConId, Variable s)]

data TypeInfo s = TypeInfo (Variable s) (STRef s (Maybe (AlgebraicDatatype s)))

asTypeConstructor :: TypeInfo s -> InferM s (TypeInfo s)
asTypeConstructor x@(TypeInfo v _) = do
  isC <- isConstant v
  if isC
    then return x
    else Compiler.throwError $ Compiler.UnexpectedError $ fromString
      "Type needs to be Constant rigidity."

asTypeVariable :: TypeInfo s -> Variable s
asTypeVariable (TypeInfo v _) = v

data DataConstructor s = DataConstructor Int [Variable s] (CRTerm s)

data Environment s = Environment
  { typeInfo        :: Env TVarId (TypeInfo s)
  , dataConstructor :: Env DConId (DataConstructor s)
  }

emptyEnvironment :: Environment s
emptyEnvironment =
  Environment { typeInfo = envEmpty, dataConstructor = envEmpty }

unionTypeVariables :: Environment s -> Environment s -> Environment s
unionTypeVariables env1 env2 =
  env1 { typeInfo = typeInfo env1 ++ typeInfo env2 }

addTypeVariable
  :: Environment s -> TVarId -> Variable s -> InferM s (Environment s)
addTypeVariable env t v = do
  r <- newSTRef Nothing
  return $ env { typeInfo = envAdd (typeInfo env) t (TypeInfo v r) }

addTypeVariables :: Env TVarId (TypeInfo s) -> Environment s -> Environment s
addTypeVariables varEnv env =
  let f env' (x, k) = envAdd env' x k
  in  env { typeInfo = foldl f (typeInfo env) varEnv }

addTypeConstructor :: Environment s -> TVarId -> TypeInfo s -> Environment s
addTypeConstructor env t x = env { typeInfo = envAdd (typeInfo env) t x }

addDataConstructor
  :: Environment s -> DConId -> DataConstructor s -> Environment s
addDataConstructor env t x =
  env { dataConstructor = envAdd (dataConstructor env) t x }

lookupTypCon :: Environment s -> TVarId -> InferM s (TypeInfo s)
lookupTypCon env t = do
  let ti = envLookup (typeInfo env) t
  case ti of
    Nothing ->
      Compiler.throwError
        $  Compiler.TypeError
        $  fromString
        $  "UnboundTypeIdentifier: "
        ++ show t
    Just ti' -> asTypeConstructor ti'

findTypCon :: Environment s -> TVarId -> InferM s (Maybe (TypeInfo s))
findTypCon env t = do
  let tiOpt = envLookup (typeInfo env) t
  case tiOpt of
    Nothing -> return Nothing
    Just ti -> Just <$> asTypeConstructor ti

lookupTypeVariable :: Environment s -> TVarId -> InferM s (CRTerm s)
lookupTypeVariable env k = do
  let tiOpt = envLookup (typeInfo env) k
  case tiOpt of
    Nothing -> Compiler.throwError $ Compiler.TypeError $ fromString ""
    Just ti -> return $ TVariable (asTypeVariable ti)

foldTypeInfo :: (b -> (TVarId, TypeInfo s) -> b) -> b -> Environment s -> b
foldTypeInfo f xinit env = foldl f xinit (typeInfo env)

typConVariable :: Environment s -> TVarId -> InferM s (CRTerm s)
typConVariable env t = do
  (TypeInfo v _) <- lookupTypCon env t
  return $ TVariable v

asFun :: Environment s -> TVarId -> InferM s (CRTerm s)
asFun tenv name = do
  tiOpt <- findTypCon tenv name
  case tiOpt of
    Nothing             -> lookupTypeVariable tenv name
    Just (TypeInfo v _) -> return $ TVariable v

asEnv
  :: Foldable t
  => Environment s
  -> t (TVarId, Variable s)
  -> InferM s (Environment s)
asEnv env varList =
  let f env' (n, v) = addTypeVariable env' n v in foldM f env varList

isTypCon :: Environment s -> TVarId -> InferM s Bool
isTypCon env t = do
  tiOpt <- findTypCon env t
  return $ isNothing tiOpt

filterTypConName :: Environment s -> [Variable s] -> InferM s [Variable s]
filterTypConName tenv =
  let f v = do
        vn <- variableName v
        case vn of
          Nothing   -> return True
          Just name -> not <$> isTypCon tenv name
  in  filterM f

addTypeAndKindVariables
  :: [(TVarId, Variable s)] -> Environment s -> InferM s (Environment s)
addTypeAndKindVariables denv tenv = do
  r <- newSTRef None
  let f (n, v) = (n, TypeInfo v r)
  return $ addTypeVariables (map f denv) tenv

typConNameConflict
  :: Environment s
  -> ([Variable s], [(TVarId, CRTerm s)])
  -> InferM s ([Variable s], [(TVarId, Variable s)])
typConNameConflict env (fqs, denv) = do
  tcOpt <- findM (\(x, _) -> isTypCon env x) denv
  case tcOpt of
    Just _ -> Compiler.throwError $ Compiler.TypeError $ fromString ""
    Nothing ->
      let f (n, TVariable v) = (n, v)
          f _                = error "False"
      in  return (fqs, map f denv)

lookupDataCon :: Environment s -> DConId -> InferM s (DataConstructor s)
lookupDataCon env k = do
  let dcOpt = envLookup (dataConstructor env) k
  case dcOpt of
    Nothing ->
      Compiler.throwError
        $  Compiler.TypeError
        $  fromString
        $  "Unbound data constructor: "
        ++ show k
    Just d -> return d

rigidArgs :: CRTerm s -> InferM s [Variable s]
rigidArgs rt =
  let f acu t = case t of
        TVariable v -> do
          isR <- isRigid v
          return $ if isR then v : acu else acu
        _ -> return acu
  in  foldM f [] (typConArgs rt)

freshDataConScheme
  :: Environment s -> DConId -> InferM s ([Variable s], CRTerm s)
freshDataConScheme tenv k = do
  DataConstructor _ kvars kt <- lookupDataCon tenv k
  let mkvar _ = variable Flexible Nothing Nothing
  freshKvars <- mapM mkvar kvars
  let freshKvarsAssoc = zip kvars freshKvars
  return (freshKvars, changeARTermVars freshKvarsAssoc kt)

isRegularDataConScheme
  :: Environment s -> [Variable s] -> CRTerm s -> InferM s Bool
isRegularDataConScheme tenv kvars kt = do
  let asF = asFun tenv
  rt    <- resultType asF kt
  rArgs <- rigidArgs rt
  return $ all (`elem` kvars) rArgs && (length rArgs == length kvars)

--TODO not implemented/used
-- findAlgebraicDatatypes

freshVars env vars = do
  vs <- variableListFromNames undefined vars
  typConNameConflict env vs
