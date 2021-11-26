{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module IR.HM where

import qualified Data.Map                      as M

import qualified Common.Compiler               as Compiler

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes

import           IR.Types.TypeSystem            ( Builtin(..)
                                                , int
                                                , unit
                                                , void
                                                )
import           Common.Identifiers             ( Binder
                                                , TVarIdx(..)
                                                )
import           Control.Comonad                ( Comonad(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )

-- | A type substitution: a mapping of type variables to concrete types
type Uft = [(Classes.Type, Classes.Type)]

data InferState = InferState  { equations :: [(Classes.Type, Classes.Type)]
                              , subs :: M.Map Classes.Type Classes.Type
                              , count :: Int
                              , uft :: M.Map Classes.Type Classes.Type
                              , varMap :: M.Map I.VarId Classes.Type
                              }

-- Inference Monad
newtype InferFn a = InferFn (StateT InferState Compiler.Pass a)
  deriving Functor                      via (StateT InferState Compiler.Pass)
  deriving Applicative                  via (StateT InferState Compiler.Pass)
  deriving Monad                        via (StateT InferState Compiler.Pass)
  deriving MonadFail                    via (StateT InferState Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT InferState Compiler.Pass)
  deriving (MonadState InferState)         via (StateT InferState Compiler.Pass)

-- | Run a InferFn computation.
runInferFn :: InferFn a -> Compiler.Pass a
runInferFn (InferFn m) = evalStateT m InferState { subs = M.empty
                                                 , varMap = M.empty
                                                 , count = 0
                                                 , uft = M.empty
                                                 , equations = []}

inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = runInferFn $ do
  defs' <- inferProgramDefs $ I.programDefs p
  return $ I.Program { I.programDefs  = defs'
                     , I.programEntry = I.programEntry p
                     , I.typeDefs     = [] } -- TODO: something with I.typeDefs p  }

inferProgramDefs :: [(I.VarId, I.Expr Ann.Type)] -> InferFn [(I.VarId, I.Expr Classes.Type)]
inferProgramDefs [] = return []
inferProgramDefs ((v, e):xs) = do
  e' <- initTypes e
  eqs <- gets equations
  unifyAll eqs
  modify $ \st -> st { equations = []}
  -- eqs' <- gets equations
  -- sb' <- gets subs
  -- throwError $ Compiler.TypeError $ "dbg: " ++ show eqs' ++ show sb'
  e'' <- getType e'
  insertVar v (extract e'')
  xs' <- inferProgramDefs xs
  return $ (v, e''):xs'

vnums :: [Int]
vnums = [0..]

fresh :: InferFn Classes.Type
fresh = do
  n <- gets count
  let t = Classes.TVar $ TVarIdx (vnums !! n)
  modify $ \st -> st { count = n+1 , uft = M.insert t t $ uft st}
  return t

-- | Look up the type of a variable in the current typing context given its variable ID.
lookupVar :: I.VarId -> InferFn (Maybe Classes.Type)
lookupVar v = M.lookup v <$> gets varMap

-- | Insert a variable ID and its type into current typing context.
insertVar :: I.VarId -> Classes.Type -> InferFn ()
insertVar v t = modify $ \st -> st { varMap = M.insert v t $ varMap st }

insertBinder :: Binder -> Classes.Type -> InferFn ()
insertBinder Nothing _ = return ()
insertBinder (Just vid) t = insertVar vid t

insertEquations :: (Classes.Type, Classes.Type) -> InferFn ()
insertEquations e = modify $ \st -> st { equations = e : equations st }

insertSubst :: Classes.Type -> Classes.Type -> InferFn ()
insertSubst t1 t2 = modify $ \st -> st { subs = M.insert t1 t2 $ subs st}

-- | Helper function to support local modificaton of type context.
withNewScope :: InferFn a -> InferFn a
withNewScope inf = do
  vm <- gets varMap
  x <- inf
  modify $ \st -> st { varMap = vm }
  return x

initTypes :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
initTypes e@(I.Var v _) = do
  record <- lookupVar v
  case record of
    Nothing -> throwError $ Compiler.TypeError $ "Unbound variable: " ++ show e
    Just t' -> return $ I.Var v t' -- TODO: check anns
initTypes (I.Lambda v b _) = do
  tin <- fresh
  b' <- withNewScope do
    insertBinder v tin
    initTypes b
  return $ I.Lambda v b' (Classes.TBuiltin $ Arrow tin (extract b'))
initTypes (I.Let vs b _) = do
  (vs', b') <- withNewScope $ localInitTypes vs b
  return $ I.Let vs' b' (extract b')
  where
    localInitTypes bs body = do
      bs' <- mapM fn bs
      body' <- initTypes body
      return (bs', body')
      where
        fn (binder, e) = do
          e' <- initTypes e
          insertBinder binder (extract e')
          return (binder, e')
initTypes e@I.Prim {} = initPTypes e
initTypes (I.Lit I.LitEvent _) = return $ I.Lit I.LitEvent unit
initTypes (I.Lit i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
initTypes (I.App a b _) = do
  t <- fresh
  a' <- initTypes a
  b' <- initTypes b
  insertEquations (Classes.TBuiltin (Arrow (extract b') t), extract a')
  return $ I.App a' b' t
initTypes e = throwError $ Compiler.TypeError $ "Unable to type unknown expression: " ++ show e

-- | Infer the type of a Primitive expression.
initPTypes :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
initPTypes (I.Prim I.New [e] _) = do
  t <- fresh
  e' <- initTypes e
  insertEquations (t, Classes.TBuiltin (Ref (extract e')))
  return $ I.Prim I.New [e'] t
initPTypes (I.Prim I.Deref [e] _) = do
  t <- fresh
  e' <- initTypes e
  insertEquations (Classes.TBuiltin (Ref t), extract e')
  return $ I.Prim I.Deref [e'] t
initPTypes (I.Prim I.Dup [e] _) = do
  t <- fresh
  e' <- initTypes e
  insertEquations (t, extract e')
  return $ I.Prim I.Dup [e'] t
initPTypes (I.Prim I.Assign [lhs, rhs] _) = do
  lhs' <- initTypes lhs
  rhs' <- initTypes rhs
  insertEquations (extract lhs', Classes.TBuiltin (Ref (extract rhs')))
  return $ I.Prim I.Assign [lhs', rhs'] unit
initPTypes (I.Prim (I.PrimOp I.PrimSub) [e1, e2]  _) = do
  e1' <- initTypes e1
  e2' <- initTypes e2
  insertEquations (extract e1', int 32)
  insertEquations (extract e2', int 32)
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] $ int 32
initPTypes (I.Prim (I.PrimOp I.PrimAdd) [e1, e2]  _) = do
  e1' <- initTypes e1
  e2' <- initTypes e2
  insertEquations (extract e1', int 32)
  insertEquations (extract e2', int 32)
  return $ I.Prim (I.PrimOp I.PrimAdd) [e1', e2'] $ int 32
initPTypes (I.Prim I.After [del, lhs, rhs] _) = do
  del' <- initTypes del
  rhs' <- initTypes rhs
  lhs' <- initTypes lhs
  insertEquations (extract del', int 32)
  insertEquations (extract lhs', Classes.TBuiltin (Ref (extract rhs')))
  return $ I.Prim I.After [del', lhs', rhs'] unit
initPTypes (I.Prim I.Break [] _) = return $ I.Prim I.Break [] void
initPTypes (I.Prim I.Return [] _) = return $ I.Prim I.Return [] void
initPTypes (I.Prim I.Loop es _) = do
    es' <- mapM initTypes es
    return $ I.Prim I.Loop es' unit
initPTypes (I.Prim I.Wait es _) = do
    es' <- mapM initTypes es
    return $ I.Prim I.Wait es' unit
initPTypes (I.Prim I.Par es _) = do
    es' <- mapM initTypes es
    return $ I.Prim I.Par es' unit
initPTypes e = throwError $ Compiler.TypeError $ "Unsupported Prim expression: " ++ show e

unifyAll :: [(Classes.Type, Classes.Type)] -> InferFn ()
unifyAll [] = return ()
unifyAll ((l,r):eqs) = do
  unify l r
  unifyAll eqs

unify :: Classes.Type -> Classes.Type -> InferFn ()
unify t1 t2
  | t1 == t2 = return ()
unify tv1@(Classes.TVar _) tv2@(Classes.TVar _) = do
  r1 <- findRoot tv1
  r2 <- findRoot tv2
  if r1 == tv1 && r2 == tv2 then insertSubst r1 r2 else unify r1 r2
unify t tv@(Classes.TVar _) = unify tv t
unify tv@(Classes.TVar _) t = do
  r1 <- findRoot tv
  if r1 == tv then insertSubst r1 t else unify r1 t
unify (Classes.TBuiltin (Ref t1)) (Classes.TBuiltin (Ref t2)) = unify t1 t2
unify (Classes.TBuiltin (Arrow t1 t2)) (Classes.TBuiltin (Arrow t3 t4)) = unify t1 t3 >> unify t2 t4
unify _ _ = throwError $ Compiler.TypeError "Single unification error"

findRoot :: Classes.Type -> InferFn Classes.Type
findRoot t = do
  sb <- gets subs
  case M.lookup t sb of
    Nothing -> return t
    Just t' -> findRoot t'

getType :: I.Expr Classes.Type -> InferFn (I.Expr Classes.Type)
getType (I.Var v t) = do
  t' <- solveType t
  return $ I.Var v t'
getType (I.Lambda v b t) = do
  b' <- getType b
  t' <- solveType t
  return $ I.Lambda v b' t'
getType (I.Let vs b _) = do
  vs' <- mapM fn vs
  b' <- getType b
  return $ I.Let vs' b' (extract b')
  where
    fn (binder, e) = do
      e' <- getType e
      return (binder, e')
getType (I.Lit I.LitEvent _) = return $ I.Lit I.LitEvent unit
getType (I.Lit i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
getType (I.App a b t) = do
  a' <- getType a
  b' <- getType b
  t' <- solveType t
  return $ I.App a' b' t'
getType (I.Prim I.New [e] t) = do
  e' <- getType e
  t' <- solveType t
  return $ I.Prim I.New [e'] t'
getType (I.Prim I.Deref [e] t) = do
  e' <- getType e
  t' <- solveType t
  return $ I.Prim I.Deref [e'] t'
getType (I.Prim I.Dup [e] t) = do
  e' <- getType e
  t' <- solveType t
  return $ I.Prim I.Dup [e'] t'
getType (I.Prim I.Assign [lhs, rhs] _) = do
  lhs' <- getType lhs
  rhs' <- getType rhs
  return $ I.Prim I.Assign [lhs', rhs'] unit
getType (I.Prim (I.PrimOp I.PrimSub) [e1, e2]  _) = do
  e1' <- getType e1
  e2' <- getType e2
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] $ int 32
getType (I.Prim (I.PrimOp I.PrimAdd) [e1, e2]  _) = do
  e1' <- getType e1
  e2' <- getType e2
  return $ I.Prim (I.PrimOp I.PrimAdd) [e1', e2'] $ int 32
getType (I.Prim I.After [del, lhs, rhs] _) = do
  del' <- getType del
  rhs' <- getType rhs
  lhs' <- getType lhs
  return $ I.Prim I.After [del', lhs', rhs'] unit
getType (I.Prim I.Break [] _) = return $ I.Prim I.Break [] void
getType (I.Prim I.Return [] _) = return $ I.Prim I.Return [] void
getType (I.Prim I.Loop es _) = do
    es' <- mapM getType es
    return $ I.Prim I.Loop es' unit
getType (I.Prim I.Wait es _) = do
    es' <- mapM getType es
    return $ I.Prim I.Wait es' unit
getType (I.Prim I.Par es _) = do
    es' <- mapM getType es
    return $ I.Prim I.Par es' unit
getType e = throwError $ Compiler.TypeError $ "Unable to get type of unknown expression: " ++ show e

solveType :: Classes.Type -> InferFn Classes.Type
solveType t@(Classes.TBuiltin (Integral 32)) = return t
solveType t@(Classes.TBuiltin Unit) = return t
solveType t@(Classes.TBuiltin Void) = return t
solveType tv@(Classes.TVar _) = findRoot tv
solveType (Classes.TBuiltin (Ref t)) = do
  t' <- solveType t
  return $ Classes.TBuiltin $ Ref t'
solveType (Classes.TBuiltin (Tuple ts)) = do
  ts' <- mapM solveType ts
  return $ Classes.TBuiltin $ Tuple ts'
solveType (Classes.TBuiltin (Arrow t1 t2)) = do
  t1' <- solveType t1
  t2' <- solveType t2
  return $ Classes.TBuiltin $ Arrow t1' t2'
solveType t = throwError $ Compiler.TypeError $ "Solve Type error" ++ show t
