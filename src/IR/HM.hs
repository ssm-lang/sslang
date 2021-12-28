{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module IR.HM where

import qualified Data.Map                      as M
import qualified Data.Set                      as S

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
                                                , TVarIdx(..), fromString
                                                )
import           Control.Comonad                ( Comonad(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )

-- | Inference State.
data InferState = InferState
  { varMap :: M.Map I.VarId Classes.Scheme
  , equations :: [(Classes.Type, Classes.Type)]
  , unionFindTree :: M.Map Classes.Type Classes.Type
  , count :: Int
  }

-- | Inference Monad.
newtype InferFn a = InferFn (StateT InferState Compiler.Pass a)
  deriving Functor                      via (StateT InferState Compiler.Pass)
  deriving Applicative                  via (StateT InferState Compiler.Pass)
  deriving Monad                        via (StateT InferState Compiler.Pass)
  deriving MonadFail                    via (StateT InferState Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT InferState Compiler.Pass)
  deriving (MonadState InferState)      via (StateT InferState Compiler.Pass)

-- | `runInferFn` runs a InferFn computation.
runInferFn :: InferFn a -> Compiler.Pass a
runInferFn (InferFn m) = evalStateT m InferState { unionFindTree = M.empty
                                                 , varMap = M.empty
                                                 , count = 0
                                                 , equations = [] }

-- | `inferProgram` @p@ infers the type of all the programDefs of the given porgram @p@.
inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = runInferFn $ do
  defs' <- inferProgramDefs $ I.programDefs p
  return $ I.Program { I.programDefs  = defs'
                     , I.programEntry = I.programEntry p
                     , I.typeDefs     = [] }

-- | `inferProgramDefs` @ds@ infers the type of programDefs @ds@ recursively and binds each varibale to its type.
inferProgramDefs :: [(I.VarId, I.Expr Ann.Type)] -> InferFn [(I.VarId, I.Expr Classes.Type)]
inferProgramDefs [] = return []
inferProgramDefs ((v, e):xs) = do
  e' <- initTypeVars e
  unifyAll
  e'' <- getType e'
  s <- generalize $ extract e''
  insertVar v s
  xs' <- inferProgramDefs xs
  return $ (v, e''):xs'

-- | Class of things that may contain free type variables.
class HasFreeTVars a where
  freeTVars :: a -> S.Set TVarIdx

instance HasFreeTVars TVarIdx where
  freeTVars = S.singleton

instance HasFreeTVars Classes.Scheme where
  {- | Determines the set of free type variables inside a scheme @Forall ns t@.

  The free variables are those in the underlying type @t@ minus the
  variables quantified over by the scheme in @ns@.
  -}
  freeTVars (Classes.Forall ns t) = S.difference (freeTVars t) (S.fromList ns)

instance HasFreeTVars Classes.Type where
  freeTVars (Classes.TBuiltin b) = freeTVars b
  freeTVars (Classes.TCon _ ts)  = freeTVars ts
  freeTVars (Classes.TVar n)     = S.singleton n

instance HasFreeTVars t => HasFreeTVars (Builtin t) where
  freeTVars Unit         = S.empty
  freeTVars Void         = S.empty
  freeTVars (Ref t     ) = freeTVars t
  freeTVars (Arrow l r ) = S.union (freeTVars l) (freeTVars r)
  freeTVars (Tuple tys ) = freeTVars tys
  freeTVars (Integral _) = S.empty

instance HasFreeTVars a => HasFreeTVars [a] where
  -- | Lifts 'freeTVars' to lists.
  freeTVars = foldr (S.union . freeTVars) S.empty

{- | Instantiates the type from the scheme @Forall vs t@.

Create fresh type variable for each quantified type variable in @vs@ and
substitute them in @t@ by applying @subst@ to @t@.
-}
instantiate :: Classes.Scheme -> InferFn Classes.Type
instantiate (Classes.Forall ns t) = do
  ns' <- mapM (const fresh) ns
  withNewTypeScope do
    modify $ \st -> st { unionFindTree = M.fromList (zip (map Classes.TVar ns) ns') }
    solveType t

{- | Generalizes the type @t@ into a type scheme given the typing context @ctx@.

Collect all the free type variables in the type @t@ and all the free type
variables in the typing context @ctx@,
away any type variables that appear free in the type environment, and whatever
is leftover can be generalized into a type scheme.
-}
generalize :: Classes.Type -> InferFn Classes.Scheme
generalize t = do
  vm <- gets varMap
  uft <- gets unionFindTree
  let vs = S.toList $ S.difference (freeTVars t) $ S.union (freeTVars (M.elems uft)) (freeTVars (M.elems vm))
  return $ Classes.Forall vs t

vnums :: [Int]
vnums = [0..]

-- | `fresh` generates a new `TVar` using the next tick number and increment the counter.
fresh :: InferFn Classes.Type
fresh = do
  n <- gets count
  let t = Classes.TVar $ TVarIdx (vnums !! n)
  modify $ \st -> st { count = n+1 }
  return t

-- | `lookupVar` @v@ looks up the type scheme of a variable in the current `varMap` given its variable ID @v@.
lookupVar :: I.VarId -> InferFn (Maybe Classes.Scheme)
lookupVar v = M.lookup v <$> gets varMap

-- | `insertVar` @v t@ Insert a variable ID and its type scheme into current `varMap`.
insertVar :: I.VarId -> Classes.Scheme -> InferFn ()
insertVar v t = modify $ \st -> st { varMap = M.insert v t $ varMap st }

-- | `insertBinder` is a wrapper of `insertVar` for `Binder` type.
insertBinder :: Binder -> Classes.Scheme -> InferFn ()
insertBinder Nothing _ = return ()
insertBinder (Just vid) t = insertVar vid t

-- | `insertEquation` @e@ inserts @e@ into the current `equations` list.
insertEquation :: (Classes.Type, Classes.Type) -> InferFn ()
insertEquation e = modify $ \st -> st { equations = e : equations st }

-- | `insertUnion` @t1 t2@ inserts @t1 : t2@ into the current `unionFindTree`.
insertUnion :: Classes.Type -> Classes.Type -> InferFn ()
insertUnion t1 t2 = modify $ \st -> st { unionFindTree = M.insert t1 t2 $ unionFindTree st}

-- | Helper function to support local modificaton of `varMap`.
withNewScope :: InferFn a -> InferFn a
withNewScope inf = do
  vm <- gets varMap
  x <- inf
  modify $ \st -> st { varMap = vm }
  return x

-- | Helper function to support local modificaton of `unionFindTree`.
withNewTypeScope :: InferFn a -> InferFn a
withNewTypeScope inf = do
  s <- gets unionFindTree
  x <- inf
  modify $ \st -> st { unionFindTree = s }
  return x

{- | Stage 1: Assign symbolic typenames

`initTypeVars` @e@ walks into the expression @e@ revursively, assign a fresh
`TVar` to each unknown type, and build type equations that will be solved later.
-}
initTypeVars :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
initTypeVars e@(I.Var v _) = do
  record <- lookupVar v
  case record of
    Nothing -> throwError $ Compiler.TypeError $ fromString $ "Unbound variable: " ++ show e
    Just s -> do
      t' <- instantiate s
      return $ I.Var v t'
initTypeVars (I.Lambda v b _) = do
  tin <- fresh
  b' <- withNewScope do
    insertBinder v (Classes.Forall [] tin)
    initTypeVars b
  return $ I.Lambda v b' (Classes.TBuiltin $ Arrow tin (extract b'))
initTypeVars (I.Let vs b _) = do
  (vs', b') <- withNewScope $ localinitTypeVars vs b
  return $ I.Let vs' b' (extract b')
  where
    localinitTypeVars bs body = do
      bs' <- mapM fn bs
      body' <- initTypeVars body
      return (bs', body')
      where
        fn (binder, e) = do
          e' <- initTypeVars e
          unifyAll
          e'' <- getType e'
          s <- generalize $ extract e''
          insertBinder binder s
          return (binder, e')
initTypeVars (I.Lit I.LitEvent _) = return $ I.Lit I.LitEvent unit
initTypeVars (I.Lit i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
initTypeVars (I.App a b _) = do
  t <- fresh
  a' <- initTypeVars a
  b' <- initTypeVars b
  insertEquation (Classes.TBuiltin (Arrow (extract b') t), extract a')
  return $ I.App a' b' t
-- | Infer the type of a Primitive expression.
initTypeVars (I.Prim I.New [e] _) = do
  t <- fresh
  e' <- initTypeVars e
  insertEquation (t, Classes.TBuiltin (Ref (extract e')))
  return $ I.Prim I.New [e'] t
initTypeVars (I.Prim I.Deref [e] _) = do
  t <- fresh
  e' <- initTypeVars e
  insertEquation (Classes.TBuiltin (Ref t), extract e')
  return $ I.Prim I.Deref [e'] t
initTypeVars (I.Prim I.Dup [e] _) = do
  t <- fresh
  e' <- initTypeVars e
  insertEquation (t, extract e')
  return $ I.Prim I.Dup [e'] t
initTypeVars (I.Prim I.Assign [lhs, rhs] _) = do
  lhs' <- initTypeVars lhs
  rhs' <- initTypeVars rhs
  insertEquation (extract lhs', Classes.TBuiltin (Ref (extract rhs')))
  return $ I.Prim I.Assign [lhs', rhs'] unit
initTypeVars (I.Prim (I.PrimOp I.PrimSub) [e1, e2]  _) = do
  e1' <- initTypeVars e1
  e2' <- initTypeVars e2
  insertEquation (extract e1', int 32)
  insertEquation (extract e2', int 32)
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] $ int 32
initTypeVars (I.Prim (I.PrimOp I.PrimAdd) [e1, e2]  _) = do
  e1' <- initTypeVars e1
  e2' <- initTypeVars e2
  insertEquation (extract e1', int 32)
  insertEquation (extract e2', int 32)
  return $ I.Prim (I.PrimOp I.PrimAdd) [e1', e2'] $ int 32
initTypeVars (I.Prim I.After [del, lhs, rhs] _) = do
  del' <- initTypeVars del
  rhs' <- initTypeVars rhs
  lhs' <- initTypeVars lhs
  insertEquation (extract del', int 32)
  insertEquation (extract lhs', Classes.TBuiltin (Ref (extract rhs')))
  return $ I.Prim I.After [del', lhs', rhs'] unit
initTypeVars (I.Prim I.Break [] _) = return $ I.Prim I.Break [] void
initTypeVars (I.Prim I.Return [] _) = return $ I.Prim I.Return [] void
initTypeVars (I.Prim I.Loop es _) = do
    es' <- mapM initTypeVars es
    return $ I.Prim I.Loop es' unit
initTypeVars (I.Prim I.Wait es _) = do
    es' <- mapM initTypeVars es
    return $ I.Prim I.Wait es' unit
initTypeVars (I.Prim I.Par es _) = do
    es' <- mapM initTypeVars es
    return $ I.Prim I.Par es' $ Classes.TBuiltin (Tuple (map extract es'))
initTypeVars e@I.Prim {} = throwError $ Compiler.TypeError $ fromString $ "Unsupported Prim expression: " ++ show e
initTypeVars e = throwError $ Compiler.TypeError $ fromString $ "Unable to type unknown expression: " ++ show e

{- | Stage 2: Solve type equations using unification.

`unifyAll` @eqs@ solve the list of equations @eqs@ one by one and puts solutions
as new enties in the `unionFindTree`.
-}
unifyAll :: InferFn ()
unifyAll = do
  eq <- gets equations
  case eq of
    [] -> return ()
    ((l,r):eqs) -> do
      unify l r
      modify $ \st -> st { equations = eqs }
      unifyAll

-- |`unify` @t1 t2@ solves the type equation t1 ~ t2 and put the solution into `unionFindTree`.
unify :: Classes.Type -> Classes.Type -> InferFn ()
unify t1 t2
  | t1 == t2 = return ()
unify tv1@(Classes.TVar _) tv2@(Classes.TVar _) = do
  r1 <- findRoot tv1
  r2 <- findRoot tv2
  if r1 == tv1 && r2 == tv2 then insertUnion r1 r2 else unify r1 r2
unify t tv@(Classes.TVar _) = unify tv t
unify tv@(Classes.TVar _) t = do
  r1 <- findRoot tv
  if r1 == tv then insertUnion r1 t else unify r1 t
unify (Classes.TBuiltin (Ref t1)) (Classes.TBuiltin (Ref t2)) = unify t1 t2
unify (Classes.TBuiltin (Arrow t1 t2)) (Classes.TBuiltin (Arrow t3 t4)) = unify t1 t3 >> unify t2 t4
unify _ _ = throwError $ Compiler.TypeError $ fromString "Single unification error"

{- | Stage 3: Find the type of the expression based on `unionFindTree`.

`getType` @e@ walks into the expression @e@ and solves its type @t@ recursively.
-}
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
    return $ I.Prim I.Par es' $ Classes.TBuiltin (Tuple (map extract es'))
getType e = throwError $ Compiler.TypeError $ fromString $ "Unable to get the type of unknown expression: " ++ show e

-- | `solveType` @t@ solves the type @t@ by replacing any embeded `TVar` @tvar@
--   with its real type, which is the root of @tvar@ in the `unionFindTree`.
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
solveType t = throwError $ Compiler.TypeError $ fromString $ "Solve Type error" ++ show t

-- | `findRoot` @t@ finds the root of @t@ inside `unionFindTree`.
findRoot :: Classes.Type -> InferFn Classes.Type
findRoot t = do
  sb <- gets unionFindTree
  -- We find the root of @t@ if @t@ exists in the tree as a key and its value is different from itself
  case M.lookup t sb of
    Just t' | t' /= t -> findRoot t'
    _ -> return t
