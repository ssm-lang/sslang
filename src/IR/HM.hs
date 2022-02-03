{- | Infer types for IR of optionally annotated program using Hindley Milner
type inference algorithm with union find tree data structure.

There are three main stages of the algorithm
1. Assign symbolic typenames
   During this stage, we walk through each expression recursively, initialize
   its type by either assigning a fresh type variable or looking it up in the
   type context at that point, and generate a list of type equations to record
   the relationships between types. We also collapse the list of type
   annotations into the most specific one and put that information into a type
   equation as well to bring it into the later inference. Note that we will be
   modifying the expression monad simultaneously to record its new type info.
2. Solve type equations using unification.
   During this stage, we solve the type equations generated in the last stage
   and build a union find tree. This stage should be merged with stage 1 in the
   future.
3. Find the type of the expression.
   During this stage, we walk through each expression recursively and assign it
   with its inferred type by loop up the root of its temporary type in the union
  find tree.

The three stages are encaptured by the functions 'initTypeVars', 'unifyAll',
and 'getType'.

TODO:
- Define a partial order for the generality of types. The order will be used to
  (1) collapse type annotations into the most specific one; (2) check that
  type annotation is equal to or more specific than the expression's inferred
  type.
- Handle recursive function correctly.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module IR.HM where

import           Data.Bifunctor                 ( second )
import qualified Data.Map                      as M
import qualified Data.Set                      as S

import qualified Common.Compiler               as Compiler

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes

import           Common.Identifiers             ( Binder
                                                , Identifier(..)
                                                , TVarId(..)
                                                , fromString
                                                )
import           Control.Comonad                ( Comonad(..) )
import           Control.Monad.Except           ( MonadError(..)
                                                , zipWithM
                                                , zipWithM_
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                , replicateM
                                                )
import           IR.Types.Classes               ( Scheme(Forall) )
import           IR.Types.TypeSystem            ( Builtin(..)
                                                , TypeDef(TypeDef)
                                                , TypeVariant
                                                  ( VariantNamed
                                                  , VariantUnnamed
                                                  )
                                                , int
                                                , unit
                                                , variants
                                                , void
                                                )

-- | Inference State.
data InferState = InferState
  { varMap        :: M.Map I.VarId Classes.Scheme
  , equations     :: [(Classes.Type, Classes.Type)]
  , unionFindTree :: M.Map Classes.Type Classes.Type
  , count         :: Int
  , dConType      :: M.Map I.DConId Classes.Type
  , dConArgType   :: M.Map I.DConId [Classes.Type]
  }

-- | Inference Monad.
newtype InferFn a = InferFn (StateT InferState Compiler.Pass a)
  deriving Functor                      via (StateT InferState Compiler.Pass)
  deriving Applicative                  via (StateT InferState Compiler.Pass)
  deriving Monad                        via (StateT InferState Compiler.Pass)
  deriving MonadFail                    via (StateT InferState Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT InferState Compiler.Pass)
  deriving (MonadState InferState)      via (StateT InferState Compiler.Pass)

-- | Run a InferFn computation.
runInferFn :: InferFn a -> Compiler.Pass a
runInferFn (InferFn m) = evalStateT
  m
  InferState { unionFindTree = M.empty
             , varMap        = M.empty
             , count         = 0
             , equations     = []
             , dConType      = M.empty
             , dConArgType   = M.empty
             }

-- | 'inferProgram' @p@ infers the type of all the programDefs of the given program @p@.
inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = runInferFn $ do
  typeDefs' <- recordADTs $ I.typeDefs p
  defs'     <- inferProgramDefs $ I.programDefs p
  return $ I.Program { I.programDefs  = defs'
                     , I.programEntry = I.programEntry p
                     , I.typeDefs     = typeDefs'
                     }


{- | 'recordADTs' saves information about ADTs in the Inference State for future use

saves type of DCon as a (DCon,TCon) key-value pair
transforms Ann.Type to Classes.Type
saves types of DCon args (the ADT fields) as (DCon, [type]) key-value pairs
-}
recordADTs
  :: [(I.TConId, TypeDef Ann.Type)]
  -> InferFn [(I.TConId, TypeDef Classes.Type)]
recordADTs defs = do
  mapM_ recordADT defs
  let defs' = second (fmap collapseAnnT) <$> defs
  mapM_ insertArg $ concat $ variants . snd <$> defs'
  return defs'
 where
  recordADT :: (I.TConId, TypeDef Ann.Type) -> InferFn ()
  recordADT (tcon, TypeDef { variants = vars }) = mapM_
    (insertType tcon . fst)
    vars   where
    insertType :: I.TConId -> I.DConId -> InferFn ()
    insertType t d = modify
      $ \st -> st { dConType = M.insert d (Classes.TCon t []) $ dConType st }
  insertArg :: (I.DConId, TypeVariant Classes.Type) -> InferFn ()
  insertArg (dcon, VariantNamed vars) = modify $ \st ->
    st { dConArgType = M.insert dcon (snd <$> vars) $ dConArgType st }
  insertArg (dcon, VariantUnnamed ts) =
    modify $ \st -> st { dConArgType = M.insert dcon ts $ dConArgType st }

-- | 'inferProgramDefs' @ds@ infers the type of programDefs @ds@ recursively and binds each varibale to its type.
inferProgramDefs
  :: [(I.VarId, I.Expr Ann.Type)] -> InferFn [(I.VarId, I.Expr Classes.Type)]
inferProgramDefs []            = return []
inferProgramDefs ((v, e) : xs) = do
  e' <- initTypeVars e
  unifyAll
  e'' <- getType e'
  s   <- generalize $ extract e''
  insertVar v s
  xs' <- inferProgramDefs xs
  return $ (v, e'') : xs'

-- | Class of things that may contain free type variables.
class HasFreeTVars a where
  freeTVars :: a -> S.Set TVarId

instance HasFreeTVars TVarId where
  freeTVars = S.singleton

instance HasFreeTVars Classes.Scheme where
  {- | Determines the set of free type variables inside a scheme @Forall ns t@.

  The free variables are those in the underlying type @t@ minus the
  variables quantified over by the scheme in @ns@.
  -}
  freeTVars (Classes.Forall ns t) = S.difference (freeTVars t) (S.fromList ns)

instance HasFreeTVars Classes.Type where
  freeTVars (Classes.TBuiltin b) = freeTVars b
  freeTVars (Classes.TCon _ ts ) = freeTVars ts
  freeTVars (Classes.TVar n    ) = S.singleton n

instance HasFreeTVars t => HasFreeTVars (Builtin t) where
  freeTVars Unit           = S.empty
  freeTVars Void           = S.empty
  freeTVars (Ref t       ) = freeTVars t
  freeTVars (Arrow l r   ) = S.union (freeTVars l) (freeTVars r)
  freeTVars (Tuple    tys) = freeTVars tys
  freeTVars (Integral _  ) = S.empty

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
    modify $ \st ->
      st { unionFindTree = M.fromList (zip (map Classes.TVar ns) ns') }
    solveType t

{- | Generalizes the type @t@ into a type scheme given the typing context @ctx@.

Collect all the free type variables in the type @t@ and all the free type
variables in the typing context @ctx@,
away any type variables that appear free in the type environment, and whatever
is leftover can be generalized into a type scheme.
-}
generalize :: Classes.Type -> InferFn Classes.Scheme
generalize t = do
  vm  <- gets varMap
  uft <- gets unionFindTree
  let vs = S.toList $ S.difference (freeTVars t) $ S.union
        (freeTVars (M.elems uft))
        (freeTVars (M.elems vm))
  return $ Classes.Forall vs t

-- | 'letters' represents list of identifiers to be used to construct free type
-- variables assigned during type inference. The leading '_' distinguish them
-- from user-annotated type variables.
letters :: [String]
letters = map ('_' :) $ [1 ..] >>= flip replicateM ['a' .. 'z']

-- | @dummyAnnT@ represents empty type annotation list.
dummyAnnT :: Classes.Type
dummyAnnT = Classes.TVar (TVarId $ Identifier "_")

-- | Generates a new 'TVar' using the next tick number and increment the counter.
fresh :: InferFn Classes.Type
fresh = do
  n <- gets count
  let t = Classes.TVar $ TVarId $ Identifier (letters !! n)
  modify $ \st -> st { count = n + 1 }
  return t

-- | 'lookupVar' @v@ looks up the type scheme of a variable in the current 'varMap' given its variable ID @v@.
lookupVar :: I.VarId -> InferFn (Maybe Classes.Scheme)
lookupVar v = M.lookup v <$> gets varMap

-- | Look up the type of a data constructor in the inference state.
lookupDCon :: I.DConId -> InferFn (Maybe Classes.Type)
lookupDCon d = M.lookup d <$> gets dConType

-- | Look up the types of an ADT's fields in the inference state.
lookupFieldTypes :: I.DConId -> InferFn (Maybe [Classes.Type])
lookupFieldTypes d = M.lookup d <$> gets dConArgType

-- | 'insertVar' @v t@ Insert a variable ID and its type scheme into current 'varMap'.
insertVar :: I.VarId -> Classes.Scheme -> InferFn ()
insertVar v t = modify $ \st -> st { varMap = M.insert v t $ varMap st }

-- | 'insertBinder' is a wrapper of 'insertVar' for 'Binder' type.
insertBinder :: Binder -> Classes.Scheme -> InferFn ()
insertBinder Nothing    _ = return ()
insertBinder (Just vid) t = insertVar vid t

-- | 'insertEquation' @e@ inserts @e@ into the current 'equations' list.
insertEquation :: (Classes.Type, Classes.Type) -> InferFn ()
insertEquation e = modify $ \st -> st { equations = e : equations st }

-- | 'insertUnion' @t1 t2@ inserts @t1 : t2@ into the current 'unionFindTree'.
insertUnion :: Classes.Type -> Classes.Type -> InferFn ()
insertUnion t1 t2 =
  modify $ \st -> st { unionFindTree = M.insert t1 t2 $ unionFindTree st }

-- | Helper function to support local modificaton of 'varMap'.
withNewScope :: InferFn a -> InferFn a
withNewScope inf = do
  vm <- gets varMap
  x  <- inf
  modify $ \st -> st { varMap = vm }
  return x

-- | Helper function to support local modificaton of 'unionFindTree'.
withNewTypeScope :: InferFn a -> InferFn a
withNewTypeScope inf = do
  s <- gets unionFindTree
  x <- inf
  modify $ \st -> st { unionFindTree = s }
  return x

-- | Helper function to collapse a list of type annotation to a single one with
-- 'Classes.type' type. If the list is empty, use
-- @Classes.TVar (TVarId $ Identifier "_"))@ to denote a dummy annotation.
-- TODO: Currently just take the first type annotation of the list as the
-- result. Need to implement the 'collapse' step to find and return the most
-- specific type annotation from the list. Need yo add support for annotating
-- type variables as well.
collapseAnnT :: Ann.Type -> Classes.Type
collapseAnnT (Ann.Type []  ) = dummyAnnT
collapseAnnT (Ann.Type anns) = case head anns of
  Ann.TBuiltin tb   -> Classes.TBuiltin $ fmap collapseAnnT tb
  Ann.TVar     tvar -> Classes.TVar tvar
  Ann.TCon tcon tys -> Classes.TCon tcon $ fmap collapseAnnT tys

{- | Stage 1: Assign symbolic typenames

'initTypeVars' @e@ walks into the expression @e@ recursively, assign a fresh
'TVar' to each unknown type, and build type equations that will be solved later.
-}
initTypeVars :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
initTypeVars e@(I.Var v annT) = do
  let annT' = collapseAnnT annT
  record <- lookupVar v
  case record of
    Nothing ->
      throwError
        $  Compiler.TypeError
        $  fromString
        $  "Unbound variable: "
        ++ show e
    Just s -> do
      t'  <- instantiate s
      t'' <- typeCheck annT' t'
      return $ I.Var v t''
initTypeVars (I.Lambda v b annT) = do
  let annT' = collapseAnnT annT
  tin <- fresh
  b'  <- withNewScope do
    insertBinder v (Classes.Forall [] tin)
    initTypeVars b
  t <- typeCheck annT' (Classes.TBuiltin $ Arrow tin (extract b'))
  return $ I.Lambda v b' t
initTypeVars (I.Let vs b annT) = do
  let annT' = collapseAnnT annT
  (vs', b') <- withNewScope $ localinitTypeVars vs b
  t         <- typeCheck annT' (extract b')
  return $ I.Let vs' b' t
 where
  localinitTypeVars bs body = do
    bs'   <- mapM fn bs
    body' <- initTypeVars body
    return (bs', body')
   where
    fn (binder, e) = do
      e' <- initTypeVars e
      unifyAll
      e'' <- getType e'
      s   <- generalize $ extract e''
      insertBinder binder s
      return (binder, e'')
initTypeVars e@(I.Data d _) = do
  tcon <- lookupDCon d
  case tcon of
    Nothing ->
      throwError
        $  Compiler.TypeError
        $  fromString
        $  "Unable to type ADT expression: "
        ++ show e
    Just t' -> return $ I.Data d t'
initTypeVars (I.App a@(I.Data _ _) b annT) = do
  let annT' = collapseAnnT annT
  a'   <- initTypeVars a
  b'   <- initTypeVars b
  tout <- fresh
  t    <- typeCheck annT' tout
  return $ I.App a' b' t
initTypeVars (I.App a b annT) = do
  let annT' = collapseAnnT annT
  a'   <- initTypeVars a
  b'   <- initTypeVars b
  tout <- fresh
  insertEquation (Classes.TBuiltin (Arrow (extract b') tout), extract a')
  t <- typeCheck annT' tout
  return $ I.App a' b' t
initTypeVars (I.Lit I.LitEvent annT) = do
  let annT' = collapseAnnT annT
  t <- typeCheck annT' unit
  return $ I.Lit I.LitEvent t
initTypeVars (I.Lit i@(I.LitIntegral _) annT) = do
  let annT' = collapseAnnT annT
  t <- typeCheck annT' (int 32)
  return $ I.Lit i t
-- | Infer the type of a Primitive expression.
initTypeVars (I.Prim I.New [e] annT) = do
  let annT' = collapseAnnT annT
  e' <- initTypeVars e
  t  <- typeCheck annT' (Classes.TBuiltin (Ref (extract e')))
  return $ I.Prim I.New [e'] t
initTypeVars (I.Prim I.Deref [e] annT) = do
  let annT' = collapseAnnT annT
  e' <- initTypeVars e
  t  <- fresh
  insertEquation (Classes.TBuiltin (Ref t), extract e')
  t' <- typeCheck annT' t
  return $ I.Prim I.Deref [e'] t'
initTypeVars (I.Prim I.Dup [e] annT) = do
  let annT' = collapseAnnT annT
  e' <- initTypeVars e
  t  <- typeCheck annT' (extract e')
  return $ I.Prim I.Dup [e'] t
initTypeVars (I.Prim I.Assign [lhs, rhs] annT) = do
  let annT' = collapseAnnT annT
  t    <- typeCheck annT' unit
  lhs' <- initTypeVars lhs
  rhs' <- initTypeVars rhs
  return $ I.Prim I.Assign [lhs', rhs'] t
initTypeVars (I.Prim (I.PrimOp I.PrimSub) [e1, e2] annT) = do
  let annT' = collapseAnnT annT
  t   <- typeCheck annT' (int 32)
  e1' <- initTypeVars e1
  e2' <- initTypeVars e2
  insertEquation (extract e1', int 32)
  insertEquation (extract e2', int 32)
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] t
initTypeVars (I.Prim (I.PrimOp I.PrimAdd) [e1, e2] annT) = do
  let annT' = collapseAnnT annT
  t   <- typeCheck annT' (int 32)
  e1' <- initTypeVars e1
  e2' <- initTypeVars e2
  insertEquation (extract e1', int 32)
  insertEquation (extract e2', int 32)
  return $ I.Prim (I.PrimOp I.PrimAdd) [e1', e2'] t
initTypeVars (I.Prim I.After [del, lhs, rhs] annT) = do
  let annT' = collapseAnnT annT
  t    <- typeCheck annT' unit
  del' <- initTypeVars del
  rhs' <- initTypeVars rhs
  lhs' <- initTypeVars lhs
  insertEquation (extract del', int 32)
  insertEquation (extract lhs', Classes.TBuiltin (Ref (extract rhs')))
  return $ I.Prim I.After [del', lhs', rhs'] t
initTypeVars (I.Prim I.Break [] annT) = do
  let annT' = collapseAnnT annT
  t <- typeCheck annT' void
  return $ I.Prim I.Break [] t
initTypeVars (I.Prim I.Return [] annT) = do
  let annT' = collapseAnnT annT
  t <- typeCheck annT' void
  return $ I.Prim I.Return [] t
initTypeVars (I.Prim I.Loop es annT) = do
  let annT' = collapseAnnT annT
  t   <- typeCheck annT' unit
  es' <- mapM initTypeVars es
  return $ I.Prim I.Loop es' t
initTypeVars (I.Prim I.Wait es annT) = do
  let annT' = collapseAnnT annT
  t   <- typeCheck annT' unit
  es' <- mapM initTypeVars es
  return $ I.Prim I.Wait es' t
initTypeVars (I.Prim I.Par es annT) = do
  let annT' = collapseAnnT annT
  es' <- mapM initTypeVars es
  t   <- typeCheck annT' (Classes.TBuiltin (Tuple (map extract es')))
  return $ I.Prim I.Par es' t
initTypeVars (I.Match cond arms annT) = do
    let annT' = collapseAnnT annT
    cond' <- initTypeVars cond
    arms' <- mapM checkArm arms
    let arms'' = zip (fst <$> arms) arms'
    return (I.Match cond' arms'' annT')
  checkArm :: (I.Alt, I.Expr Ann.Type) -> InferFn (I.Expr Classes.Type)
  checkArm (I.AltData dcon args, rhs) = withNewScope do
    Just ts <- lookupFieldTypes dcon
    mapM_ (\(a, b) -> insertBinder a (Forall [] b)) (zip args ts)
    initTypeVars rhs
  checkArm (_, rhs) = initTypeVars rhs
initTypeVars e@I.Prim{} =
  throwError
    $  Compiler.TypeError
    $  fromString
    $  "Unsupported Prim expression: "
    ++ show e
initTypeVars e =
  throwError
    $  Compiler.TypeError
    $  fromString
    $  "Unable to type unknown expression: "
    ++ show e

-- | @typeCheck annT expectedT@ checks whether the type annotation @annT@ is
-- compatible with the expected type @expectedT@. Insert new type equations when
-- the expected type is a type variable.
-- TODO: currently only consider equality as compatibility. Need to support the
-- case when type annotation is more specific than the expected type.
typeCheck :: Classes.Type -> Classes.Type -> InferFn Classes.Type
typeCheck annT expectedT | annT == dummyAnnT || annT == expectedT =
  return expectedT
typeCheck (Classes.TBuiltin (Ref t1)) (Classes.TBuiltin (Ref t2)) = do
  t <- typeCheck t1 t2
  return $ Classes.TBuiltin (Ref t)
typeCheck (Classes.TBuiltin (Arrow t1 t2)) (Classes.TBuiltin (Arrow t3 t4)) =
  do
    t1' <- typeCheck t1 t3
    t2' <- typeCheck t2 t4
    return $ Classes.TBuiltin (Arrow t1' t2')
typeCheck (Classes.TBuiltin (Tuple ts1)) (Classes.TBuiltin (Tuple ts2)) = do
  ts <- zipWithM typeCheck ts1 ts2
  return $ Classes.TBuiltin (Tuple ts)
typeCheck annT expectedT@(Classes.TVar _) =
  insertEquation (expectedT, annT) >> return expectedT
typeCheck annT expectedT =
  throwError
    $  Compiler.TypeError
    $  fromString
    $  "Incompatible type annotation: expected "
    ++ show expectedT
    ++ ", but got "
    ++ show annT

{- | Stage 2: Solve type equations using unification.

Solve the list of 'equations' one by one and puts solutions as new enties in
the 'unionFindTree'.
-}
unifyAll :: InferFn ()
unifyAll = do
  eq <- gets equations
  case eq of
    []             -> return ()
    ((l, r) : eqs) -> do
      unify l r
      modify $ \st -> st { equations = eqs }
      unifyAll

-- |'unify' @t1 t2@ solves the type equation t1 ~ t2 and put the solution into 'unionFindTree'.
unify :: Classes.Type -> Classes.Type -> InferFn ()
unify t1 t2 | t1 == t2                          = return ()
unify tv1@(Classes.TVar _) tv2@(Classes.TVar _) = do
  r1 <- findRoot tv1
  r2 <- findRoot tv2
  if r1 == tv1 && r2 == tv2 then insertUnion r1 r2 else unify r1 r2
unify t                   tv@(Classes.TVar _) = unify tv t
unify tv@(Classes.TVar _) t                   = do
  r1 <- findRoot tv
  if r1 == tv then insertUnion r1 t else unify r1 t
unify (Classes.TBuiltin (Ref t1)) (Classes.TBuiltin (Ref t2)) = unify t1 t2
unify (Classes.TBuiltin (Arrow t1 t2)) (Classes.TBuiltin (Arrow t3 t4)) =
  unify t1 t3 >> unify t2 t4
unify (Classes.TBuiltin (Tuple ts1)) (Classes.TBuiltin (Tuple ts2)) =
  zipWithM_ unify ts1 ts2
unify t1 t2 =
  throwError
    $  Compiler.TypeError
    $  fromString
    $  "Single unification error: "
    ++ show t1
    ++ ", "
    ++ show t2

{- | Stage 3: Find the type of the expression based on 'unionFindTree'.

'getType' @e@ walks into the expression @e@ and solves its type @t@ recursively.
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
  vs' <- mapM (fmapSnd getType) vs
  b'  <- getType b
  return $ I.Let vs' b' (extract b')
 where
  fmapSnd fn (v, e) = do
    e' <- fn e
    return (v, e')
getType (  I.Lit  I.LitEvent          _) = return $ I.Lit I.LitEvent unit
getType (  I.Lit  i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
getType e@(I.Data d                   _) = do
  tcon <- lookupDCon d
  case tcon of
    Nothing ->
      throwError
        $  Compiler.TypeError
        $  fromString
        $  "Unable to type ADT expression: "
        ++ show e
    Just t' -> return $ I.Data d t'
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
getType (I.Prim (I.PrimOp I.PrimSub) [e1, e2] _) = do
  e1' <- getType e1
  e2' <- getType e2
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] $ int 32
getType (I.Prim (I.PrimOp I.PrimAdd) [e1, e2] _) = do
  e1' <- getType e1
  e2' <- getType e2
  return $ I.Prim (I.PrimOp I.PrimAdd) [e1', e2'] $ int 32
getType (I.Prim I.After [del, lhs, rhs] _) = do
  del' <- getType del
  rhs' <- getType rhs
  lhs' <- getType lhs
  return $ I.Prim I.After [del', lhs', rhs'] unit
getType (I.Prim I.Break  [] _) = return $ I.Prim I.Break [] void
getType (I.Prim I.Return [] _) = return $ I.Prim I.Return [] void
getType (I.Prim I.Loop   es _) = do
  es' <- mapM getType es
  return $ I.Prim I.Loop es' unit
getType (I.Prim I.Wait es _) = do
  es' <- mapM getType es
  return $ I.Prim I.Wait es' unit
getType (I.Prim I.Par es _) = do
  es' <- mapM getType es
  return $ I.Prim I.Par es' $ Classes.TBuiltin (Tuple (map extract es'))
getType (I.Match cond arms _) = do
  cond'         <- getType cond
  arms'@(h : _) <- mapM (getType . snd) arms
  -- make sure all arms are of the same type
  if any (/= extract h) (extract <$> arms')
    then throwError $ Compiler.TypeError $ fromString
      "RHS of match arms are different types "
    else do
      let arms'' = zip (fst <$> arms) arms'
      let t      = extract h
      -- type of match is type of one of its arms
      return (I.Match cond' arms'' t)
getType e =
  throwError
    $  Compiler.TypeError
    $  fromString
    $  "Unable to get the type of unknown expression: "
    ++ show e

-- | 'solveType' @t@ solves the type @t@ by replacing any embeded 'TVar' @tvar@
--   with its real type, which is the root of @tvar@ in the 'unionFindTree'.
solveType :: Classes.Type -> InferFn Classes.Type
solveType t@( Classes.TBuiltin (Integral 32)) = return t
solveType t@( Classes.TBuiltin Unit         ) = return t
solveType t@( Classes.TBuiltin Void         ) = return t
solveType tv@(Classes.TVar     _            ) = findRoot tv
solveType (   Classes.TBuiltin (Ref t)      ) = do
  t' <- solveType t
  return $ Classes.TBuiltin $ Ref t'
solveType (Classes.TBuiltin (Tuple ts)) = do
  ts' <- mapM solveType ts
  return $ Classes.TBuiltin $ Tuple ts'
solveType (Classes.TBuiltin (Arrow t1 t2)) = do
  t1' <- solveType t1
  t2' <- solveType t2
  return $ Classes.TBuiltin $ Arrow t1' t2'
solveType t@(Classes.TCon (I.TConId _) _) = return t
solveType t =
  throwError $ Compiler.TypeError $ fromString $ "Solve Type error " ++ show t

-- | 'findRoot' @t@ finds the root of @t@ inside 'unionFindTree'.
findRoot :: Classes.Type -> InferFn Classes.Type
findRoot t@(Classes.TVar _) = do
  sb <- gets unionFindTree
  -- We find the root of @t@ if @t@ exists in the tree as a key and its value is different from itself
  case M.lookup t sb of
    Just t' | t' /= t -> findRoot t'
    _                 -> return t
findRoot t = solveType t
