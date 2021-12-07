{- | Infer types for optionally annotated program.

For now, this module implements a very crude form of pure, syntax-directed type
inference where type annotations can be locally and immediately inferred,
without the help of a type environment/context.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module IR.TypeInference where

import qualified Data.Map                      as M

import qualified Common.Compiler               as Compiler

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes

import           Common.Identifiers                       ( Binder )
import           Control.Comonad                          ( Comonad(..) )
import           Control.Monad.Except                     ( MonadError(..) )
import           Control.Monad.State.Lazy                 ( MonadState
                                                          , StateT(..)
                                                          , evalStateT
                                                          , get
                                                          , gets
                                                          , modify
                                                          )
import           Data.Maybe                               ( fromJust )
import           IR.Types.TypeSystem                      ( Builtin(..)
                                                          , dearrow
                                                          , deref
                                                          , int
                                                          , ref
                                                          , tuple
                                                          , unit
                                                          , void
                                                          )

-- | Typing Environment
newtype TypeCtx = TypeCtx { varMap :: M.Map I.VarId Classes.Type }

-- Inference Monad
newtype InferFn a = InferFn (StateT TypeCtx Compiler.Pass a)
  deriving Functor                      via (StateT TypeCtx Compiler.Pass)
  deriving Applicative                  via (StateT TypeCtx Compiler.Pass)
  deriving Monad                        via (StateT TypeCtx Compiler.Pass)
  deriving MonadFail                    via (StateT TypeCtx Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT TypeCtx Compiler.Pass)
  deriving (MonadState TypeCtx)         via (StateT TypeCtx Compiler.Pass)

-- | Run a InferFn computation.
runInferFn :: InferFn a -> Compiler.Pass a
runInferFn (InferFn m) = evalStateT m TypeCtx { varMap = M.empty }

-- | Insert a variable ID and its type into current typing context.
insertVar :: I.VarId -> Classes.Type -> InferFn ()
insertVar v t = modify $ \st -> st { varMap = M.insert v t $ varMap st }

-- | Replace the current typing context with a new one.
replaceCtx :: TypeCtx -> InferFn ()
replaceCtx ctx = modify $ const ctx

-- | Look up the type of a variable in the current typing context given its variable ID.
lookupVar :: I.VarId -> InferFn (Maybe Classes.Type)
lookupVar v = M.lookup v <$> gets varMap

-- | Infer all the program defs of the given porgram.
inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = runInferFn $ do
  defs' <- inferTop $ I.programDefs p
  return $ I.Program { I.programDefs    = defs'
                     , I.programEntry   = I.programEntry p
                     , I.programTypes   = [] -- TODO: something with I.typeDefs p
                     , I.programClasses = []
                     , I.programInsts   = []
                     }

{- | Top level inference.

This will infer the (VarId, Expr) pair sequentially and add the infered types
into the type context so that these info can be used when infering the following
expressions.
-}
inferTop
  :: [(I.VarId, I.Expr Ann.Type)] -> InferFn [(I.VarId, I.Expr Classes.Type)]
inferTop []            = return []
inferTop ((v, e) : xs) = do
  e' <- inferExpr e
  insertVar v (extract e')
  xs' <- inferTop xs
  return $ (v, e') : xs'

{- | Infer a single expression @e@.

This function will also infer any nested expressions inside @e@ and all
expressions in the return value will have Classes.Type as its type. Error will
be thrown if the type annotation is in inconsistent with the true true or the
expression is known.
-}
inferExpr :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
inferExpr (I.Var v t) = do
  record <- lookupVar v
  case record of
    Nothing -> do
      t' <- anns2Class t
      insertVar v t'
      return $ I.Var v t'
    Just t' -> return $ I.Var v t'
inferExpr e@(I.Lambda v b t) = do
  b' <- withNewScope $ withVty e v t >> inferExpr b
  t' <- anns2Class t
  return $ I.Lambda v b' t'
inferExpr (I.Let vs b _) = do
  (vs', b') <- withNewScope $ localInferExpr vs b
  return $ I.Let vs' b' (extract b')
 where
  localInferExpr bs body = do
    bs'   <- mapM fn bs
    body' <- inferExpr body
    return (bs', body')
   where
    fn (binding, e) = do
      e' <- inferExpr e
      case binding of
        Just vid -> insertVar vid (extract e')
        Nothing  -> return ()
      return (binding, e')
inferExpr e@I.Prim{}                      = inferPrim e
inferExpr (  I.Lit I.LitEvent          _) = return $ I.Lit I.LitEvent unit
inferExpr (  I.Lit i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
inferExpr e@(I.App a b _                ) = do
  a' <- inferExpr a
  b' <- inferExpr b
  case dearrow $ extract a' of
    Just (t1, t2) -> if t1 == extract a'
      then
        throwError
        $  Compiler.TypeError
        $  "App expression has inconsistent type: "
        ++ show e
      else return $ I.App a' b' t2
    Nothing ->
      throwError
        $  Compiler.TypeError
        $  "Unable to type App expression: "
        ++ show e
inferExpr e =
  throwError
    $  Compiler.TypeError
    $  "Unable to type unknown expression: "
    ++ show e

-- | Infer the type of a Primitive expression.
inferPrim :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
inferPrim (I.Prim I.New [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.New [e'] $ ref $ extract e'
inferPrim (I.Prim I.Deref [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.Deref [e'] $ fromJust $ deref $ extract e'
inferPrim (I.Prim I.Dup [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.Dup [e'] $ extract e'
inferPrim e@(I.Prim I.Assign [lhs, rhs] _) = do
  lhs' <- inferExpr lhs
  rhs' <- inferExpr rhs
  let rrty = ref $ extract rhs'
  if extract lhs' == rrty
    then do
      case lhs' of
        I.Var v _ -> insertVar v rrty
        _         -> return ()
      return $ I.Prim I.Assign [rrty <$ lhs', rhs'] unit
    else
      throwError
      $  Compiler.TypeError
      $  "Assign expression has inconsistent type: "
      ++ show e
inferPrim e@(I.Prim I.After [del, lhs, rhs] _) = do
  del' <- inferExpr del
  rhs' <- inferExpr rhs
  lhs' <- inferExpr lhs
  let rrty = ref $ extract rhs'
  if extract del' == int 32 && extract lhs' == rrty
    then do
      case lhs' of
        I.Var v _ -> insertVar v rrty
        _         -> return ()
      return $ I.Prim I.After [(\_ -> int 32) <$> del', rrty <$ lhs', rhs'] unit
    else
      throwError
      $  Compiler.TypeError
      $  "After expression has inconsistent type: "
      ++ show e
inferPrim (I.Prim (I.PrimOp I.PrimSub) [e1, e2] _) = do
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] $ int 32
inferPrim (I.Prim (I.PrimOp I.PrimAdd) [e1, e2] _) = do
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  return $ I.Prim (I.PrimOp I.PrimAdd) [e1', e2'] $ int 32
inferPrim (I.Prim I.Break  [] _) = return $ I.Prim I.Break [] void
inferPrim (I.Prim I.Return [] _) = return $ I.Prim I.Return [] void
inferPrim (I.Prim I.Loop   es _) = do
  es' <- mapM inferExpr es
  return $ I.Prim I.Loop es' unit
inferPrim (I.Prim I.Wait es _) = do
  es' <- mapM inferExpr es
  return $ I.Prim I.Wait es' unit
inferPrim (I.Prim I.Par es _) = do
  es' <- mapM inferExpr es
  let ts = map extract es'
  return $ I.Prim I.Par es' $ tuple ts
inferPrim e =
  throwError $ Compiler.TypeError $ "Unable to type Prim expression: " ++ show e

-- | Helper function to support local modificaton of type context.
withNewScope :: InferFn a -> InferFn a
withNewScope inf = do
  ctx <- get
  x   <- inf
  modify $ const ctx
  return x

-- | Helper function to do type inference with the knowledge of the correct type for a binding.
withVty :: I.Expr Ann.Type -> Binder -> Ann.Type -> InferFn ()
withVty _ Nothing  _ = return ()
withVty e (Just v) t = case dearrow t of
  Just (vty, _) -> do
    vty' <- anns2Class vty
    insertVar v vty'
  Nothing ->
    throwError
      $  Compiler.TypeError
      $  "Lambda wasn't annotated (or it wasn't an arrow type): "
      ++ show e

-- | Transfrom an Ann.Type to Classes.Type.
anns2Class :: Ann.Type -> InferFn Classes.Type
anns2Class (Ann.Type []) =
  throwError $ Compiler.TypeError "Cannot change empty Ann type to Classes type"
anns2Class (Ann.Type ts) = ann2Class $ head ts

{- Transfrom an Ann.TypeAnnote to Classes.Type.

Since we are doing this in a monadic way, and Builtin is also a Functor, we have
to do explicit pattern matching on the `Ann.TBuiltin bty` case to transform
`Builtin Ann.Type` to `Builtin Classes.Type`.
-}
ann2Class :: Ann.TypeAnnote -> InferFn Classes.Type
ann2Class (Ann.TBuiltin bty) = case bty of
  Unit       -> return $ Classes.TBuiltin Unit
  Void       -> return $ Classes.TBuiltin Void
  Integral s -> return $ Classes.TBuiltin $ Integral s
  Arrow l r  -> do
    l' <- anns2Class l
    r' <- anns2Class r
    return $ Classes.TBuiltin $ Arrow l' r'
  Ref t -> do
    t' <- anns2Class t
    return $ Classes.TBuiltin $ Ref t'
  Tuple tys -> do
    tys' <- mapM anns2Class tys
    return $ Classes.TBuiltin $ Tuple tys'
ann2Class (Ann.TCon tid tys) = do
  t <- mapM anns2Class tys
  return $ Classes.TCon tid t
ann2Class (Ann.TVar tidx) = return $ Classes.TVar tidx
