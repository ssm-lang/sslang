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

import           IR.Types.TypeSystem            ( dearrow
                                                , deref
                                                , int
                                                , ref
                                                , unit
                                                , void
                                                )
import           Common.Identifiers             ( Binder )
import           Control.Comonad                ( Comonad(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , get
                                                , modify
                                                )
import           Data.Maybe                     ( fromJust ) 

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

runInferFn :: InferFn a -> Compiler.Pass a
runInferFn (InferFn m) = evalStateT m TypeCtx { varMap = M.empty }

insertVar :: I.VarId -> Classes.Type -> InferFn ()
insertVar v t = modify $ \st -> st { varMap = M.insert v t $ varMap st }

replaceCtx :: TypeCtx -> InferFn ()
replaceCtx ctx = modify $ const ctx

lookupVar :: I.VarId -> InferFn (Maybe Classes.Type)
lookupVar v = M.lookup v <$> gets varMap

inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = runInferFn $ do
  defs' <- inferTop $ I.programDefs p
  return $ I.Program { I.programDefs  = defs'
                     , I.programEntry = I.programEntry p
                     , I.typeDefs     = [] -- TODO: something with I.typeDefs p
                     }

inferTop :: [(I.VarId, I.Expr Ann.Type)] -> InferFn [(I.VarId, I.Expr Classes.Type)]
inferTop [] = return []
inferTop ((v, e):xs) = do
  e' <- inferExpr e
  insertVar v (extract e')
  xs' <- inferTop xs
  return $ (v, e'):xs'

inferExpr :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
inferExpr (I.Var v t) = do
  record <- lookupVar v
  case record of
    Nothing -> do
      let t' = anns2Class t
      insertVar v t'
      return $ I.Var v t'
    Just t' -> return $ I.Var v t'
inferExpr e@(I.Lambda v b t) = do
  b' <- withNewScope $ withVty e v t >> inferExpr b
  return $ I.Lambda v b' (anns2Class t)
inferExpr (I.Let vs b _) = do
  (vs', b') <- withNewScope $ localInferExpr vs b
  return $ I.Let vs' b' (extract b')
inferExpr e@I.Prim {} = inferPrim e
inferExpr (I.Lit I.LitEvent _) = return $ I.Lit I.LitEvent unit
inferExpr (I.Lit i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
inferExpr e@(I.App a b _) = do
  a' <- inferExpr a
  b' <- inferExpr b
  case dearrow $ extract a' of
    Just (t1, t2) ->
      if t1 == extract a'
        then throwError $ Compiler.TypeError $ "App expression has inconsistent type: " ++ show e
        else return $ I.App a' b' t2
    Nothing -> throwError $ Compiler.TypeError $ "Unable to type App expression: " ++ show e
inferExpr e = throwError $ Compiler.TypeError $ "Unable to type unknown expression: " ++ show e

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
        _ -> return ()
      return $ I.Prim I.Assign [rrty <$ lhs', rhs'] unit
    else throwError $ Compiler.TypeError $ "Assign expression has inconsistent type: " ++ show e
inferPrim e@(I.Prim I.After [del, lhs, rhs] _) = do
  del' <- inferExpr del
  rhs' <- inferExpr rhs
  lhs' <- inferExpr lhs
  let rrty = ref $ extract rhs'
  if extract del' == int 32 && extract lhs' == rrty
    then do
      case lhs' of
        I.Var v _ -> insertVar v rrty
        _ -> return ()
      return $ I.Prim I.After [(\_ -> int 32) <$> del', rrty <$ lhs', rhs'] unit
    else throwError $ Compiler.TypeError $ "After expression has inconsistent type: " ++ show e
inferPrim (I.Prim (I.PrimOp I.PrimSub) [e1, e2]  _) = do
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] $ int 32
inferPrim e@(I.Prim p [] _)
  | p `elem` [I.Break, I.Return] = return $ I.Prim p [] void
  | otherwise = throwError $ Compiler.TypeError $ "Unable to type Prim expression: " ++ show e
inferPrim e@(I.Prim p es _)
  | p `elem` [I.Loop, I.Wait, I.Par] = do
      es' <- mapM inferExpr es
      return $ I.Prim p es' unit
  | otherwise = throwError $ Compiler.TypeError $ "Unable to type Prim expression: " ++ show e
inferPrim e = throwError $ Compiler.TypeError $ "Unable to type Prim expression: " ++ show e

localInferExpr :: [(Binder, I.Expr Ann.Type)] -> I.Expr Ann.Type -> InferFn ([(Binder, I.Expr Classes.Type)], I.Expr Classes.Type)
localInferExpr [] body = do
  body' <- inferExpr body
  return ([], body')
localInferExpr ((b, e):vs) body = do
  e' <- inferExpr e
  case b of
    Just vid -> insertVar vid (extract e')
    Nothing -> return ()
  (vs', body') <- localInferExpr vs body
  return ((b, e'):vs', body')

withNewScope :: InferFn a -> InferFn a
withNewScope inf = do
  ctx <- get
  x <- inf
  modify $ const ctx
  return x

withVty :: I.Expr Ann.Type -> Binder -> Ann.Type -> InferFn ()
withVty _ Nothing _ = return ()
withVty e (Just v) t =
  case dearrow t of
    Just (vty, _) -> insertVar v (anns2Class vty)
    Nothing       -> throwError $ Compiler.TypeError $ "Lambda wasn't annotated (or it wasn't an arrow type): " ++ show e

anns2Class :: Ann.Type -> Classes.Type
anns2Class (Ann.Type ts) = ann2Class $ head ts

ann2Class :: Ann.TypeAnnote -> Classes.Type
ann2Class (Ann.TBuiltin bty) = Classes.TBuiltin $ fmap anns2Class bty
ann2Class (Ann.TCon tid tys) = Classes.TCon tid $ fmap anns2Class tys
ann2Class (Ann.TVar tidx   ) = Classes.TVar tidx
