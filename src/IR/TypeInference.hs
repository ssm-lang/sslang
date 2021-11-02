{- | Infer types for optionally annotated program.

For now, this module implements a very crude form of pure, syntax-directed type
inference where type annotations can be locally and immediately inferred,
without the help of a type environment/context.
-}
{-# LANGUAGE DerivingVia #-}
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
replaceCtx ctx = modify $ \_ -> ctx

lookupVar :: I.VarId -> InferFn (Maybe Classes.Type)
lookupVar v = M.lookup v <$> gets varMap

inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = runInferFn $ do
  defs' <- inferTop $ I.programDefs p
  return $ I.Program { I.programDefs  = defs'
                     , I.programEntry = I.programEntry p
                     , I.typeDefs     = [] -- TODO: something with I.typeDefs p
                     }

inferTop :: [(I.VarId, I.Expr Ann.Type)] -> InferFn ([(I.VarId, I.Expr Classes.Type)])
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
inferExpr (I.Lambda v b t) = do
  ctx <- get
  b' <- inferExpr b
  replaceCtx ctx
  return $ I.Lambda v b' (anns2Class t)
inferExpr (I.Let vs b _) = do
  ctx <- get
  vs' <- localInferExpr vs
  b' <- inferExpr b
  replaceCtx ctx
  return $ I.Let vs' b' (extract b')
inferExpr (I.Prim I.New [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.New [e'] $ ref $ extract e'
inferExpr (I.Prim I.Deref [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.Deref [e'] $ fromJust $ deref $ extract e'
inferExpr (I.Prim I.Dup [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.Dup [e'] $ extract e'
inferExpr (I.Prim I.Assign [lhs, rhs] _) = do
  rhs' <- inferExpr rhs
  lhs' <- inferExpr lhs
  let rty = extract rhs'
  case lhs' of
    I.Var v _ -> insertVar v (ref rty)
    _ -> return ()
  return $ I.Prim I.Assign [(\_ -> (ref rty)) <$> lhs', rhs'] unit
inferExpr (I.Prim I.After [del, lhs, rhs] _) = do
  del' <- inferExpr del
  rhs' <- inferExpr rhs
  lhs' <- inferExpr lhs
  let rty = extract rhs'
  case lhs' of
    I.Var v _ -> insertVar v (ref rty)
    _ -> return ()
  return $ I.Prim I.After [(\_ -> (int 32)) <$> del', (\_ -> (ref rty)) <$> lhs', rhs'] unit
inferExpr (I.Prim I.Loop es _) = do
  ctx <- get
  es' <- mapM inferExpr es
  replaceCtx ctx
  return $ I.Prim I.Loop es' unit
inferExpr (I.Prim I.Wait rs _) = do
  rs' <- mapM inferExpr rs
  return $ I.Prim I.Wait rs' unit
inferExpr (I.Prim I.Break [] _) = return $ I.Prim I.Break [] $ void
inferExpr (I.Prim I.Return [] _) = return $ I.Prim I.Return [] $ void
inferExpr (I.Prim (I.PrimOp I.PrimSub) [e1, e2]  _) = do
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  return $ I.Prim (I.PrimOp I.PrimSub) [e1', e2'] $ int 32
inferExpr (I.Prim p es t) = do
  es' <- mapM inferExpr es
  return $ I.Prim p es' (anns2Class t)
inferExpr (I.Lit I.LitEvent _) = return $ I.Lit I.LitEvent unit
inferExpr (I.Lit i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
inferExpr (I.App a b _) = do
  a' <- inferExpr a
  b' <- inferExpr b
  let retTy = snd $ fromJust $ dearrow $ extract a'
  return $ I.App a' b' retTy
inferExpr e = return $ anns2Class <$> e

localInferExpr :: [(Binder, I.Expr Ann.Type)] -> InferFn ([(Binder, I.Expr Classes.Type)])
localInferExpr [] = return []
localInferExpr ((b, e):vs) = do
  e' <- inferExpr e
  case b of
    Just vid -> insertVar vid (extract e')
    Nothing -> return ()
  vs' <- localInferExpr vs
  return $ (b, e'):vs'

anns2Class :: Ann.Type -> Classes.Type
anns2Class (Ann.Type ts) = ann2Class $ head ts

ann2Class :: Ann.TypeAnnote -> Classes.Type
ann2Class (Ann.TBuiltin bty) = Classes.TBuiltin $ fmap anns2Class bty
ann2Class (Ann.TCon tid tys) = Classes.TCon tid $ fmap anns2Class tys
ann2Class (Ann.TVar tidx   ) = Classes.TVar tidx
