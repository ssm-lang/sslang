{- | Infer types for optionally annotated program.

For now, this module implements a very crude form of pure, syntax-directed type
inference where type annotations can be locally and immediately inferred,
without the help of a type environment/context.
-}
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
import           Data.Maybe                     ( fromJust )

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

-- | Typing contexts (Gamma).
type Context = M.Map I.VarId Ann.Type

-- | 'emptyCtx' is the empty typing context.
emptyCtx :: Context
emptyCtx = M.empty

-- | 'addTo' adds/replaces the binding for variable @x@ in the typing context @ctx@ with type @t@.
addTo :: Context -> (I.VarId, Ann.Type) -> Context
addTo ctx (x, t) = M.insert x t ctx

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = do
  defs' <- inferTop emptyCtx (I.programDefs p)
  return $ ann2Class <$> p { I.programDefs = defs' }

inferTop :: Context -> [(I.VarId, I.Expr Ann.Type)] -> Compiler.Pass ([(I.VarId, I.Expr Ann.Type)])
inferTop _ [] = return []
inferTop ctx ((v, e):xs) = do
  let (e', ety) = (inferExpr ctx e, extract e')
  xs' <- inferTop (addTo ctx (v, ety)) xs
  return $ (v, e):xs'

inferExpr :: Context -> I.Expr Ann.Type -> I.Expr Ann.Type
inferExpr ctx (I.Var v t) = case M.lookup v ctx of
    Nothing -> I.Var v t
    Just t' -> I.Var v (t' <> t)
inferExpr ctx (I.Lambda v  b t) = I.Lambda v (inferExpr ctx b) t
inferExpr ctx (I.Let    vs b t) = I.Let (reverse vs') b' (bty <> t)
  where
    (ctx', vs') = foldl inferExprLet (ctx, []) vs
    (b', bty)  = (inferExpr ctx' b, extract b')
inferExpr ctx (I.Prim I.New [e] t) = I.Prim I.New [e'] (ref ety <> t)
  where (e', ety) = (inferExpr ctx e, extract e')
inferExpr ctx (I.Prim I.Deref [e] t) = I.Prim I.Deref
                                          [e']
                                          (fromJust (deref ety) <> t)
  where (e', ety) = (inferExpr ctx e, extract e')
inferExpr ctx (I.Prim I.Dup [e] t) = I.Prim I.Dup [e'] (ety <> t)
  where (e', ety) = (inferExpr ctx e, extract e')
inferExpr ctx (I.Prim I.Assign [lhs, rhs] t) = I.Prim
  I.Assign
  [fmap ((ref $ extract rhs') <>) lhs', rhs']
  (unit <> t)
  where (lhs', rhs') = (inferExpr ctx lhs, inferExpr ctx rhs)
inferExpr ctx (I.Prim I.After [del, lhs, rhs] t) = I.Prim
  I.After
  [fmap (int 32 <>) del', fmap ((ref $ extract rhs') <>) lhs', rhs']
  (unit <> t)
  where (del', lhs', rhs') = (inferExpr ctx del, inferExpr ctx lhs, inferExpr ctx rhs)
inferExpr ctx (I.Prim I.Loop               [b] t) = I.Prim I.Loop [inferExpr ctx b] $ unit <> t
inferExpr _   (I.Prim I.Break              []  t) = I.Prim I.Break [] $ void <> t
inferExpr _   (I.Prim I.Return             []  t) = I.Prim I.Return [] $ void <> t
inferExpr _   (I.Prim (I.PrimOp I.PrimSub) []  t) = I.Prim I.Return [] $ int 32 <> t
inferExpr ctx (I.Prim p                    es  t) = I.Prim p (map (inferExpr ctx) es) t
inferExpr _   (I.Lit I.LitEvent                t) = I.Lit I.LitEvent $ unit <> t
inferExpr _   (I.Lit i@(I.LitIntegral _)       t) = I.Lit i $ int 32 <> t
inferExpr ctx (I.App a b t) = I.App a' b' (retTy <> t)
 where
  (a', b') = (inferExpr ctx a, inferExpr ctx b)
  retTy    = snd $ fromJust $ dearrow $ extract a'
inferExpr _ e = e

inferExprLet :: (Context, [(Binder, I.Expr Ann.Type)]) -> (Binder, I.Expr Ann.Type) -> (Context, [(Binder, I.Expr Ann.Type)])
inferExprLet (ctx, bs) (b, e) = case b of
  Nothing -> (ctx, (b, e):bs)
  Just v  -> ((addTo ctx (v, ety)), (b, e'):bs)
    where
      (e', ety) = (inferExpr ctx e, extract e')

ann2Class :: Ann.Type -> Classes.Type
ann2Class (Ann.Type ts) = anns2Class $ head ts

anns2Class :: Ann.TypeAnnote -> Classes.Type
anns2Class (Ann.TBuiltin bty) = Classes.TBuiltin $ fmap ann2Class bty
anns2Class (Ann.TCon tid tys) = Classes.TCon tid $ fmap ann2Class tys
anns2Class (Ann.TVar tidx   ) = Classes.TVar tidx
