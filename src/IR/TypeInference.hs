{- | Infer types for optionally annotated program.

For now, this module implements a very crude form of pure, syntax-directed type
inference where type annotations can be locally and immediately inferred,
without the help of a type environment/context.
-}
module IR.TypeInference where

import qualified Common.Compiler               as Compiler

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes

import           IR.Types.TypeSystem            ( int
                                                , ref
                                                , unit
                                                , void
                                                )

import           Control.Comonad                ( Comonad(..) )
import           Data.Bifunctor                 ( Bifunctor(..) )

inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = return $ ann2Class <$> p { I.programDefs = defs' }
 where
  defs' = second inferExpr <$> I.programDefs p

  ann2Class :: Ann.Type -> Classes.Type
  ann2Class (Ann.Type ts) = anns2Class $ head ts

  anns2Class :: Ann.TypeAnnote -> Classes.Type
  anns2Class (Ann.TBuiltin bty) = Classes.TBuiltin $ fmap ann2Class bty
  anns2Class (Ann.TCon tid tys) = Classes.TCon tid $ fmap ann2Class tys
  anns2Class (Ann.TVar tidx   ) = Classes.TVar tidx

inferExpr :: I.Expr Ann.Type -> I.Expr Ann.Type
inferExpr (I.Let vs b t) = I.Let vs b' (bty <> t)
  where (b', bty) = (inferExpr b, extract b')
inferExpr (I.Prim I.New [e] t) = I.Prim I.New [e'] (ety <> t)
  where (e', ety) = (inferExpr e, extract e')
inferExpr (I.Prim I.Dup [e] t) = I.Prim I.Dup [e'] (ety <> t)
  where (e', ety) = (inferExpr e, extract e')
inferExpr (I.Prim I.Assign [lhs, rhs] t) = I.Prim
  I.Assign
  [fmap ((ref $ extract rhs') <>) lhs', rhs']
  (unit <> t)
  where (lhs', rhs') = (inferExpr lhs, inferExpr rhs)
inferExpr (I.Prim I.After [del, lhs, rhs] t) = I.Prim
  I.After
  [fmap (int 32 <>) del', fmap ((ref $ extract rhs') <>) lhs', rhs']
  (unit <> t)
  where (del', lhs', rhs') = (inferExpr del, inferExpr lhs, inferExpr rhs)
inferExpr (I.Prim I.Loop [b] t) = I.Prim I.Loop [inferExpr b] $ unit <> t
inferExpr (I.Prim I.Break  []  t      ) = I.Prim I.Break [] $ void <> t
inferExpr (I.Prim I.Return []  t      ) = I.Prim I.Return [] $ void <> t
inferExpr (I.Lit I.LitEvent          t) = I.Lit I.LitEvent $ unit <> t
inferExpr (I.Lit i@(I.LitIntegral _) t) = I.Lit i $ int 32 <> t
-- inferExpr (I.Prim (I.PrimOp _) _  t) = void <> t
-- inferExpr (I.Prim I.Par ps t) | all ((/= Ann.untyped) . inferExpr) ps = unit <> t
inferExpr e                             = e
