module IR where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import qualified IR.IR                         as I

import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes
import qualified IR.Types.Flat                 as Flat
import qualified IR.Types.Poly                 as Poly

import           Control.Comonad                ( Comonad(..) )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           GHC.Stack                      ( HasCallStack )
import           IR.LowerAst                    ( lowerProgram )
import           IR.Types.TypeSystem            ( int
                                                , ref
                                                , unit
                                                , void
                                                )

lowerAst :: A.Program -> Compiler.Pass (I.Program Ann.Type)
lowerAst = return . lowerProgram

inferTypes :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferTypes p@I.Program { I.programDefs = defs } = return $ ann2Class <$> p
  { I.programDefs = second pmInfer <$> defs
  }
 where
  pmInfer :: I.Expr Ann.Type -> I.Expr Ann.Type
  pmInfer (I.Let vs b t) | bty /= Ann.untyped = I.Let vs b' (bty <> t)
    where (b', bty) = (pmInfer b, extract b')
  pmInfer (I.Prim I.New [e] t) | ety /= Ann.untyped = I.Prim I.New
                                                             [e']
                                                             (ety <> t)
    where (e', ety) = (pmInfer e, extract e')
  pmInfer (I.Prim I.Dup [e] t) | ety /= Ann.untyped = I.Prim I.Dup
                                                             [e']
                                                             (ety <> t)
    where (e', ety) = (pmInfer e, extract e')
  pmInfer (I.Prim I.Assign [lhs, rhs] t) = I.Prim
    I.Assign
    [fmap ((ref $ extract rhs') <>) lhs', rhs']
    (unit <> t)
    where (lhs', rhs') = (pmInfer lhs, pmInfer rhs)
  pmInfer (I.Prim I.After [del, lhs, rhs] t) = I.Prim
    I.After
    [fmap (int 32 <>) del', fmap ((ref $ extract rhs') <>) lhs', rhs']
    (unit <> t)
    where (del', lhs', rhs') = (pmInfer del, pmInfer lhs, pmInfer rhs)
  -- pmInfer (I.Prim I.Wait       rs t) = unit <> t
  pmInfer (I.Prim I.Loop       [b]  t) = I.Prim I.Loop [pmInfer b] $ unit <> t
  pmInfer (I.Prim I.Break      []  t) = I.Prim I.Break [] $ void <> t
  pmInfer (I.Prim I.Return     []  t) = I.Prim I.Return [] $ void <> t
  -- pmInfer (I.Prim (I.PrimOp _) _  t) = void <> t
  -- pmInfer (I.Prim I.Fork ps t) | all ((/= Ann.untyped) . pmInfer) ps = unit <> t
  pmInfer e = e

  ann2Class :: HasCallStack => Ann.Type -> Classes.Type
  ann2Class (Ann.Type []      ) = error "no type annotation"
  ann2Class (Ann.Type (ty : _)) = anns2Class ty

  anns2Class :: Ann.TypeAnnote -> Classes.Type
  anns2Class (Ann.TBuiltin bty) = Classes.TBuiltin $ fmap ann2Class bty
  anns2Class (Ann.TCon tid tys) = Classes.TCon tid $ fmap ann2Class tys
  anns2Class (Ann.TVar tidx   ) = Classes.TVar tidx

instantiateClasses
  :: I.Program Classes.Type -> Compiler.Pass (I.Program Poly.Type)
instantiateClasses = return . fmap class2Poly
 where
  class2Poly :: Classes.Type -> Poly.Type
  class2Poly (Classes.TBuiltin bty) = Poly.TBuiltin $ fmap class2Poly bty
  class2Poly (Classes.TCon tid tys) = Poly.TCon tid $ fmap class2Poly tys
  class2Poly (Classes.TVar tidx   ) = Poly.TVar tidx

yieldAbstraction :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
yieldAbstraction = return

lambdaLift :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
lambdaLift = return

defunctionalize :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
defunctionalize = return

inferDrops :: I.Program a -> Compiler.Pass (I.Program a)
inferDrops = return

monomorphize :: I.Program Poly.Type -> Compiler.Pass (I.Program Flat.Type)
monomorphize = return . fmap poly2Flat
 where
  poly2Flat :: Poly.Type -> Flat.Type
  poly2Flat (Poly.TBuiltin bty) = Flat.TBuiltin $ fmap poly2Flat bty
  poly2Flat (Poly.TCon tid tys) =
    Flat.TCon $ Flat.flattenApp tid $ fmap poly2Flat tys
  poly2Flat (Poly.TVar _) = error "Cannot handle this right now"
