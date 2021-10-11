module IR where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import qualified IR.IR                         as I

import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes
import qualified IR.Types.Flat                 as Flat
import qualified IR.Types.Poly                 as Poly

import           IR.LowerAst                    ( lowerProgram )

lowerAst :: A.Program -> Compiler.Pass (I.Program Ann.Type)
lowerAst = return . lowerProgram

inferTypes :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferTypes = return . fmap ann2Class
 where
  ann2Class :: Ann.Type -> Classes.Type
  ann2Class (Ann.Type tys) = anns2Class $ head tys

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
