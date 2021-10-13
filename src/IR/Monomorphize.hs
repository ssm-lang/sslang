module IR.Monomorphize where

import qualified Common.Compiler               as Compiler
import qualified IR.IR                         as I

import qualified IR.Types.Flat                 as Flat
import qualified IR.Types.Poly                 as Poly

monoProgram :: I.Program Poly.Type -> Compiler.Pass (I.Program Flat.Type)
monoProgram = return . fmap poly2Flat
 where
  poly2Flat :: Poly.Type -> Flat.Type
  poly2Flat (Poly.TBuiltin bty) = Flat.TBuiltin $ fmap poly2Flat bty
  poly2Flat (Poly.TCon tid tys) =
    Flat.TCon $ Flat.flattenApp tid $ fmap poly2Flat tys
  poly2Flat (Poly.TVar _) = error "Cannot handle this right now"
