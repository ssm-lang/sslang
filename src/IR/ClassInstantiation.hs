module IR.ClassInstantiation where

import qualified Common.Compiler               as Compiler
import qualified IR.IR                         as I

import qualified IR.Types.Classes              as Classes
import qualified IR.Types.Poly                 as Poly

instProgram :: I.Program Classes.Type -> Compiler.Pass (I.Program Poly.Type)
instProgram = return . fmap class2Poly
 where
  class2Poly :: Classes.Type -> Poly.Type
  class2Poly (Classes.TBuiltin bty) = Poly.TBuiltin $ fmap class2Poly bty
  class2Poly (Classes.TCon tid tys) = Poly.TCon tid $ fmap class2Poly tys
  class2Poly (Classes.TVar tidx   ) = Poly.TVar tidx
