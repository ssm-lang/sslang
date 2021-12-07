module IR.ClassInstantiation where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers                       ( TConId(..)
                                                          , VarId(..)
                                                          )
import           Data.Bifunctor                           ( second )
import qualified IR.IR                         as I

import qualified IR.Types.Classes              as Classes
import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem                      ( TypeDef(..) )

instProgram :: I.Program Classes.Type -> Compiler.Pass (I.Program Poly.Type)
instProgram I.Program { I.programEntry = pEntry, I.programDefs = pdefs, I.programTypes = tdefs, I.programClasses = cdefs, I.programInsts = idefs }
    = return $ I.Program { I.programEntry   = pEntry
                         , I.programDefs    = instInsts idefs pdefs
                         , I.programTypes   = ctdefs ++ tdefs'
                         , I.programClasses = []
                         , I.programInsts   = []
                         }
  where
    ctdefs = instClasses cdefs
    tdefs' = fmap (second $ fmap class2Poly) tdefs

instClasses :: [I.ClassDef Classes.Type] -> [(TConId, TypeDef Poly.Type)]
instClasses _ = []

instInsts
    :: [I.InstDef Classes.Type]
    -> [(VarId, I.Expr Classes.Type)]
    -> [(VarId, I.Expr Poly.Type)]
instInsts _ = fmap (second $ fmap class2Poly)


class2Poly :: Classes.Type -> Poly.Type
class2Poly (Classes.TBuiltin bty) = Poly.TBuiltin $ fmap class2Poly bty
class2Poly (Classes.TCon tid tys) = Poly.TCon tid $ fmap class2Poly tys
class2Poly (Classes.TVar tidx   ) = Poly.TVar tidx
