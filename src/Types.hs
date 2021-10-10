module Types where

import qualified Common.Compiler               as Compiler
import           IR.IR                          ( Program(..) )

import qualified Types.Ast                     as Ast
import qualified Types.Classes                 as Classes
import qualified Types.Flat                    as Flat
import qualified Types.Poly                    as Poly

inferTypes :: Program Ast.Type -> Compiler.Pass (Program Classes.Type)
inferTypes = return . fmap ast2Class
 where
  ast2Class :: Ast.Type -> Classes.Type
  ast2Class (Ast.Type tys) = ann2Class $ head tys

  ann2Class :: Ast.TypeAnnote -> Classes.Type
  ann2Class (Ast.TBuiltin bty) = Classes.TBuiltin $ fmap ast2Class bty
  ann2Class (Ast.TCon tid tys) = Classes.TCon tid $ fmap ast2Class tys
  ann2Class (Ast.TVar tidx   ) = Classes.TVar tidx

instantiateClasses :: Program Classes.Type -> Compiler.Pass (Program Poly.Type)
instantiateClasses = return . fmap class2Poly
 where
  class2Poly :: Classes.Type -> Poly.Type
  class2Poly (Classes.TBuiltin bty) = Poly.TBuiltin $ fmap class2Poly bty
  class2Poly (Classes.TCon tid tys) = Poly.TCon tid $ fmap class2Poly tys
  class2Poly (Classes.TVar tidx   ) = Poly.TVar tidx

monomorphize :: Program Poly.Type -> Compiler.Pass (Program Flat.Type)
monomorphize = return . fmap poly2Flat
  where poly2Flat :: Poly.Type -> Flat.Type
        poly2Flat (Poly.TBuiltin bty) = Flat.TBuiltin $ fmap poly2Flat bty
        poly2Flat (Poly.TCon tid tys) = Flat.TCon tid -- TODO concat tys
        poly2Flat (Poly.TVar _) = error "Cannot handle this right now"
