module Front.RewriteImportId where
import qualified Common.Compiler as Compiler
import Common.Identifiers
import Data.Bifunctor (first)
import Data.Generics (Data (..), everywhere, mkT)
import qualified Front.Ast as A
import Data.List



-- | Itereate the AST and first Import ASTs and generates a list of imported information
extractImportList :: A.Program -> [A.Import]
extractImportList (A.Program defs) =
	foldr extractImportListTopDef [] defs
	where extractImportListTopDef (A.TopImport imp) l = imp : l
	      extractImportListTopDef _ l = l

	

-- | rewrite importids with the full ids based on the imported list
rewriteImportList :: A.Program -> Compiler.Pass A.Program
rewriteImportList = error ("unimplemented")



-- | subst A.ImportId to a correct one
substIdentifier :: A.Expr -> A.Expr
substIdentifier (A.ImportId ids) = error ("unimplemented")
substIdentifier e = e 

