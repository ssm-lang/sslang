module RewriteImportList where
import qualified Common.Compiler as Compiler
import Common.Identifiers
import Data.Bifunctor (first)
import Data.Generics (Data (..), everywhere, mkT)
import qualified Front.Ast as A



-- | Itereate the AST and first Import ASTs and generates a list of imported information
extractImportList :: A.Program -> Compiler.Pass A.Program



-- | rewrite importids with the full ids based on the imported list
rewriteImportList :: A.Program -> Compiler.Pass A.Program
