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



findRealIdentifierInImportAs :: [A.Import] -> A.Expr -> Maybe [Identifier]
findRealIdentifierInImportAs [] id = Nothing
findRealIdentifierInImportAs ((A.ImportAs ls id):rest) (impor@(A.ImportId iid@([id1, id2])))
    | id1 == id = Just (id2:ls)
	| otherwise = findRealIdentifierInImportAs rest impor

findRealIdentifierInImportAs ((A.ImportWith ls iel):rest) (impor@(A.ImportId iid@(_:_))) =
	if firstn == ls
	then let res = findinImportElement laste iel in
			       case res of
					Just e -> Just (ls ++ [e])
					_ -> Nothing
	else findRealIdentifierInImportAs rest impor
	where firstn = take ((length iid) - 1) iid
	      laste = last iid


findinImportElement element [] = Nothing
findinImportElement element ((A.ElementSymbol id): res)
	| id == element = Just id
	| otherwise = findinImportElement element res


desugarDef :: (Data a) => a -> a
desugarDef = (everywhere $ mkT $ substIdentifier)


-- | subst A.ImportId to a correct one
substIdentifier :: A.Expr -> A.Expr
substIdentifier (A.ImportId ids) = error ("unimplemented")
substIdentifier e = e

-- first get top import list, 

-- importInTopLevel :: [Identifier] -> A.Program -> ()
-- importInTopLevel impIds program =
--   -- Check if imported expression is exactly one element longer and matches on of the top imports
--   if any (\topLevelIds -> length impIds == length topLevelIds + 1 && zip impIds topLevelIds == topLevelIds) topLevelImportIds
--     then ()
--     else error "Import not found in top-level imports"
--   where
--     topLevelImports = extractImportList program
--     topLevelImportIds = map importToList topLevelImports

-- importToList :: A.Import -> [Identifier]
-- importToList (A.ImportSym ids) = ids
-- importToList (A.ImportAs ids _) = ids
-- importToList (A.ImportWith ids _) = ids

