-- | Desugar ListExpr nodes into App nodes
module Front.DesugarLists
  ( desugarLists,
  )
where

import qualified Common.Compiler as Compiler
import Front.Ast as A
import Common.Identifiers as I

-- | Desugar ListExpr nodes inside of an AST 'Program'.
desugarLists :: Program -> Compiler.Pass Program
desugarLists (Program decls) = return $ Program $ desugarTop <$> decls --test
  where
    desugarTop (TopDef d) = TopDef $ desugarDef d
    desugarTop t = t
    -- test = Compiler.unexpected $ (show $ (A.foldApp) (A.Id (I.Identifier "Cons")) [(Lit (LitInt 1)), (Lit (LitInt 2))]) 
    desugarDef (DefFn v bs t e) = DefFn v bs t $ desugarExpr e
    desugarDef (DefPat b e) = DefPat b $ desugarExpr e

-- | Transform a node of type ListExpr into a node of type App
-- For ex, (ListExpr [1, 2, 3]) turns into
-- App (App (Id "Cons") (Lit (LitInt 1) ))
--     (App (App (Id "Cons") (Lit (LitInt 2)))
--          (App (App (Id "Cons") (Lit (LitInt 3))) (id "Nil")))

appidhelper:: A.ListExpr -> A.ListExpr
appidhelper [] = [(A.Id (I.Identifier "App")) (A.Id (I.Identifier "Cons")) (A.Id (I.Identifier "Nil"))]
appidhelper (h:t) = A.Id (I.Identifier "App") (A.Id (I.Identifier "Cons")) h : appidhelper t
  
--[(A.Id (I.Identifier "App")) (A.Id (I.Identifier "Cons")) | i <- a] 
desugarExpr :: Expr -> Expr
--desugarExpr _ = Compiler.unexpected $ (show (A.Lit(A.LitInt 5)))
desugarExpr (A.ListExpr es) = A.foldApp h t
where
  (h : t) = appidhelper es

