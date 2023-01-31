-- | Desugar 'ListExpr' nodes into 'App' nodes
module Front.DesugarLists (desugarLists) where

import qualified Common.Compiler as Compiler
import Common.Identifiers
import Data.Generics (everywhere, mkT)
import qualified Front.Ast as A


-- | Desugar ListExpr nodes inside of an AST 'A.Program'.
desugarLists :: A.Program -> Compiler.Pass A.Program
desugarLists (A.Program decls) = return $ A.Program $ desugarTop <$> decls
 where
  desugarTop (A.TopDef d) = A.TopDef $ desugarDef d
  desugarTop t = t
  desugarDef (A.DefFn v bs t e) = A.DefFn v bs t $ everywhere (mkT desugarExpr) e
  desugarDef (A.DefPat b e) = A.DefPat b $ everywhere (mkT desugarExpr) e


{- | Transform a node of 'ListExpr' into a node of 'App'.

For example,
@@
(ListExpr [1, 2, 3])
@@

turns into

@@
App (App (Id "Cons") (Lit (LitInt 1) ))
  (App (App (Id "Cons") (Lit (LitInt 2)))
       (App (App (Id "Cons") (Lit (LitInt 3))) (id "Nil")))
@@
-}
desugarExpr :: A.Expr -> A.Expr
desugarExpr (A.ListExpr es) = helper es
 where
  helper :: [A.Expr] -> A.Expr
  helper [] = A.Id nil
  helper (h : t) = A.Apply (A.Apply (A.Id cons) h) (helper t)
desugarExpr e = e
