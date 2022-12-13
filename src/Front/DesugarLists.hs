-- | Desugar ListExpr nodes into App nodes
module Front.DesugarLists
  ( desugarLists
  ) where

import qualified Common.Compiler               as Compiler
import Common.Identifiers
import Front.Ast                                ( Definition(..)
                                                , Expr(..)
                                                , Program(..)
                                                , TopDef(..)
                                                )
import Data.Generics                            ( mkT, everywhere )                                             

-- | Desugar ListExpr nodes inside of an AST 'Program'.
desugarLists :: Program -> Compiler.Pass Program
desugarLists (Program decls) = return $ Program $ desugarTop <$> decls 
  where
    desugarTop (TopDef d) = TopDef $ desugarDef d
    desugarTop t = t
    desugarDef (DefFn v bs t e) = DefFn v bs t $ everywhere (mkT desugarExpr) e
    desugarDef (DefPat b e    ) = DefPat b $ everywhere (mkT desugarExpr) e

-- | Transform a node of type ListExpr into a node of type App
-- For ex, (ListExpr [1, 2, 3]) turns into
-- App (App (Id "Cons") (Lit (LitInt 1) ))
--     (App (App (Id "Cons") (Lit (LitInt 2)))
--          (App (App (Id "Cons") (Lit (LitInt 3))) (id "Nil")))
desugarExpr :: Expr -> Expr
desugarExpr (ListExpr es) = helper es
  where helper :: [Expr] -> Expr
        helper [] = (Id nil)
        helper (h:t) = Apply (Apply (Id cons) h) (helper t)
desugarExpr e = e
