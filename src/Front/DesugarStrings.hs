-- | Desugar String Literal nodes into ListExpr nodes
module Front.DesugarStrings
  ( desugarStrings
  ) where

import qualified Common.Compiler               as Compiler
import           Data.Char                      ( ord )
import           Data.Generics                  ( everywhere
                                                , mkT
                                                )
import           Front.Ast                      ( Definition(..)
                                                , Expr(..)
                                                , Literal(..)
                                                , Program(..)
                                                , TopDef(..)
                                                )

-- | Desugar String Literal nodes inside of an AST 'Program'.
desugarStrings :: Program -> Compiler.Pass Program
desugarStrings (Program decls) = return $ Program $ desugarTop <$> decls
 where
  desugarTop (TopDef d) = TopDef $ desugarDef d
  desugarTop t          = t

  desugarDef (DefFn v bs t e) = DefFn v bs t $ everywhere (mkT desugarExpr) e
  desugarDef (DefPat b e    ) = DefPat b $ everywhere (mkT desugarExpr) e


-- | Transform a node of type LitString into a node of type ListExpr
-- For ex, (Lit (LitString "abc")) turns into (ListExpr [97, 98, 99])
desugarExpr :: Expr -> Expr
desugarExpr (Lit (LitString s)) = ListExpr (convertList s)
  where convertList ls = [ Lit (LitInt (toInteger i)) | i <- map ord ls ]
desugarExpr e = e
