-- | Desugar String Literal nodes into ListExpr nodes
module Front.DesugarStrings
  ( desugarStrings
  ) where

import qualified Common.Compiler               as Compiler
import           Front.Ast                      ( Definition(..)
                                                , Expr(..)
                                                , Program(..)
                                                , TopDef(..)
                                                , Literal(..)
                                                )
import           Data.Char                    ( ord
                                                )
-- | Desugar String Literal nodes inside of an AST 'Program'.
desugarStrings :: Program -> Compiler.Pass Program
desugarStrings (Program decls) = return $ Program $ desugarTop <$> decls
 where
  desugarTop (TopDef d) = TopDef $ desugarDef d
  desugarTop t          = t

  desugarDef (DefFn v bs t e) = DefFn v bs t $ desugarExpr e
  desugarDef (DefPat b e    ) = DefPat b $ desugarExpr e

-- | Transform a node of type LitString into a node of type ListExpr
-- For ex, (Lit (LitString "abc")) turns into (ListExpr [97, 98, 99])
desugarExpr :: Expr -> Expr
desugarExpr (Lit (LitString s)) = ListExpr (convertList s)
desugarExpr e = e

convertList :: [Char] -> [Expr]
convertList ls = [Lit (LitInt (toInteger i)) | i <- map ord ls]