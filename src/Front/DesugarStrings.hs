-- | Desugar String Literal nodes into ListExpr nodes
module Front.DesugarStrings (
  desugarStrings,
) where

import qualified Common.Compiler as Compiler
import qualified Front.Ast as A

import Data.Char (ord)
import Data.Generics (everywhere, mkT)


-- | Desugar 'A.LitString' nodes inside of an AST 'A.Program'.
desugarStrings :: A.Program -> Compiler.Pass A.Program
desugarStrings (A.Program decls) = return $ A.Program $ desugarTop <$> decls
 where
  desugarTop (A.TopDef d) = A.TopDef $ desugarDef d
  desugarTop t = t

  desugarDef (A.DefFn v bs t e) = A.DefFn v bs t $ everywhere (mkT desugarExpr) e
  desugarDef (A.DefPat b e) = A.DefPat b $ everywhere (mkT desugarExpr) e


{- | Transform a node of 'A.LitString' into a node of type 'A.ListExpr'.

For example:

@@
  (Lit (LitString "abc"))
@@

turns into

@@
  (ListExpr [97, 98, 99])
@@
-}
desugarExpr :: A.Expr -> A.Expr
desugarExpr (A.Lit (A.LitString s)) = A.ListExpr $ convertList s
 where
  convertList ls = [A.Lit $ A.LitInt $ toInteger i | i <- map ord ls]
desugarExpr e = e
