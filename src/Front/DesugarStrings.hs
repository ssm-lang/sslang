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
import                                          Common.Identifiers
import           Data.Char                      ( ord
                                                )
import Data.Generics                            ( mkT, everywhere )                                             

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
desugarExpr (Apply e1 e2) = Apply (desugarExpr e1) (desugarExpr e2)
desugarExpr (Lambda p e) = Lambda p (desugarExpr e)
desugarExpr (OpRegion e o1) = OpRegion (desugarExpr e) o1
desugarExpr (Let d e) = Let (map extractExpr d) (desugarExpr e)
desugarExpr (While e1 e2) = While  (desugarExpr e1) (desugarExpr e2)
desugarExpr (Loop e) = Loop (desugarExpr e)
desugarExpr (Par ls) = Par (map desugarExpr ls)
desugarExpr (IfElse e1 e2 e3) = IfElse (desugarExpr e1) (desugarExpr e2) (desugarExpr e3)
desugarExpr (After e1 e2 e3) = After (desugarExpr e1) (desugarExpr e2) (desugarExpr e3)
desugarExpr (Assign e1 e2) = Assign (desugarExpr e1) (desugarExpr e2)
desugarExpr (Constraint e t) = Constraint (desugarExpr e) t
desugarExpr (Wait ls) = Wait (map desugarExpr ls)               
desugarExpr (Seq e1 e2) = Seq (desugarExpr e1) (desugarExpr e2)
desugarExpr (Match e1 [(p, e2)]) = Match (desugarExpr e1) [(p, desugarExpr e2)]
desugarExpr (CCall i ls) = CCall i (map desugarExpr ls)
desugarExpr (ListExpr es) = func es
desugarExpr e = e 

func :: [Expr] -> Expr
func [] = Id (Identifier "Nil")
func (h:t) = Apply (Apply (Id (Identifier "Cons")) h) (func t)

convertList :: [Char] -> [Expr]
convertList ls = [Lit (LitInt (toInteger i)) | i <- map ord ls]

extractExpr :: Definition -> Definition
extractExpr (DefFn i p t e) = DefFn i p t (desugarExpr e)
extractExpr (DefPat p e) = DefPat p (desugarExpr e)