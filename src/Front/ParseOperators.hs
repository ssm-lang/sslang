{-# LANGUAGE OverloadedStrings #-}

-- | Parse OpRegion nodes inside of an AST 'Program'.
module Front.ParseOperators (
  parseOperators,
  A.Fixity (..),
) where

import qualified Common.Compiler as Compiler
import Common.Identifiers (Identifier)

import qualified Front.Ast as A

import Data.Bifunctor (Bifunctor (..))
import qualified Data.Map.Strict as Map


data Stack
  = BOS
  | Stack Stack A.Expr Identifier


-- FIXME: These should be defined and included in the standard library

-- | Default fixity of operators.
defaultOps :: [A.Fixity]
defaultOps =
  [ A.Infixl 4 "=="
  , A.Infixl 4 "!="
  , A.Infixl 4 "<="
  , A.Infixl 4 ">="
  , A.Infixl 4 "<"
  , A.Infixl 4 ">"
  , A.Infixl 6 "+"
  , A.Infixl 6 "-"
  , A.Infixl 8 "*"
  , A.Infixl 8 "/"
  , A.Infixl 8 "%"
  , A.Infixr 8 "^"
  ]


-- | Parse OpRegion nodes inside of an AST 'Program'.
parseOperators :: A.Program -> Compiler.Pass A.Program
parseOperators (A.Program decls) = return $ A.Program $ map (parseTop ops) decls
 where
  ops = defaultOps
  parseTop fs (A.TopDef d) = A.TopDef $ parseDef fs d
  parseTop _ t = t

  parseDef fs (A.DefFn v bs t e) = A.DefFn v bs t $ parseExprOps fs e
  parseDef fs (A.DefPat b e) = A.DefPat b $ parseExprOps fs e


{- | Remove the OpRegion constructs in the AST by parsing the operators
   according to the given Fixity specifications
-}
parseExprOps :: [A.Fixity] -> A.Expr -> A.Expr
parseExprOps fixity = rw
 where
  rw r@(A.OpRegion _ _) = let A.OpRegion e r' = rewrite rw r in step BOS e r'
  rw e = rewrite rw e

  defaultPrec = (18, 17)
  opMap = Map.fromList $ map fixToPair fixity

  fixToPair (A.Infixl prec op) = (op, (prec * 2, prec * 2 - 1))
  fixToPair (A.Infixr prec op) = (op, (prec * 2, prec * 2 + 1))

  step :: Stack -> A.Expr -> A.OpRegion -> A.Expr
  step BOS e A.EOR = e
  step BOS e1 (A.NextOp op e2 ts) = shift BOS e1 op e2 ts
  step (Stack s e1 op) e2 A.EOR = reduce s e1 op e2 A.EOR
  step s0@(Stack s1 e1 op1) e2 t0@(A.NextOp op2 e3 t1)
    | op1 `shouldShift` op2 = shift s0 e2 op2 e3 t1
    | otherwise = reduce s1 e1 op1 e2 t0

  shift, reduce :: Stack -> A.Expr -> Identifier -> A.Expr -> A.OpRegion -> A.Expr
  shift s e1 op e2 ts = step (Stack s e1 op) e2 ts
  reduce s e1 op e2 ts = step s (A.Apply (A.Apply (A.Id op) e1) e2) ts

  shouldShift :: Identifier -> Identifier -> Bool
  shouldShift opl opr = pl < pr
   where
    pl = fst prel
    pr = snd prer
    prel = Map.findWithDefault defaultPrec opl opMap
    prer = Map.findWithDefault defaultPrec opr opMap
  rewrite :: (A.Expr -> A.Expr) -> A.Expr -> A.Expr
  rewrite f (A.Apply e1 e2) = A.Apply (f e1) (f e2)
  rewrite f (A.OpRegion e r) = A.OpRegion (f e) (h r)
   where
    h A.EOR = A.EOR
    h (A.NextOp op e' r') = A.NextOp op (f e') (h r')
  rewrite f (A.Let d b) = A.Let (map g d) $ f b
   where
    g (A.DefFn v bs t e) = A.DefFn v bs t $ f e
    g (A.DefPat p e) = A.DefPat p $ f e
  rewrite f (A.While e1 e2) = A.While (f e1) (f e2)
  rewrite f (A.Loop e) = A.Loop (f e)
  rewrite f (A.Par e) = A.Par $ map f e
  rewrite f (A.Wait e) = A.Wait $ map f e
  rewrite f (A.IfElse e1 e2 e3) = A.IfElse (f e1) (f e2) (f e3)
  rewrite f (A.After e1 p e2) = A.After (f e1) p (f e2)
  rewrite f (A.Assign p e) = A.Assign p (f e)
  rewrite f (A.Constraint e t) = A.Constraint (f e) t
  rewrite f (A.Seq e1 e2) = A.Seq (f e1) (f e2)
  rewrite f (A.Lambda ps b) = A.Lambda ps (f b)
  rewrite f (A.Match s as) = A.Match (f s) $ map (second f) as
  rewrite _ e = e
