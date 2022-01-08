{-# LANGUAGE OverloadedStrings #-}
-- | Parse 'OpRegion' nodes inside of an AST 'Program'.
module Front.ParseOperators
  ( parseOperators
  , Fixity(..)
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers

import           Front.Ast                      ( Definition(..)
                                                , Expr(..)
                                                , Fixity(..)
                                                , OpRegion(..)
                                                , Program(..)
                                                , TopDef(..)
                                                )

import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.Map.Strict               as Map

data Stack = BOS
           | Stack Stack Expr Identifier

-- FIXME: These should be defined and included in the standard library
-- | Default fixity of operators.
defaultOps :: [Fixity]
defaultOps =
  [Infixl 6 "+", Infixl 6 "-", Infixl 7 "*", Infixl 8 "/", Infixr 8 "^"]

-- | Parse OpRegion nodes inside of an AST 'Program'.
parseOperators :: Program -> Compiler.Pass Program
parseOperators (Program decls) = return $ Program $ map (parseTop ops) decls
 where
  ops = defaultOps
  parseTop fs  (TopDef  d) = TopDef $ parseDef fs d
  parseTop _fs (TopType t) = TopType t

  parseDef fs (DefFn v bs t e) = DefFn v bs t $ parseExprOps fs e
  parseDef fs (DefPat b e    ) = DefPat b $ parseExprOps fs e

-- | Remove the OpRegion constructs in the AST by parsing the operators
--   according to the given Fixity specifications
parseExprOps :: [Fixity] -> Expr -> Expr
parseExprOps fixity = rw
 where
  rw r@(OpRegion _ _) = let OpRegion e r' = rewrite rw r in step BOS e r'
  rw e                = rewrite rw e

  defaultPrec = (18, 17)
  opMap       = Map.fromList $ map fixToPair fixity

  fixToPair (Infixl prec op) = (op, (prec * 2, prec * 2 - 1))
  fixToPair (Infixr prec op) = (op, (prec * 2, prec * 2 + 1))

  step :: Stack -> Expr -> OpRegion -> Expr
  step BOS             e  EOR               = e
  step BOS             e1 (NextOp op e2 ts) = shift BOS e1 op e2 ts
  step (Stack s e1 op) e2 EOR               = reduce s e1 op e2 EOR
  step s0@(Stack s1 e1 op1) e2 t0@(NextOp op2 e3 t1)
    | op1 `shouldShift` op2 = shift s0 e2 op2 e3 t1
    | otherwise             = reduce s1 e1 op1 e2 t0

  shift, reduce :: Stack -> Expr -> Identifier -> Expr -> OpRegion -> Expr
  shift s e1 op e2 ts = step (Stack s e1 op) e2 ts
  reduce s e1 op e2 ts = step s (Apply (Apply (Id op) e1) e2) ts

  shouldShift :: Identifier -> Identifier -> Bool
  shouldShift opl opr = pl < pr
   where
    pl   = fst prel
    pr   = snd prer
    prel = Map.findWithDefault defaultPrec opl opMap
    prer = Map.findWithDefault defaultPrec opr opMap

  -- | Rewriter for expressions. ('Expr' is almost a functor, except its kind is
  -- @*@; functors must be of kind @* -> *@.)
  rewrite :: (Expr -> Expr) -> Expr -> Expr
  rewrite f (Apply    e1 e2) = Apply (f e1) (f e2)
  rewrite f (OpRegion e  r ) = OpRegion (f e) (h r)
   where
    h EOR               = EOR
    h (NextOp op e' r') = NextOp op (f e') (h r')
  rewrite f (Let d b) = Let (map g d) $ f b
   where
    g (DefFn v bs t e) = DefFn v bs t $ f e
    g (DefPat p e    ) = DefPat p $ f e
  rewrite f (While e1 e2     ) = While (f e1) (f e2)
  rewrite f (Loop e          ) = Loop (f e)
  rewrite f (Par  e          ) = Par $ map f e
  rewrite f (Wait e          ) = Wait $ map f e
  rewrite f (IfElse e1 e2 e3 ) = IfElse (f e1) (f e2) (f e3)
  rewrite f (After  e1 p  e2 ) = After (f e1) p (f e2)
  rewrite f (Assign     p  e ) = Assign p (f e)
  rewrite f (Constraint e  t ) = Constraint (f e) t
  rewrite f (Seq        e1 e2) = Seq (f e1) (f e2)
  rewrite f (Lambda     ps b ) = Lambda ps (f b)
  rewrite f (Match      s  as) = Match (f s) $ map (second f) as
  rewrite _ e                  = e
