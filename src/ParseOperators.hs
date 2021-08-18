module ParseOperators ( parseOperators, Fixity(..) ) where

import Ast
import qualified Data.Map.Strict as Map

data Fixity = Infixl Int String
            | Infixr Int String

data Stack = BOS
           | Stack Stack Expr String

-- | Remove the OpRegion constructs in the AST by parsing the operators
--   according to the given Fixity specifications
parseOperators :: [Fixity] -> Expr -> Expr
parseOperators fixity expr = rw expr
  where
    rw r@(OpRegion _ _) = let OpRegion e r' = rewrite rw r
                          in step BOS e r'
    rw e = rewrite rw e

    step :: Stack -> Expr -> OpRegion -> Expr
    step BOS                  e  EOR               = e
    step BOS                  e1 (NextOp op e2 ts) = shift  BOS e1 op  e2 ts
    step (Stack s e1 op)      e2 EOR               = reduce s   e1 op  e2 EOR
    step s0@(Stack s1 e1 op1) e2 t0@(NextOp op2 e3 t1)
                      | op1 `shouldShift` op2      = shift  s0  e2 op2 e3 t1
                      | otherwise                  = reduce s1  e1 op1 e2 t0

    shift, reduce :: Stack -> Expr -> String -> Expr -> OpRegion -> Expr
    shift  s e1 op e2 ts = step (Stack s e1 op) e2                            ts
    reduce s e1 op e2 ts = step s               (Apply (Apply (Id op) e1) e2) ts

    shouldShift :: String -> String -> Bool
    shouldShift opl opr = pl < pr
      where
        pl = fst prel
        pr = snd prer
        prel = Map.findWithDefault defaultPrec opl opMap
        prer = Map.findWithDefault defaultPrec opr opMap

    defaultPrec = (18, 17)

    opMap = Map.fromList $ map fixToPair fixity

    fixToPair (Infixl prec op) = (op, (prec * 2, prec * 2 - 1))
    fixToPair (Infixr prec op) = (op, (prec * 2, prec * 2 + 1))    
