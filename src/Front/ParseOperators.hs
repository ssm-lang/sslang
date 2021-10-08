module Front.ParseOperators
  ( parseOperators
  , Fixity(..)
  ) where

import qualified Data.Map.Strict               as Map
import           Front.Ast                      ( Declaration(..)
                                                , Def(..)
                                                , Expr(..)
                                                , OpRegion(..)
                                                , Program(..)
                                                )

data Fixity = Infixl Int String
            | Infixr Int String

data Stack = BOS
           | Stack Stack Expr String

-- FIXME: These should be defined and included in the standard library
defaultOps :: [Fixity]
defaultOps =
  [Infixl 6 "+", Infixl 6 "-", Infixl 7 "*", Infixl 8 "/", Infixr 8 "^"]

parseOperators :: Program -> Program
parseOperators (Program decls) = Program $ map (parseOps defaultOps) decls
  where parseOps fs (Function s bs e t) = Function s bs (parseExprOps fs e) t

-- | Remove the OpRegion constructs in the AST by parsing the operators
--   according to the given Fixity specifications
parseExprOps :: [Fixity] -> Expr -> Expr
parseExprOps fixity expr = rw expr
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

  shift, reduce :: Stack -> Expr -> String -> Expr -> OpRegion -> Expr
  shift s e1 op e2 ts = step (Stack s e1 op) e2 ts
  reduce s e1 op e2 ts = step s (Apply (Apply (Id op) e1) e2) ts

  shouldShift :: String -> String -> Bool
  shouldShift opl opr = pl < pr
   where
    pl   = fst prel
    pr   = snd prer
    prel = Map.findWithDefault defaultPrec opl opMap
    prer = Map.findWithDefault defaultPrec opr opMap

  rewrite :: (Expr -> Expr) -> Expr -> Expr
  rewrite f (Apply    e1 e2) = Apply (f e1) (f e2)
  rewrite f (OpRegion e  r ) = OpRegion (f e) (h r)
   where
    h EOR               = EOR
    h (NextOp op e' r') = NextOp op (f e') (h r')
  rewrite f (Let   d  b      ) = Let (map (\(Def p e) -> Def p (f e)) d) b
  rewrite f (While e1 e2     ) = While (f e1) (f e2)
  rewrite f (Loop e          ) = Loop (f e)
  rewrite f (Par  e          ) = Par $ map f e
  rewrite f (Wait e          ) = Wait $ map f e
  rewrite f (IfElse e1 e2 e3 ) = IfElse (f e1) (f e2) (f e3)
  rewrite f (Later  e1 p  e2 ) = Later (f e1) p (f e2)
  rewrite f (Assign     p  e ) = Assign p (f e)
  rewrite f (Constraint e  t ) = Constraint (f e) t
  rewrite f (As         s  e ) = As s (f e)
  rewrite f (Seq        e1 e2) = Seq (f e1) (f e2)
  rewrite _ e                  = e
