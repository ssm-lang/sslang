-- | Parse out the op regions leftover from the parser.
module Front.ParseOperators
  ( parseOperators
  , Fixity(..)
  ) where

import qualified Data.Map.Strict               as Map
import           Front.Ast                      ( Definition(..)
                                                , Expr(..)
                                                , OpRegion(..)
                                                , Program(..)
                                                )

-- | The associativity of an operator and its precedence.
data Fixity = Infixl Int String -- ^ Left-associative infix operator
            | Infixr Int String -- ^ Right-associative infix operator

-- | Stack of expressions, used during parsing.
type Stack = [(Expr, String)]

-- | FIXME: These should be defined and included in the standard library
defaultOps :: [Fixity]
defaultOps =
  [Infixl 6 "+", Infixl 6 "-", Infixl 7 "*", Infixl 8 "/", Infixr 8 "^"]

-- | Unflatten op regions to application.
parseOperators :: Program -> Program
parseOperators (Program decls) = Program $ map (parseOps defaultOps) decls
 where
  parseOps fs (DefFn v bs t e) = DefFn v bs t $ parseExprOps fs e
  parseOps fs (DefPat b e    ) = DefPat b $ parseExprOps fs e

{- | Remove the OpRegion constructs in the AST by parsing the operators
according to the given Fixity specifications
-}
parseExprOps :: [Fixity] -> Expr -> Expr
parseExprOps fixity = rw
 where
  rw r@(OpRegion _ _) = let OpRegion e r' = rewrite rw r in step [] e r'
  rw e                = rewrite rw e

  defaultPrec = (18, 17)
  opMap       = Map.fromList $ map fixToPair fixity

  fixToPair (Infixl prec op) = (op, (prec * 2, prec * 2 - 1))
  fixToPair (Infixr prec op) = (op, (prec * 2, prec * 2 + 1))

  step :: Stack -> Expr -> OpRegion -> Expr
  step [] e  EOR               = e
  step [] e1 (NextOp op e2 ts) = shift [] e1 op e2 ts
  step ((e1, op):s) e2 EOR               = reduce s e1 op e2 EOR
  step s0@((e1, op1):s1) e2 t0@(NextOp op2 e3 t1)
    | op1 `shouldShift` op2 = shift s0 e2 op2 e3 t1
    | otherwise             = reduce s1 e1 op1 e2 t0

  shift, reduce :: Stack -> Expr -> String -> Expr -> OpRegion -> Expr
  shift s e1 op e2 ts = step ((e1, op):s) e2 ts
  reduce s e1 op e2 ts = step s (Apply (Apply (Id op) e1) e2) ts

  shouldShift :: String -> String -> Bool
  shouldShift opl opr = pl < pr
   where
    pl   = fst prel
    pr   = snd prer
    prel = Map.findWithDefault defaultPrec opl opMap
    prer = Map.findWithDefault defaultPrec opr opMap

  {- | Rewriter for expressions.

  'Expr' is almost a functor, except its kind is @*@; functors must be of
  kind @* -> *@.
  -}
  rewrite :: (Expr -> Expr) -> Expr -> Expr
  rewrite f (Apply    e1 e2) = Apply (f e1) (f e2)
  rewrite f (OpRegion e  r ) = OpRegion (f e) (h r)
   where
    h EOR               = EOR
    h (NextOp op e' r') = NextOp op (f e') (h r')
  rewrite f (Let d b) = Let (map g d) b
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
  rewrite _ e                  = e
