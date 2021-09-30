module IR.IR
  ( Program(..)
  , Binder
  , Literal(..)
  , PrimOp(..)
  , Primitive(..)
  , Expr(..)
  , Alt(..)
  , collectApp
  , typeExpr
  , VarId(..)
  , DConId(..)
  ) where
import           Common.Identifiers             ( Binder
                                                , DConId(..)
                                                , VarId(..)
                                                )

-- | Top-level compilation unit.
data Program t = Program
  { programEntry :: VarId
  , programDefs  :: [(VarId, Expr t)]
  -- TODO: type defs.
  }

{- | Literal values supported by the language.

Note that these don't carry any connotation of type: '1' just means '1', 
-}
data Literal
  = LitIntegral Int
  | LitBool Bool

{- | Primitive operations.

These should be the kinds of functions one may expect to be available as
operators in C, or as instructions in an assembly language.

For simplicity and consistency, they should be:

- Pure (i.e., side-effectful iff operands are side-effectful, i.e., no '=')
- Strict in all operands (i.e., no '&&' or '||')

We can instead implement short-circuit control flow using match statements.
-}
data PrimOp
  = PrimNeg     -- ^ negation, i.e., -x
  | PrimNot     -- ^ logical not, i.e., !x
  | PrimBitNot  -- ^ bitwise not, i.e., ~x
  | PrimAdd     -- ^ addition, i.e., x + y
  | PrimSub     -- ^ subtraction, i.e., x - y
  | PrimMul     -- ^ multiplication, i.e., x * y
  | PrimDiv     -- ^ division, i.e., x / y
  | PrimMod     -- ^ modulus, i.e., x % y
  | PrimBitAnd  -- ^ bitwise-and, i.e., x & y
  | PrimBitOr   -- ^ bitwise-or, i.e., x | y
  | PrimEq      -- ^ equality, i.e., x == y
  | PrimGt      -- ^ greater than, i.e., x > y
  | PrimGe      -- ^ greater than or equal to, i.e., x >= y
  | PrimLt      -- ^ less than, i.e., x < y
  | PrimLe      -- ^ less than or equal to, i.e., x <= y

-- | Primitive functions for side-effects and imperative control flow.
data Primitive
  = New
  {- ^ 'New e t' allocates a schedule variable initialized to 'e', and
  returns a reference to it. If reuse token 't' is non-null,
  this will use token 't' instead of allocating new memory.
  -}
  | Dup
  {- ^ 'Dup r' duplicates the reference 'r' (i.e., increments its
  reference count.)
  -}
  | Drop
  {- ^ 'Drop r' brings reference 'r' out of scope, and frees it if it
  is the last remaining reference to the scheduled variable.
  -}
  | Reuse
  {- ^ 'Reuse r' is like 'Drop r', except it returns a reuse token
  that should be passed to 'New' for reuse.
  -}
  | Deref
  {- ^ 'Deref r' dereferences reference 'r' to obtain its value. -}
  | Assign
  {- ^ 'Assign r e' instantly assigns value 'e' to reference 'r'. -}
  | After
  {- ^ 'After t r e' assigns 'e' to reference 'r' after time 't'. -}
  | Fork
  {- ^ 'Fork ps+' forks processes in 'ps' concurrently. -}
  | Wait
  {- ^ 'Wait rs+' waits for an assignment to any reference in 'rs'. -}
  | Loop
  {- ^ 'Loop b' loops body 'b' forever. -}
  | Break
  {- ^ 'Break' breaks out of the innermost loop. -}
  | Return
  {- ^ 'Return' returns from the current function. -}
  | PrimOp PrimOp
  {- ^ Primitive operator. -}

{- | Expressions, based on the let-polymorphic lambda calculus.

't' represents the type of this expression. At various stages, this may
represent a richer or simpler type system. The type is embedded in each data
constructor so as to type-annotate the entire expression tree.

Designed for side effects with call-by-value evaluation order. Basic
sequencing can be recovered through let-bindings:

   let _ = stmt1 in
   let _ = stmt2 in
   ...

Effects of 'stmt1' take place before that of 'stmt2'.
-}
data Expr t
  = Var VarId t
  {- ^ 'Var n t' is a variable named 'n' of type 't'. -}
  | Data DConId t
  {- ^ 'Data d t' is a data constructor named 'n' of type 't'. -}
  | Lit Literal t
  {- ^ 'Lit l t' is a literal value 'l' of type 't'. -}
  | App (Expr t) (Expr t) t
  {- ^ 'App f a t' applies function 'f' to argument 'a', producing a value of
  type 't'.
  -}
  | Let [(Binder, Expr t)] (Expr t) t
  {- ^ 'Let [(n, v)] b t' binds value 'v' to variable 'v' in its body 'b'.

  The bindings list may only be of length greater than 1 for a set of mutually
  co-recursive functions.
  -}
  | Lambda Binder (Expr t) t
  {- ^ 'Lambda v b t' constructs an anonymous function of type 't' that binds
  a value to parameter 'v' in its body 'b'.
  -}
  | Match (Expr t) Binder [Alt t] t
  {- ^ 'Match s v alts t' pattern-matches on scrutinee 's' against alternatives
  'alts', each producing a value of type 't'. In the expression of each
  alternative, the value of 's' is bound to variable 'v'.
  -}
  | Prim Primitive [Expr t] t
  {- 'Prim p es t' applies primitive 'p' arguments 'es', producing a value of
  type 't'.
  -}

-- | An alternative in a pattern-match.
data Alt t
  = AltData DConId [Binder] (Expr t)
  {- ^ 'AltData d vs e' matches against data constructor 'd', producing
  expresison 'e', with dcon members bound to names 'vs' in 'e'.
  -}
  | AltLit Literal (Expr t)
  {- ^ 'AltLit l e' matches against literal 'd', producing expression 'e'.

  TODO: do we even need this? It seems like this is better suited to PrimEq
  applied to literals anyway.
  -}
  | AltDefault (Expr t)
  {- ^ 'AltDefault e' matches anything, producing expression 'e'. -}

-- | Collect a curried application into the function applied to a list of args.
collectApp :: Expr t -> (Expr t, [Expr t])
collectApp (App lhs rhs _) = (fn, args ++ [rhs])
  where (fn, args) = collectApp lhs
collectApp e = (e, [])

-- | Extract the type information embedded in an expression.
typeExpr :: Expr t -> t
typeExpr (Var  _ t     ) = t
typeExpr (Data _ t     ) = t
typeExpr (Lit  _ t     ) = t
typeExpr (Let    _ _ t ) = t
typeExpr (Lambda _ _ t ) = t
typeExpr (App    _ _ t ) = t
typeExpr (Match _ _ _ t) = t
typeExpr (Prim _ _ t   ) = t
