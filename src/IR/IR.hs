{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Sslang's intermediate representation and its associated helpers.
module IR.IR (
  Program (..),
  TypeDef (..),
  TypeVariant (..),
  Binder (..),
  Literal (..),
  PrimOp (..),
  Primitive (..),
  Expr (..),
  Alt (..),
  VarId (..),
  TConId (..),
  DConId (..),
  ExceptType (..),
  Type,
  Annotation,
  Annotations,
  variantFields,
  foldLambda,
  unfoldLambda,
  extract,
  inject,
  injectMore,
  foldApp,
  unfoldApp,
  isValue,
  altBinders,
  pattern BindVar,
  pattern BindAnon,
  binderToVar,
  Carrier,
) where

import Common.Identifiers (
  CSym (..),
  DConId (..),
  HasFreeVars (..),
  TConId (..),
  TVarId (..),
  VarId (..),
 )
import Data.Data (
  Data,
  Typeable,
 )

import Data.Maybe (
  catMaybes,
  mapMaybe,
  maybeToList,
 )
import Data.Set ((\\))
import qualified Data.Set as S
import IR.Types.Type (
  Annotation,
  Annotations,
  Type,
  pattern Arrow,
 )


{- | Top-level compilation unit.

@t@ is the type system in use, e.g., "IR.Types.Flat"
-}
data Program t = Program
  { programEntry :: VarId
  , cDefs :: String
  , externDecls :: [(VarId, Type)]
  , programDefs :: [(Binder t, Expr t)]
  , typeDefs :: [(TConId, TypeDef)]
  }
  deriving (Eq, Show, Typeable, Data, Functor)


{- | The type definition associated with a type constructor.
A definition for `data MyList a = Cons a (MyList a) | Nil` looks like:
@
  TypeDef { targs = [a]
          , [ ("Cons", VariantUnnamed [TVar a, TCon ("MyList" [TVar a])])
            , ("Nil", VariantUnnamed [])
            ]
          }
@
(Data constructors for identifiers are omitted for brevity.)
Note that for a flat type system, where all type constructors are nullary, targs
will just be set to [].
-}
data TypeDef = TypeDef
  { variants :: [(DConId, TypeVariant)]
  , targs :: [TVarId]
  }
  deriving (Show, Eq, Typeable, Data)


-- | Arguments to a data constructor, whose fields may or may not be named
data TypeVariant
  = -- | A record with named fields
    VariantNamed [(VarId, Type)]
  | -- | An algebraic type with unnamed fields
    VariantUnnamed [Type]
  deriving (Show, Eq, Typeable, Data)


-- | A name to be bound; 'Nothing' represents a wildcard, e.g., @let _ = ...@.
data Binder t = Binder {_binderId :: Maybe VarId, _binderType :: t}
  deriving (Show, Eq, Typeable, Data, Functor, Foldable, Traversable)


-- | An anonymous (wildcard) binder.
pattern BindAnon :: t -> Binder t
pattern BindAnon t = Binder{_binderId = Nothing, _binderType = t}


-- | A concrete, named binder.
pattern BindVar :: VarId -> t -> Binder t
pattern BindVar v t = Binder{_binderId = Just v, _binderType = t}


{-# COMPLETE BindAnon, BindVar #-}


{- | Literal values supported by the language.

Note that these don't carry any connotation of type: @1@ just means @1@,
-}
data Literal
  = LitIntegral Integer
  | LitEvent
  deriving (Eq, Show, Typeable, Data)


{- | Primitive operations.

These should be the kinds of functions one may expect to be available as
operators in C, or as instructions in an assembly language.

For simplicity and consistency, they should be:

- Pure (i.e., side-effectful iff operands are side-effectful, i.e., no @=@)
- Strict in all operands (i.e., no @&&@ or @||@)

We can instead implement short-circuit control flow using match statements.
-}
data PrimOp
  = -- | negation, i.e., -x
    PrimNeg
  | -- | logical not, i.e., !x
    PrimNot
  | -- | bitwise not, i.e., ~x
    PrimBitNot
  | -- | addition, i.e., x + y
    PrimAdd
  | -- | subtraction, i.e., x - y
    PrimSub
  | -- | multiplication, i.e., x * y
    PrimMul
  | -- | division, i.e., x / y
    PrimDiv
  | -- | modulus, i.e., x % y
    PrimMod
  | -- | bitwise-and, i.e., x & y
    PrimBitAnd
  | -- | bitwise-or, i.e., x | y
    PrimBitOr
  | -- | equality, i.e., x == y
    PrimEq
  | -- | equality, i.e., x != y
    PrimNeq
  | -- | greater than, i.e., x > y
    PrimGt
  | -- | greater than or equal to, i.e., x >= y
    PrimGe
  | -- | less than, i.e., x < y
    PrimLt
  | -- | less than or equal to, i.e., x <= y
    PrimLe
  deriving (Eq, Show, Typeable, Data)


-- | Primitive functions for side-effects and imperative control flow.
data Primitive
  = -- | @New e@ allocates a schedule variable initialized to @e@, and
    --  returns a reference to it.
    New
  | -- | @Dup r@ dups the reference @r@ and returns @r@.
    Dup
  | -- | @Drop e r@ evaluates to @e@, but also drops @r@.
    Drop
  | -- | @Deref r@ dereferences reference @r@ to obtain its value.
    Deref
  | -- | @Assign r e@ instantly assigns value @e@ to reference @r@.
    Assign
  | -- | @After t r e@ assigns @e@ to reference @r@ after time @t@.
    After
  | -- | @Par  es+@ evaluates expressions @es@ concurrently.
    Par
  | -- | @Wait rs+@ waits for an assignment to any reference in @rs@.
    Wait
  | -- | @Loop b@ loops body @b@ forever.
    Loop
  | -- | @Break@ breaks out of the innermost loop.
    Break
  | -- | @Now@ obtains the value of the current instant
    Now
  | -- | Inlined C expression code.
    PrimOp PrimOp
  | -- | Primitive operator.
    CQuote String
  | -- | Direct call to arbitrary C function (NOTE: HACKY).
    CCall CSym
  | -- | Call to well-typed extern symbol.
    FfiCall VarId
  deriving (Eq, Show, Typeable, Data)


{- | Expressions, based on the let-polymorphic lambda calculus.

@t@ represents the type of this expression, e.g., 'IR.Types.Flat'. At
various stages, this may represent a richer or simpler type
system. The type is embedded in each data constructor so as to
type-annotate the entire expression tree.

Designed for side effects with call-by-value evaluation order. Basic
sequencing can be recovered through let-bindings:

> let _ = stmt1 in
> let _ = stmt2 in
> ...

Effects of @stmt1@ take place before that of @stmt2@.
-}
data Expr t
  = -- | @Var n t@ is a variable named @n@ of type @t@.
    Var VarId t
  | -- | @Data d t@ is a data constructor named @d@ of type @t@.
    Data DConId t
  | -- | @Lit l t@ is a literal value @l@ of type @t@.
    Lit Literal t
  | -- | @App f a t@ applies function @f@ to argument @a@, producing a value of
    --  type @t@.
    App (Expr t) (Expr t) t
  | -- | @Let [(n, v)] b t@ binds value @v@ to variable @v@ in its body @b@.
    --
    --  The bindings list may only be of length greater than 1 for a set of mutually
    --  co-recursive functions.
    Let [(Binder t, Expr t)] (Expr t) t
  | -- | @Lambda v b t@ constructs an anonymous function of type @t@ that binds
    --  a value to parameter @v@ in its body @b@.
    Lambda (Binder t) (Expr t) t
  | -- | @Match s alts t@ pattern-matches on scrutinee @s@ against alternatives
    --  @alts@, each producing a value of type @t@.
    Match (Expr t) [(Alt t, Expr t)] t
  | -- | @Prim p es t@ applies primitive @p@ arguments @es@, producing a value
    --  of type @t@.
    Prim Primitive [Expr t] t
  | -- | @Exception et t@ produces a exception for the program.
    Exception ExceptType t
  deriving (Eq, Show, Typeable, Data, Functor, Foldable, Traversable)


-- | An alternative in a pattern-match.
data Alt t
  = -- | @AltData d vs@ matches data constructor @d@, and recursive patterns @alts@.
    AltData DConId [Alt t] t
  | -- | @AltLit l@ matches against literal @l@, producing expression @e@.
    AltLit Literal t
  | -- | @AltBinder v@ matches anything, and bound to name @v@.
    AltBinder (Binder t)
  deriving (Eq, Show, Typeable, Data, Functor, Foldable, Traversable)


newtype ExceptType
  = ExceptDefault Literal
  deriving (Eq, Show, Typeable, Data)


-- | The number of fields in a 'TypeVariant'.
variantFields :: TypeVariant -> Int
variantFields (VariantNamed fields) = length fields
variantFields (VariantUnnamed fields) = length fields


-- | Many data types carry other kinds of data, e.g., 'Expr' carries a type.
class Carrier c where
  -- | Extract the data carried by the carrier.
  extract :: c a -> a


  -- | Replace the data carried by the carrier.
  inject :: a -> c a -> c a


injectMore :: (Semigroup a, Carrier c) => a -> c a -> c a
injectMore a c = inject (a <> extract c) c


-- | Extract the type carried by an 'Expr'.
instance Carrier Expr where
  extract (Var _ t) = t
  extract (Data _ t) = t
  extract (Lit _ t) = t
  extract (Let _ _ t) = t
  extract (Lambda _ _ t) = t
  extract (App _ _ t) = t
  extract (Match _ _ t) = t
  extract (Prim _ _ t) = t
  extract (Exception _ t) = t


  inject t (Var v _) = Var v t
  inject t (Data d _) = Data d t
  inject t (Lit l _) = Lit l t
  inject t (Let ds b _) = Let ds b t
  inject t (Lambda xs b _) = Lambda xs b t
  inject t (App h a _) = App h a t
  inject t (Match s as _) = Match s as t
  inject t (Prim p es _) = Prim p es t
  inject t (Exception et _) = Exception et t


instance Carrier Alt where
  extract (AltData _ _ t) = t
  extract (AltLit _ t) = t
  extract (AltBinder b) = extract b
  inject t (AltData d a _) = AltData d a t
  inject t (AltLit l _) = AltLit l t
  inject t (AltBinder b) = AltBinder $ inject t b


instance Carrier Binder where
  extract Binder{_binderType = t} = t
  inject t b = b{_binderType = t}


{- | Collect a curried application into the function and argument list.

The type accompanying each argument represents type produced by the
application, and is extracted from the 'App' node that this function unwraps.

For example, the term @f a b@ (where @a: A@ and @b: B@) would be represented by
the following AST:
@@
(App (App (Var f (A -> B -> C)) (Var a A) (B -> C)) (Var b B) C)
@@

which, when unzipped, gives:

@@
(Var f (A -> B -> C)) [(Var a A, B -> C), (Var b B, C)]
@@

'unfoldApp' is the inverse of 'foldApp'.
-}
unfoldApp :: Expr t -> (Expr t, [(Expr t, t)])
unfoldApp (App lhs rhs t) =
  let (fn, args) = unfoldApp lhs in (fn, args ++ [(rhs, t)])
unfoldApp e = (e, [])


{- | Apply a function to zero or more arguments.

'foldApp' is the inverse of 'unfoldApp'.
-}
foldApp :: Expr t -> [(Expr t, t)] -> Expr t
foldApp = foldl $ \f (a, t) -> App f a t


-- | Collect a curried list of function arguments from a nesting of lambdas.
unfoldLambda :: Expr t -> ([Binder t], Expr t)
unfoldLambda (Lambda a b _) =
  let (as, body) = unfoldLambda b in (a : as, body)
unfoldLambda e = ([], e)


-- | Create a lambda chain given a list of argument-type pairs and a body.
foldLambda :: [Binder Type] -> Expr Type -> Expr Type
foldLambda args body = foldr chain body args
 where
  chain v@(Binder _ t) b = Lambda v b $ t `Arrow` extract b


-- | Whether an expression is a value.
isValue :: Expr t -> Bool
isValue Var{} = True
isValue Data{} = True
isValue Lit{} = True
isValue Lambda{} = True
isValue _ = False


-- | Retrieve list of binders from an 'Alt'.
altBinders :: Alt t -> [Binder t]
altBinders (AltLit _ _) = []
altBinders (AltBinder b) = [b]
altBinders (AltData _ as _) = concatMap altBinders as


binderToVar :: Binder a -> Maybe VarId
binderToVar (BindVar v _) = Just v
binderToVar _ = Nothing


instance HasFreeVars (Expr t) VarId where
  freeVars (Var v _) = S.singleton v
  freeVars Data{} = S.empty
  freeVars Lit{} = S.empty
  freeVars (App l r _) = freeVars l `S.union` freeVars r
  freeVars (Lambda (maybeToList . _binderId -> v) b _) = freeVars b \\ S.fromList v
  freeVars (Prim _ es _) = S.unions $ map freeVars es
  freeVars (Let (unzip -> (bs, ds)) b _) =
    S.unions (map freeVars $ b : ds) \\ S.fromList (catMaybes $ _binderId <$> bs)
  freeVars (Match s as _) = S.unions (freeVars s : map freeAltVars as)
   where
    freeAltVars :: (Alt t, Expr t) -> S.Set VarId
    freeAltVars (a, e) = freeVars e \\ S.fromList (mapMaybe _binderId $ altBinders a)
  freeVars Exception{} = S.empty
