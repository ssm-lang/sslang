{-# LANGUAGE DeriveDataTypeable #-}
-- | Sslang's intermediate representation and its associated helpers.
module IR.IR
  ( Program(..)
  , Binder
  , Literal(..)
  , PrimOp(..)
  , Primitive(..)
  , Expr(..)
  , Alt(..)
  , VarId(..)
  , TConId(..)
  , DConId(..)
  , wellFormed
  , collectLambda
  , makeLambdaChain
  , extract
  , zipApp
  , unzipApp
  ) where
import           Common.Identifiers             ( Binder
                                                , DConId(..)
                                                , TConId(..)
                                                , VarId(..)
                                                )
import           Common.Pretty
import           IR.Types.TypeSystem            ( TypeDef(..)
                                                , TypeSystem
                                                , arrow
                                                )

import           Control.Comonad                ( Comonad(..) )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Data                      ( Data
                                                , Typeable
                                                )

{- | Top-level compilation unit.

@t@ is the type system in use, e.g., "IR.Types.Flat"
-}
data Program t = Program
  { programEntry :: VarId
  , programDefs  :: [(VarId, Expr t)]
  , typeDefs     :: [(TConId, TypeDef t)]
  }
  deriving (Eq, Show, Typeable, Data)

{- | Literal values supported by the language.

Note that these don't carry any connotation of type: @1@ just means @1@,
-}
data Literal
  = LitIntegral Integer
  | LitBool Bool
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
  deriving (Eq, Show, Typeable, Data)

-- | Primitive functions for side-effects and imperative control flow.
data Primitive
  = New
  {- ^ @New e@ allocates a schedule variable initialized to @e@, and
  returns a reference to it.
  -}
  | Dup
  {- ^ @Dup r@ duplicates the reference @r@ (i.e., increments its
  reference count.)
  -}
  | Drop
  {- ^ @Drop r@ brings reference @r@ out of scope, and frees it if it
  is the last remaining reference to the scheduled variable.
  -}
  | Deref
  {- ^ @Deref r@ dereferences reference @r@ to obtain its value. -}
  | Assign
  {- ^ @Assign r e@ instantly assigns value @e@ to reference @r@. -}
  | After
  {- ^ @After t r e@ assigns @e@ to reference @r@ after time @t@. -}
  | Par
  {- ^ @Par  es+@ evaluates expressions @es@ concurrently. -}
  | Wait
  {- ^ @Wait rs+@ waits for an assignment to any reference in @rs@. -}
  | Loop
  {- ^ @Loop b@ loops body @b@ forever. -}
  | Break
  {- ^ @Break@ breaks out of the innermost loop. -}
  | Return
  {- ^ @Return e@ returns the value @e@ from the current function. -}
  | PrimOp PrimOp
  {- ^ Primitive operator. -}
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
  = Var VarId t
  {- ^ @Var n t@ is a variable named @n@ of type @t@. -}
  | Data DConId t
  {- ^ @Data d t@ is a data constructor named @d@ of type @t@. -}
  | Lit Literal t
  {- ^ @Lit l t@ is a literal value @l@ of type @t@. -}
  | App (Expr t) (Expr t) t
  {- ^ @App f a t@ applies function @f@ to argument @a@, producing a value of
  type @t@.
  -}
  | Let [(Binder, Expr t)] (Expr t) t
  {- ^ @Let [(n, v)] b t@ binds value @v@ to variable @v@ in its body @b@.

  The bindings list may only be of length greater than 1 for a set of mutually
  co-recursive functions.
  -}
  | Lambda Binder (Expr t) t
  {- ^ @Lambda v b t@ constructs an anonymous function of type @t@ that binds
  a value to parameter @v@ in its body @b@.
  -}
  | Match (Expr t) [(Alt, Expr t)] t
  {- ^ @Match s alts t@ pattern-matches on scrutinee @s@ against alternatives
  @alts@, each producing a value of type @t@.
  -}
  | Prim Primitive [Expr t] t
  {- ^ @Prim p es t@ applies primitive @p@ arguments @es@, producing a value
  of type @t@.
  -}
  deriving (Eq, Show, Typeable, Data)

-- | An alternative in a pattern-match.
data Alt
  = AltData DConId [Binder]
  -- ^ @AltData d vs@ matches data constructor @d@, and names dcon members @vs@.
  | AltLit Literal
  -- ^ @AltLit l@ matches against literal @l@, producing expression @e@.
  | AltDefault Binder
  -- ^ @AltDefault v@ matches anything, and bound to name @v@.
  deriving (Eq, Show, Typeable, Data)

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

'unzipApp' is the inverse of 'zipApp'.
-}
unzipApp :: Expr t -> (Expr t, [(Expr t, t)])
unzipApp (App lhs rhs t) =
  let (fn, args) = unzipApp lhs in (fn, args ++ [(rhs, t)])
unzipApp e = (e, [])

{- | Apply a function to zero or more arguments.

'zipApp' is the inverse of 'unzipApp'.
-}
zipApp :: Expr t -> [(Expr t, t)] -> Expr t
zipApp = foldr $ \(a, t) f -> App f a t

-- | Collect a curried list of function arguments from a nesting of lambdas.
collectLambda :: Expr t -> ([Binder], Expr t)
collectLambda (Lambda a b _) =
  let (as, body) = collectLambda b in (a : as, body)
collectLambda e = ([], e)

-- | Create a lambda chain given a list of argument-type pairs and a body.
makeLambdaChain :: TypeSystem t => [(Binder, t)] -> Expr t -> Expr t
makeLambdaChain args body = foldr chain body args
  where chain (v, t) b = Lambda v b $ t `arrow` extract b

instance Functor Program where
  fmap f Program { programEntry = e, programDefs = defs, typeDefs = tds } =
    Program { programEntry = e
            , programDefs  = fmap (second $ fmap f) defs
            , typeDefs     = fmap (second $ fmap f) tds
            }

instance Functor Expr where
  fmap f (Var  v t      ) = Var v (f t)
  fmap f (Data d t      ) = Data d (f t)
  fmap f (Lit  l t      ) = Lit l (f t)
  fmap f (App    l  r  t) = App (fmap f l) (fmap f r) (f t)
  fmap f (Let    xs b  t) = Let (fmap (second $ fmap f) xs) (fmap f b) (f t)
  fmap f (Lambda v  b  t) = Lambda v (fmap f b) (f t)
  fmap f (Match  s  as t) = Match (fmap f s) (fmap (second $ fmap f) as) (f t)
  fmap f (Prim   p  as t) = Prim p (fmap (fmap f) as) (f t)

instance Comonad Expr where
  extract (Var  _ t    ) = t
  extract (Data _ t    ) = t
  extract (Lit  _ t    ) = t
  extract (Let    _ _ t) = t
  extract (Lambda _ _ t) = t
  extract (App    _ _ t) = t
  extract (Match  _ _ t) = t
  extract (Prim   _ _ t) = t
  extend f e@(Var  i _) = Var i (f e)
  extend f e@(Data i _) = Data i (f e)
  extend f e@(Lit  l _) = Lit l (f e)
  extend f e@(Let xs b _) =
    Let (fmap (second $ extend f) xs) (extend f b) (f e)
  extend f e@(Lambda v b _) = Lambda v (extend f b) (f e)
  extend f e@(App    l r _) = App (extend f l) (extend f r) (f e)
  extend f e@(Match s as _) =
    Match (extend f s) (fmap (second $ extend f) as) (f e)
  extend f e@(Prim p es _) = Prim p (fmap (extend f) es) (f e)

instance Foldable Expr where
  foldMap f (Var  _ t ) = f t
  foldMap f (Data _ t ) = f t
  foldMap f (Lit  _ t ) = f t
  foldMap f (App l r t) = foldMap f l <> foldMap f r <> f t
  foldMap f (Let xs b t) =
    mconcat (map (foldMap f . snd) xs) <> foldMap f b <> f t
  foldMap f (Lambda _ b t) = foldMap f b <> f t
  foldMap f (Match s as t) =
    foldMap f s <> mconcat (map (foldMap f . snd) as) <> f t
  foldMap f (Prim _ es t) = mconcat (map (foldMap f) es) <> f t

{- | Predicate of whether an expression "looks about right".

Description left deliberately vague so that we have the flexibility to
strengthen this predicate. For now, we just check that all primitives are
applied to the right number of arguments.

This predicate also provides a template to recursively traverse through all
sub-expressions of an expression.
-}
wellFormed :: Expr t -> Bool
wellFormed (Var  _ _          ) = True
wellFormed (Data _ _          ) = True
wellFormed (Lit  _ _          ) = True
wellFormed (Let    defs b    _) = all (wellFormed . snd) defs && wellFormed b
wellFormed (Lambda _    b    _) = wellFormed b
wellFormed (App    f    a    _) = wellFormed f && wellFormed a
wellFormed (Match  s    alts _) = wellFormed s && all (wellFormed . snd) alts
wellFormed (Prim   p    es   _) = wfPrim p es && all wellFormed es
 where
  wfPrim New                 [_]       = True
  wfPrim Dup                 [_]       = True
  wfPrim Drop                [_]       = True
  wfPrim Deref               [_]       = True
  wfPrim Assign              [_, _]    = True
  wfPrim After               [_, _, _] = True
  wfPrim Par                 (_ : _)   = True
  wfPrim Wait                (_ : _)   = True
  wfPrim Break               []        = True
  wfPrim Return              [_]       = True
  wfPrim (PrimOp PrimNeg   ) [_]       = True
  wfPrim (PrimOp PrimNot   ) [_]       = True
  wfPrim (PrimOp PrimBitNot) [_]       = True
  wfPrim (PrimOp PrimAdd   ) [_, _]    = True
  wfPrim (PrimOp PrimSub   ) [_, _]    = True
  wfPrim (PrimOp PrimMul   ) [_, _]    = True
  wfPrim (PrimOp PrimDiv   ) [_, _]    = True
  wfPrim (PrimOp PrimMod   ) [_, _]    = True
  wfPrim (PrimOp PrimBitAnd) [_, _]    = True
  wfPrim (PrimOp PrimBitOr ) [_, _]    = True
  wfPrim (PrimOp PrimEq    ) [_, _]    = True
  wfPrim (PrimOp PrimGt    ) [_, _]    = True
  wfPrim (PrimOp PrimGe    ) [_, _]    = True
  wfPrim (PrimOp PrimLt    ) [_, _]    = True
  wfPrim (PrimOp PrimLe    ) [_, _]    = True
  wfPrim _                   _         = False

instance Pretty t => Pretty (Program t) where
  pretty Program { programDefs = ds } = vsep $ map pretty ds
  -- TODO: type defs
  -- TODO: how to represent entry point?

instance Pretty t => Pretty (Expr t) where
  pretty (Var  v t  ) = typeAnn t $ pretty v
  pretty (Data d t  ) = typeAnn t $ pretty d
  pretty (Lit  l t  ) = typeAnn t $ pretty l
  pretty (App l  r t) = typeAnn t $ pretty l <+> pretty r
  pretty (Let as b t) = typeAnn t $ parens letexpr
   where
    letexpr = pretty "let" <+> block dbar (map def as) <> semi <+> pretty b
    def (Just v , e) = pretty v <+> equals <+> braces (pretty e)
    def (Nothing, e) = pretty '_' <+> equals <+> braces (pretty e)
  pretty (Lambda a b t) =
    typeAnn t $ pretty "fun" <+> pretty a <+> braces (pretty b)
  pretty (Match s as t) = typeAnn t $ pretty "match" <+> pretty s <+> arms
   where
    -- Where to add binder?
    arms = block bar (map arm as)
    arm (a, e) = pretty a <+> drarrow <+> braces (pretty e)
  pretty (Prim New   [r] t) = typeAnn t $ pretty "new" <+> pretty r
  pretty (Prim Dup   [r] t) = typeAnn t $ pretty "__dup" <+> pretty r
  pretty (Prim Drop  [r] t) = typeAnn t $ pretty "__drop" <+> pretty r
  pretty (Prim Deref [r] t) = typeAnn t $ pretty "deref" <+> pretty r
  pretty (Prim Assign [l, r] t) =
    typeAnn t $ parens $ pretty l <+> pretty "<-" <+> braces (pretty r)
  pretty (Prim After [d, l, r] t) = typeAnn t $ parens ae
   where
    ae =
      pretty "after" <+> pretty d <> comma <+> pretty l <+> larrow <+> braces
        (pretty r)
  pretty (Prim Par es t) =
    typeAnn t $ pretty "par" <+> block dbar (map pretty es)
  pretty (Prim Wait es t) =
    typeAnn t $ pretty "wait" <+> block dbar (map pretty es)
  pretty (Prim Loop  [b] t) = typeAnn t $ pretty "loop" <+> braces (pretty b)
  pretty (Prim Break []  t) = typeAnn t $ pretty "break"
  pretty (Prim Return [e] t) =
    typeAnn t $ pretty "return" <+> braces (pretty e)
  pretty (Prim (PrimOp po) [l, r] t) =
    parens $ pretty l <+> pretty po <+> pretty r <> pretty t
  pretty (Prim p _ _) = error "Primitive expression not well-formed: " $ show p

instance Pretty Alt where
  pretty (AltData a b        ) = parens $ pretty a <+> hsep (map pretty b)
  pretty (AltLit     a       ) = pretty a
  pretty (AltDefault (Just v)) = pretty v
  pretty (AltDefault Nothing ) = pretty '_'

instance Pretty Literal where
  pretty (LitIntegral i) = pretty $ show i
  pretty (LitBool     b) = pretty $ show b
  pretty LitEvent        = pretty "()"

instance Pretty PrimOp where
  pretty PrimNeg    = pretty "-"
  pretty PrimNot    = pretty "!"
  pretty PrimBitNot = pretty "~"
  pretty PrimAdd    = pretty "+"
  pretty PrimSub    = pretty "-"
  pretty PrimMul    = pretty "*"
  pretty PrimDiv    = pretty "/"
  pretty PrimMod    = pretty "%"
  pretty PrimBitAnd = pretty "&"
  pretty PrimBitOr  = pretty "|"
  pretty PrimEq     = pretty "=="
  pretty PrimGt     = pretty ">"
  pretty PrimGe     = pretty ">="
  pretty PrimLt     = pretty "<"
  pretty PrimLe     = pretty "<="
