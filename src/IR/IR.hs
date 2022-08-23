{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveTraversable #-}
-- | Sslang's intermediate representation and its associated helpers.
module IR.IR
  ( Program(..)
  , TypeDef(..)
  , TypeVariant(..)
  , Binder
  , Literal(..)
  , PrimOp(..)
  , Primitive(..)
  , Expr(..)
  , Alt(..)
  , VarId(..)
  , TConId(..)
  , DConId(..)
  , Type
  , Annotation
  , variantFields
  , wellFormed
  , foldLambda
  , unfoldLambda
  , extract
  , inject
  , zipApp
  , unzipApp
  ) where
import           Common.Identifiers             ( Binder
                                                , CSym(..)
                                                , DConId(..)
                                                , TConId(..)
                                                , TVarId(..)
                                                , VarId(..)
                                                )
import           Common.Pretty
import           Control.Monad                  ( void )
import           Data.Data                      ( Data
                                                , Typeable
                                                )

import           IR.Types.Type                  ( Annotation
                                                , pattern Arrow
                                                , Type
                                                , unfoldArrow
                                                )

{- | Top-level compilation unit.

@t@ is the type system in use, e.g., "IR.Types.Flat"
-}
data Program t = Program
  { programEntry :: VarId
  , cDefs        :: String
  , externDecls  :: [(VarId, Type)]
  , programDefs  :: [(VarId, Expr t)]
  , typeDefs     :: [(TConId, TypeDef)]
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
  , targs    :: [TVarId]
  }
  deriving (Show, Eq, Typeable, Data)

-- | Arguments to a data constructor, whose fields may or may not be named
data TypeVariant
  = VariantNamed [(VarId, Type)] -- ^ A record with named fields
  | VariantUnnamed [Type]        -- ^ An algebraic type with unnamed fields
  deriving (Show, Eq, Typeable, Data)


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
  | PrimNeq     -- ^ equality, i.e., x != y
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
  | Now
  {- ^ @Now@ obtains the value of the current instant -}
  | PrimOp PrimOp
  {- ^ Inlined C expression code. -}
  | CQuote String
  {- ^ Primitive operator. -}
  | CCall CSym
  {- ^ Direct call to arbitrary C function (NOTE: HACKY). -}
  | FfiCall VarId
  {- ^ Call to well-typed extern symbol. -}
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
  deriving (Eq, Show, Typeable, Data, Functor, Foldable, Traversable)

-- | An alternative in a pattern-match.
data Alt
  = AltData DConId [Binder]
  -- ^ @AltData d vs@ matches data constructor @d@, and names dcon members @vs@.
  | AltLit Literal
  -- ^ @AltLit l@ matches against literal @l@, producing expression @e@.
  | AltDefault Binder
  -- ^ @AltDefault v@ matches anything, and bound to name @v@.
  deriving (Eq, Show, Typeable, Data)

-- | Extract the type carried by an 'Expr'.
extract :: Expr t -> t
extract (Var  _ t    ) = t
extract (Data _ t    ) = t
extract (Lit  _ t    ) = t
extract (Let    _ _ t) = t
extract (Lambda _ _ t) = t
extract (App    _ _ t) = t
extract (Match  _ _ t) = t
extract (Prim   _ _ t) = t

-- | Replace the top-level type carried by an 'Expr'.
inject :: t -> Expr t -> Expr t
inject t (Var  v _      ) = Var v t
inject t (Data d _      ) = Data d t
inject t (Lit  l _      ) = Lit l t
inject t (Let    ds b  _) = Let ds b t
inject t (Lambda xs b  _) = Lambda xs b t
inject t (App    h  a  _) = App h a t
inject t (Match  s  as _) = Match s as t
inject t (Prim   p  es _) = Prim p es t

-- | The number of fields in a 'TypeVariant'.
variantFields :: TypeVariant -> Int
variantFields (VariantNamed   fields) = length fields
variantFields (VariantUnnamed fields) = length fields

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
unfoldLambda :: Expr t -> ([Binder], Expr t)
unfoldLambda (Lambda a b _) =
  let (as, body) = unfoldLambda b in (a : as, body)
unfoldLambda e = ([], e)

-- | Create a lambda chain given a list of argument-type pairs and a body.
foldLambda :: [(Binder, Type)] -> Expr Type -> Expr Type
foldLambda args body = foldr chain body args
  where chain (v, t) b = Lambda v b $ t `Arrow` extract b

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
  wfPrim New                  [_]       = True
  wfPrim Dup                  [_]       = True
  wfPrim Drop                 [_]       = True
  wfPrim Deref                [_]       = True
  wfPrim Assign               [_, _]    = True
  wfPrim After                [_, _, _] = True
  wfPrim Par                  (_ : _)   = True
  wfPrim Wait                 (_ : _)   = True
  wfPrim Break                []        = True
  wfPrim Now                  [_]       = True
  wfPrim (CCall   _         ) _         = True
  wfPrim (FfiCall _         ) [_]       = True
  wfPrim (PrimOp  PrimNeg   ) [_]       = True
  wfPrim (PrimOp  PrimNot   ) [_]       = True
  wfPrim (PrimOp  PrimBitNot) [_]       = True
  wfPrim (PrimOp  PrimAdd   ) [_, _]    = True
  wfPrim (PrimOp  PrimSub   ) [_, _]    = True
  wfPrim (PrimOp  PrimMul   ) [_, _]    = True
  wfPrim (PrimOp  PrimDiv   ) [_, _]    = True
  wfPrim (PrimOp  PrimMod   ) [_, _]    = True
  wfPrim (PrimOp  PrimBitAnd) [_, _]    = True
  wfPrim (PrimOp  PrimBitOr ) [_, _]    = True
  wfPrim (PrimOp  PrimEq    ) [_, _]    = True
  wfPrim (PrimOp  PrimGt    ) [_, _]    = True
  wfPrim (PrimOp  PrimGe    ) [_, _]    = True
  wfPrim (PrimOp  PrimLt    ) [_, _]    = True
  wfPrim (PrimOp  PrimLe    ) [_, _]    = True
  wfPrim _                    _         = False

{- | Pretty Typeclass: pretty print the IR

Adds 
* indentation and line breaks
* some parens (not minimal parens, but fewer than around every node)
Omits 
* let _ =
* type annotations
Reverts
* curried funcs of one arg back to multiple arg funcs
-}
instance Pretty (Program Type) where
  pretty Program { programDefs = ds, typeDefs = tys, externDecls = xds } =
    vsep $ punctuate line tops
   where
    tops =
      map prettyTypDef tys ++ map prettyExternDecl xds ++ map prettyFuncDef ds

    -- Generates readable Doc representation of an IR Top Level Function
    prettyFuncDef :: (VarId, Expr Type) -> Doc ann
    prettyFuncDef (v, l@(Lambda _ _ ty)) =
      pretty v <+> typSig <+> pretty "=" <+> line <> indent
        2
        (pretty (void body))
     where
      typSig = hsep args <+> rarrow <+> pretty retTy
      args   = zipWith
        (\arg t -> parens $ pretty arg <+> pretty ":" <+> pretty t)
        argIds
        argTys
      (argIds, body ) = unfoldLambda l
      (argTys, retTy) = unfoldArrow ty -- FIXME
    prettyFuncDef (v, e) = pretty v <+> pretty "=" <+> pretty (void e)

    prettyExternDecl :: (Pretty t) => (VarId, t) -> Doc ann
    prettyExternDecl (v, t) =
      pretty "extern" <+> pretty v <+> colon <+> pretty t

    -- Generates readable Doc representation of an IR Type Definition
    prettyTypDef :: (TConId, TypeDef) -> Doc ann
    prettyTypDef (tcon, TypeDef { variants = vars }) =
      pretty "type"
        <+> pretty tcon
        <+> line
        <>  indent indentNo (vsep $ map prettyDCon vars)
        <>  line
    prettyDCon :: (DConId, TypeVariant) -> Doc ann
    prettyDCon (dcon, VariantNamed argz) =
      pretty dcon <+> hsep (pretty . snd <$> argz)
    prettyDCon (dcon, VariantUnnamed argz) =
      pretty dcon <+> hsep (pretty <$> argz)


-- | Pretty prints IR Expr nodes without type annotations
instance Pretty (Expr ()) where
  pretty a@App{} = pretty nm <+> hsep (parenz . fst <$> args)
   where
    (nm, args) = unzipApp a
    -- insert (usually) necessary parens
    parenz :: Expr () -> Doc ann
    parenz v@(Var _ _) = pretty v  -- variables
    parenz l@(Lit _ _) = pretty l  -- literals
    parenz e           = parens (pretty e)
    -- TODO: minimum parens algo
  pretty (Prim Wait es _           ) = pretty "wait" <+> vsep (map pretty es)
  pretty (Var v _                  ) = pretty v
  pretty (Lambda a              b _) = pretty "fun" <+> pretty a <+> pretty b
  pretty (Let    [(Nothing, e)] b _) = pretty e <> line <> pretty b
  pretty (Let    as             b _) = letexpr
   where
    letexpr = pretty "let" <+> vsep (map def as) <> line <> pretty b
    def (Just v , e) = pretty v <+> equals <+> align (pretty e)
    def (Nothing, e) = pretty '_' <+> equals <+> align (pretty e)
  pretty (Prim After [d, l, r] _) = ae
   where
    ae =
      pretty "after" <+> pretty d <> comma <+> pretty l <+> larrow <+> pretty r
  pretty (Prim  Assign [l, r] _) = parens $ pretty l <+> larrow <+> pretty r
  pretty (Match s      as     _) = pretty "match" <+> pretty s <> line <> arms
   where
    arms = vsep (map (indent indentNo . arm) as)
    arm :: (Alt, Expr ()) -> Doc ann
    arm (a, e) = pretty a <+> equals <+> align (pretty e)
  pretty (Prim Loop [b] _) =
    pretty "loop" <> line <> indent indentNo (pretty b)
  pretty (Prim (PrimOp po) [l, r] _) = pretty l <+> pretty po <+> pretty r
  pretty (Data d _                 ) = pretty d
  pretty (Lit  l _                 ) = pretty l
  pretty (Prim New   [r] _         ) = pretty "new" <+> pretty r
  pretty (Prim Dup   [r] _         ) = pretty "__dup" <+> pretty r
  pretty (Prim Drop  [r] _         ) = pretty "__drop" <+> pretty r
  pretty (Prim Deref [r] _         ) = pretty "deref" <+> pretty r
  pretty (Prim Par es _) = pretty "par" <+> block dbar (map pretty es)
  pretty (Prim Break []  _         ) = pretty "break"
  pretty (Prim (CCall s) es _) =
    pretty "$" <> pretty s <> parens (hsep $ punctuate comma $ map pretty es)
  pretty (Prim (FfiCall s) es _) = pretty s <+> hsep (map (parens . pretty) es)
  pretty (Prim (CQuote s) [] _) = pretty "$$" <> pretty s <> pretty "$$"

  -- pretty (Prim Return [e] _        ) = pretty "return" <+> braces (pretty e)
  pretty (Prim p _ _) = error "Primitive expression not well-formed: " $ show p

instance Pretty Alt where
  pretty (AltData a b        ) = pretty a <+> hsep (map pretty b)
  pretty (AltLit     a       ) = pretty a
  pretty (AltDefault (Just v)) = pretty v
  pretty (AltDefault Nothing ) = pretty '_'

instance Pretty Literal where
  pretty (LitIntegral i) = pretty $ show i
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
  pretty PrimNeq    = pretty "!="
  pretty PrimGt     = pretty ">"
  pretty PrimGe     = pretty ">="
  pretty PrimLt     = pretty "<"
  pretty PrimLe     = pretty "<="

-- | Dumpy Typeclass: generate comprehensive Doc representation of the IR
instance Dumpy (Program Type) where
  dumpy Program { programDefs = ds, typeDefs = tys, externDecls = xds } =
    vsep $ punctuate line tops
   where
    tops =
      map dumpyTypDef tys ++ map dumpyExternDecl xds ++ map dumpyFuncDef ds

    -- Generates readable Doc representation of an IR Top Level Function
    dumpyFuncDef :: (VarId, Expr Type) -> Doc ann
    dumpyFuncDef (v, l@(Lambda _ _ ty)) =
      pretty v <+> typSig <+> pretty "=" <+> line <> indent 2 (dumpy body)
     where
      typSig = hsep args <+> rarrow <+> dumpy retTy
      args   = zipWith
        (\arg t -> parens $ pretty arg <+> pretty ":" <+> dumpy t)
        argIds
        argTys
      (argIds, body ) = unfoldLambda l
      (argTys, retTy) = unfoldArrow ty
    dumpyFuncDef (v, e) = pretty v <+> pretty "=" <+> dumpy e

    -- Generates readable Doc representation of an IR Type Definition
    dumpyExternDecl :: Dumpy t => (VarId, t) -> Doc ann
    dumpyExternDecl (v, t) = pretty "extern" <+> pretty v <+> colon <+> dumpy t

    -- Generates readable Doc representation of an IR Type Definition
    dumpyTypDef :: (TConId, TypeDef) -> Doc ann
    dumpyTypDef (tcon, TypeDef { variants = vars }) =
      pretty "type"
        <+> pretty tcon
        <+> line
        <>  indent indentNo (vsep $ map dumpyDCon vars)
        <>  line
    dumpyDCon :: (DConId, TypeVariant) -> Doc ann
    dumpyDCon (dcon, VariantNamed argz) =
      pretty dcon <+> hsep (dumpy . snd <$> argz)
    dumpyDCon (dcon, VariantUnnamed argz) =
      pretty dcon <+> hsep (dumpy <$> argz)
  -- TODO: how to represent entry point?

instance Dumpy (Expr Type) where
  dumpy (Var  v t  ) = typeAnn t $ pretty v
  dumpy (Data d t  ) = typeAnn t $ pretty d
  dumpy (Lit  l t  ) = typeAnn t $ dumpy l
  dumpy (App l  r t) = typeAnn t $ dumpy l <+> dumpy r
  dumpy (Let as b t) = typeAnn t $ parens letexpr
   where
    letexpr = pretty "let" <+> block dbar (map def as) <> semi <+> dumpy b
    def (Just v , e) = pretty v <+> equals <+> braces (dumpy e)
    def (Nothing, e) = pretty '_' <+> equals <+> braces (dumpy e)
  dumpy (Lambda a b t) =
    typeAnn t $ pretty "fun" <+> pretty a <+> braces (dumpy b)
  dumpy (Match s as t) = typeAnn t $ pretty "match" <+> dumpy s <+> arms
   where
    -- Where to add binder?
    arms = block bar (map arm as)
    arm (a, e) = dumpy a <+> pretty "=" <+> braces (dumpy e)
  dumpy (Prim New   [r] t) = typeAnn t $ pretty "new" <+> dumpy r
  dumpy (Prim Dup   [r] t) = typeAnn t $ pretty "__dup" <+> dumpy r
  dumpy (Prim Drop  [r] t) = typeAnn t $ pretty "__drop" <+> dumpy r
  dumpy (Prim Deref [r] t) = typeAnn t $ pretty "deref" <+> dumpy r
  dumpy (Prim Assign [l, r] t) =
    typeAnn t $ parens $ dumpy l <+> larrow <+> braces (dumpy r)
  dumpy (Prim After [d, l, r] t) = typeAnn t $ parens ae
   where
    ae = pretty "after" <+> dumpy d <> comma <+> dumpy l <+> larrow <+> braces
      (dumpy r)
  dumpy (Prim Par es t) =
    typeAnn t $ pretty "par" <+> block dbar (map dumpy es)
  dumpy (Prim Wait es t) =
    typeAnn t $ pretty "wait" <+> block dbar (map dumpy es)
  dumpy (Prim Loop      [b] t) = typeAnn t $ pretty "loop" <+> braces (dumpy b)
  dumpy (Prim Break     []  t) = typeAnn t $ pretty "break"
  dumpy (Prim (CCall s) es  t) = typeAnn t $ pretty "$" <> pretty s <> parens
    (hsep $ punctuate comma $ map dumpy es)
  dumpy (Prim (CQuote s) [] t) =
    typeAnn t $ pretty "$$" <> pretty s <> pretty "$$"
  dumpy (Prim (FfiCall s) es _) = pretty s <+> hsep (map (parens . dumpy) es)
  dumpy (Prim (PrimOp po) [l, r] t) =
    typeAnn t $ dumpy l <+> dumpy po <+> dumpy r
  dumpy (Prim p _ _) = error "Primitive expression not well-formed: " $ show p

instance Dumpy Alt where
  dumpy (AltData a b        ) = parens $ pretty a <+> hsep (map pretty b)
  dumpy (AltLit     a       ) = pretty a
  dumpy (AltDefault (Just v)) = pretty v
  dumpy (AltDefault Nothing ) = pretty '_'

instance Dumpy Literal where
  dumpy (LitIntegral i) = pretty $ show i
  dumpy LitEvent        = pretty "()"

instance Dumpy PrimOp where
  dumpy PrimNeg    = pretty "-"
  dumpy PrimNot    = pretty "!"
  dumpy PrimBitNot = pretty "~"
  dumpy PrimAdd    = pretty "+"
  dumpy PrimSub    = pretty "-"
  dumpy PrimMul    = pretty "*"
  dumpy PrimDiv    = pretty "/"
  dumpy PrimMod    = pretty "%"
  dumpy PrimBitAnd = pretty "&"
  dumpy PrimBitOr  = pretty "|"
  dumpy PrimEq     = pretty "=="
  dumpy PrimNeq    = pretty "!="
  dumpy PrimGt     = pretty ">"
  dumpy PrimGe     = pretty ">="
  dumpy PrimLt     = pretty "<"
  dumpy PrimLe     = pretty "<="
