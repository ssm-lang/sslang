{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Definitions of and related to the sslang IR's type system.
module IR.Types.Type where

import           Common.Identifiers             ( DConId(..)
                                                , HasFreeVars(..)
                                                , Identifiable
                                                , TConId(..)
                                                , TVarId(..)
                                                , fromId
                                                , fromString
                                                )
import           Common.Pretty                  ( (<+>)
                                                , Dumpy(..)
                                                , Pretty(pretty)
                                                , brackets
                                                , comma
                                                , hsep
                                                , parens
                                                )

import           Data.Foldable                  ( toList )
import           Data.Generics                  ( Data
                                                , Typeable
                                                )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Set                       ( (\\) )


{- | Encoding of sslang types.

Structurally speaking, these are very simple. Types are either type variables or
type constructors applied to some other types.

Builtin types (and type constructors) include 'Arrow', 'Unit', 'Ref', 'List',
and various sizes of tuples; for convenience, those are defined elsewhere using
the GHC PatternSynonyms extension.
-}
data Type
  = TCon TConId [Type]    -- ^ Type constructor, applied to zero or more types
  | TVar TVarId           -- ^ Type variable (may be implicitly quantified)
  deriving (Eq, Show, Typeable, Data)

instance HasFreeVars Type TVarId where
  freeVars (TCon _ ts) = S.unions $ map freeVars ts
  freeVars Hole        = S.empty
  freeVars (TVar v)    = S.singleton v

{- | Constraints on a type scheme.

For now, we only support trivial constraints.
-}
data Constraint = CTrue -- ^ The trivial constraint, i.e., always satisfied.
  deriving (Eq, Show, Typeable, Data)

{- | Schemes quantify over 'Type' variables and impose some 'Constraint'.

'SchemeOf' is implemented as functor over some kind of type so we can easily
substitute in 'Type' vs 'UType' when performing type inference/unification.
-}
data SchemeOf t = Forall (S.Set TVarId) Constraint t
  deriving (Eq, Show, Functor, Foldable, Traversable, Typeable, Data)

-- | Unwrap a scheme and obtain the underlying type.
unScheme :: SchemeOf t -> t
unScheme (Forall _ _ t) = t

-- | Construct a scheme with quantified type variables and a trivial constraint.
forall :: (Functor l, Foldable l) => l TVarId -> t -> SchemeOf t
forall vs = Forall (S.fromList $ toList $ fmap fromId vs) CTrue

-- | Construct a scheme from all free type variables and a trivial constraint.
schemeOf :: Type -> Scheme
schemeOf t = Scheme $ forall (S.toList $ freeVars t) t

-- | Schemes over 'Type'.
newtype Scheme = Scheme (SchemeOf Type)
  deriving (Eq, Show, Typeable, Data)

instance HasFreeVars Scheme TVarId where
  freeVars (Scheme (Forall s _ t)) = freeVars t \\ s

-- | An annotation records the annotated portion of a pattern.
data Annotation
  = AnnType Type                        -- ^ A basic 'Type' annotation
  | AnnDCon DConId [Annotation]         -- ^ Annotations collected from patterns
  | AnnArrows [Annotation] Annotation   -- ^ Annotations collected from fun args
  deriving (Eq, Show, Typeable, Data)

-- | Expressions are annotated with a (potentially empty) list of 'Annotation'.
newtype Annotations = Annotations [Annotation]
  deriving (Eq, Show, Typeable, Data, Semigroup, Monoid)

-- | Unwrap the 'Annotations' data constructor.
unAnnotations :: Annotations -> [Annotation]
unAnnotations (Annotations as) = as

-- | Unroll an annotation into a type.
-- FIXME: get rid of this.
fromAnnotations :: Annotations -> Type
fromAnnotations = go . unAnnotations
 where
  go (AnnType t : _   ) = t
  go (_         : anns) = go anns
  go []                 = Hole -- TODO: properly unroll

-- | Some data type that contains a sslang 'Type'.
class HasType a where
  getType :: a -> Type

instance HasType Type where
  getType = id

instance HasType Scheme where
  getType (Scheme (Forall _ _ t)) = t

-- | A fresh, free type variable; may appear in type annotations.
pattern Hole :: Type
pattern Hole = TVar "_"

-- | The type constructor for function arrows.
pattern Arrow :: Type -> Type -> Type
pattern Arrow a b = TCon "->" [a, b]

-- | Unfold an 'Arrow' 'Type' into a list of argument types and a return type.
unfoldArrow :: Type -> ([Type], Type)
unfoldArrow (Arrow a b) = let (as, rt) = unfoldArrow b in (a : as, rt)
unfoldArrow t           = ([], t)

-- | Fold a list of argument types and a return type into an 'Arrow' 'Type'.
foldArrow :: ([Type], Type) -> Type
foldArrow (a : as, rt) = a `Arrow` foldArrow (as, rt)
foldArrow ([]    , t ) = t

-- | The builtin singleton 'Type', whose only data constructor is just @()@.
pattern Unit :: Type
pattern Unit = TCon "()" []

-- | The builtin reference 'Type', created using @new@.
pattern Ref :: Type -> Type
pattern Ref a = TCon "&" [a]

-- | The builtin list 'Type', created using list syntax, e.g., @[a, b]@.
pattern List :: Type -> Type
pattern List a = TCon "[]" [a]

-- | The builtin 64-bit timestamp 'Type'.
pattern Time :: Type
pattern Time = TCon "Time" []

-- | Builtin 'Type' for signed 64-bit integers.
pattern I64 :: Type
pattern I64 = TCon "Int64" []

-- | Builtin 'Type' for unsigned 64-bit integers.
pattern U64 :: Type
pattern U64 = TCon "UInt64" []

-- | Builtin 'Type' for signed 32-bit integers.
pattern I32 :: Type
pattern I32 = TCon "Int32" []

-- | Builtin 'Type' for unsigned 32-bit integers.
pattern U32 :: Type
pattern U32 = TCon "UInt32" []

-- | Builtin 'Type' for signed 16-bit integers.
pattern I16 :: Type
pattern I16 = TCon "Int16" []

-- | Builtin 'Type' for unsigned 16-bit integers.
pattern U16 :: Type
pattern U16 = TCon "UInt16" []

-- | Builtin 'Type' for signed 8-bit integers.
pattern I8 :: Type
pattern I8 = TCon "Int8" []

-- | Builtin 'Type' for unsigned 8-bit integers.
pattern U8 :: Type
pattern U8 = TCon "UInt8" []

-- | Test whether a 'Type' is one of the builtin signed integers.
isInt :: Type -> Bool
isInt I64 = True
isInt I32 = True
isInt I16 = True
isInt I8  = True
isInt _   = False

-- | Test whether a 'Type' is one of the builtin unsigned integers.
isUInt :: Type -> Bool
isUInt U64 = True
isUInt U32 = True
isUInt U16 = True
isUInt U8  = True
isUInt _   = False

-- | Test whether a 'Type' is one of the builtin numeric types.
isNum :: Type -> Bool
isNum t = isInt t || isUInt t

-- | Construct a builtin tuple type out of a list of at least 2 types.
tuple :: [Type] -> Type
tuple ts
  | length ts >= 2 = TCon (tupleId $ length ts) ts
  | otherwise = error $ "Cannot create tuple of arity: " ++ show (length ts)

-- | Test whether a 'Type' is a tuple of some arity.
isTuple :: Type -> Bool
isTuple (TCon n ts) | length ts >= 2 = n == tupleId (length ts)
isTuple _                            = False

-- | Construct the type constructor of a builtin tuple of given arity (>= 2).
tupleId :: (Integral i, Identifiable v) => i -> v
tupleId i
  | i >= 2    = fromString $ "(" ++ replicate (fromIntegral i - 1) ',' ++ ")"
  | otherwise = error $ "Cannot create tuple of arity: " ++ show (toInteger i)

tempTupleId :: (Integral i, Identifiable v) => i -> v
tempTupleId i
  | i == 2 = fromString "Pair"
  | i == 3 || i == 4 = fromString $ "Pair" ++ show (toInteger i)
  | otherwise = error $ "Cannot create tuple of arity: " ++ show (toInteger i)

-- | More convenient representation of tuple types, for pattern-matching.
data TupleView
 = Tup2 (Type, Type)              -- ^ 2-tuples
 | Tup3 (Type, Type, Type)        -- ^ 3-tuples
 | Tup4 (Type, Type, Type, Type)  -- ^ 4-tuples
 | TupN [Type]                    -- ^ n-ary tuples
 | NotATuple                      -- ^ not a tuple

{- | Convert a 'Type' to a 'TupleView'; convenient for ViewPatterns.

For example, to match on just 2-tuples;

> foo :: Type -> String
> foo (tupleOf -> Tup2 (a, b)) = "2-tuple of " ++ show a ++ " and " ++ show b
> foo t                        = "Some other kind of type: " ++ show t
-}
tupleOf :: Type -> TupleView
tupleOf (TCon "(,)"   [a, b]      ) = Tup2 (a, b)
tupleOf (TCon "(,,)"  [a, b, c]   ) = Tup3 (a, b, c)
tupleOf (TCon "(,,,)" [a, b, c, d]) = Tup4 (a, b, c, d)
tupleOf t@(TCon _ ts) | isTuple t   = TupN ts
tupleOf _                           = NotATuple

-- | Kinds are just the arity of type constructors.
type Kind = Int

-- | Map to help us look up the kinds of builtin types.
builtinKinds :: M.Map TConId Kind
builtinKinds =
  M.fromList
    $  [ k $ TVar "a" `Arrow` TVar "b"
       , k Unit
       , k $ Ref $ TVar "a"
       , k $ List $ TVar "a"
       , k Time
       , k I64
       , k U64
       , k I32
       , k U32
       , k I16
       , k U16
       , k I8
       , k U8
       , ("(,)" , 2)
       , ("(,,)", 3)
       ]
    ++ take 8 (map tup [(2 :: Int) ..])
 where
  k (TCon tc ts) = (tc, length ts)
  k _            = error "This should only be used with (builtin) TCons"
  tup i = (tupleId i, 2)

instance Pretty Type where
  pretty (Arrow a b) = parens $ pretty a <+> "->" <+> pretty b
  pretty (List a   ) = brackets $ pretty a
  pretty (TCon n []) = pretty n
  pretty (TCon n ts) = parens $ hsep (pretty n : map pretty ts)
  pretty (TVar n   ) = pretty n

instance Dumpy Type where
  dumpy = pretty

instance Pretty Scheme where
  pretty (Scheme (Forall tvs CTrue t)) =
    pretty ("forall" :: String)
      <+> hsep (map pretty $ S.toList tvs)
      <>  comma
      <+> pretty t

instance Dumpy Scheme where
  dumpy = pretty

instance Pretty Annotation where
  pretty (AnnType t) = pretty t
  pretty _           = mempty

instance Dumpy Annotation where
  dumpy = pretty

instance Pretty Annotations where
  pretty (unAnnotations -> as) | null as   = mempty
                               | otherwise = pretty $ head as

instance Dumpy Annotations where
  dumpy = pretty
