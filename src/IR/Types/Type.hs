{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.Set                      as S
import           Data.Set                       ( (\\) )


{- | The type of sslang types.

Structurally speaking, these are very simple. Types are either type variables or
type constructors applied to some other types.

Builtin types (and type constructors) include 'Arrow', 'Unit', 'Ref', 'List',
and various sizes of tuples; for convenience, these are defined using GHC
extensions PatternSynonyms.
-}
data Type
  = TCon TConId [Type]
  | TVar TVarId
  deriving (Eq, Show, Typeable, Data)

{- | Constraints on a type scheme.

For now, we only support trivial constraints.
-}
data Constraint = CTrue
  deriving (Eq, Show, Typeable, Data)

{- | Schemes quantify over 'Type' variables and impose 'Constraint'.

'SchemeOf' is implemented a functor over some kind of type so that we can easily
substitute in 'Type' vs 'UType' when performing type inference/unification.
-}
data SchemeOf t = Forall (S.Set TVarId) Constraint t
  deriving (Eq, Show, Functor, Foldable, Traversable, Typeable, Data)

-- | Unwrap a scheme and obtain the underlying type.
unScheme :: SchemeOf t -> t
unScheme (Forall _ _ t) = t

-- | Construct a scheme with quantified type variables and a trivial constraint
forall :: (Functor l, Foldable l) => l TVarId -> t -> SchemeOf t
forall vs = Forall (S.fromList $ toList $ fmap fromId vs) CTrue

-- | Schemes over 'Type'.
newtype Scheme = Scheme (SchemeOf Type)
  deriving (Eq, Show, Typeable, Data)

-- | An annotation records the annotated portion of a pattern.
data Annotation
  = AnnType Type
  | AnnDCon DConId [Annotation]
  | AnnArrows [Annotation] Annotation
  deriving (Eq, Show, Typeable, Data)

newtype Annotations = Annotations [Annotation]
  deriving (Eq, Show, Typeable, Data, Semigroup, Monoid)

unAnnotations :: Annotations -> [Annotation]
unAnnotations (Annotations as) = as

fromAnnotations :: Annotations -> Type
fromAnnotations = go . unAnnotations
 where
  go (AnnType t : _   ) = t
  go (_         : anns) = go anns
  go []                 = error "TODO: No type annotations"

instance HasFreeVars Type TVarId where
  freeVars (TCon _ ts) = S.unions $ map freeVars ts
  freeVars Hole        = S.empty
  freeVars (TVar v)    = S.singleton v

instance HasFreeVars Scheme TVarId where
  freeVars (Scheme (Forall s _ t)) = freeVars t \\ s

schemeOf :: Type -> Scheme
schemeOf t = Scheme $ Forall (freeVars t) CTrue t

-- | Some data type that contains a sslang 'Type'.
class HasType a where
  getType :: a -> Type

instance HasType Type where
  getType = id

instance HasType Scheme where
  getType (Scheme (Forall _ _ t)) = t

-- | May appear in type annotations; indicates a fresh free type variable.
pattern Hole :: Type
pattern Hole = TVar "_"

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b = TCon "->" [a, b]

unfoldArrow :: Type -> ([Type], Type)
unfoldArrow (Arrow a b) = let (as, rt) = unfoldArrow b in (a : as, rt)
unfoldArrow t           = ([], t)

foldArrow :: ([Type], Type) -> Type
foldArrow (a : as, rt) = a `Arrow` foldArrow (as, rt)
foldArrow ([]    , t ) = t

pattern Unit :: Type
pattern Unit = TCon "()" []

pattern Ref :: Type -> Type
pattern Ref a = TCon "&" [a]

pattern List :: Type -> Type
pattern List a = TCon "[]" [a]

pattern Time :: Type
pattern Time = TCon "Time" []

pattern I64 :: Type
pattern I64 = TCon "Int64" []

pattern U64 :: Type
pattern U64 = TCon "UInt64" []

pattern I32 :: Type
pattern I32 = TCon "Int32" []

pattern U32 :: Type
pattern U32 = TCon "UInt32" []

pattern I16 :: Type
pattern I16 = TCon "Int16" []

pattern U16 :: Type
pattern U16 = TCon "UInt16" []

pattern I8 :: Type
pattern I8 = TCon "Int8" []

pattern U8 :: Type
pattern U8 = TCon "UInt8" []

isInt :: Type -> Bool
isInt I64 = True
isInt I32 = True
isInt I16 = True
isInt I8  = True
isInt _   = False

isUInt :: Type -> Bool
isUInt U64 = True
isUInt U32 = True
isUInt U16 = True
isUInt U8  = True
isUInt _   = False

isNum :: Type -> Bool
isNum t = isInt t || isUInt t

tuple :: [Type] -> Type
tuple ts = TCon (tupleId $ length ts) ts

-- | Tests whether a 'Type' is a tuple of some arity.
isTuple :: Type -> Bool
isTuple (TCon n ts) | length ts >= 2 = n == tupleId (length ts)
isTuple _                            = False

-- | Construct the name of the built-in tuple type of given arity.
--
-- Fails if arity is less than 2.
tupleId :: (Integral i, Identifiable v) => i -> v
tupleId i
  | i >= 2    = fromString $ "(" ++ replicate (fromIntegral i - 1) ',' ++ ")"
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

-- FIXME: convenience for ghci debugging
scheme :: [String] -> Constraint -> Type -> Scheme
scheme vs c t = Scheme $ Forall (S.fromList $ map fromString vs) c t

-- FIXME: convenience for ghci debugging
tv :: String -> Type
tv = TVar . fromString
