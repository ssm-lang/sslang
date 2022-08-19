{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module IR.Types where

import           Common.Identifiers             ( TConId(..)
                                                , TVarId(..)
                                                , fromString
                                                )
import           Common.Pretty                  ( (<+>)
                                                , Dumpy(..)
                                                , Pretty(pretty)
                                                , brackets
                                                , hsep
                                                , parens
                                                )
import qualified Data.Set                      as S

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
  deriving (Eq, Show)

{- | Constraints on a type scheme.

For now, we only support trivial constraints.
-}
data Constraint = CTrue
  deriving (Eq, Show)

{- | Type schemes that quantify over some type variables.

-}
data Scheme = Scheme [TVarId] Constraint Type
  deriving (Eq, Show)

-- | A 'Scheme' with no quantified type variables and a trivial constraint.
trivialScheme :: Type -> Scheme
trivialScheme = Scheme [] CTrue

-- | Some data type that contains a sslang 'Type'.
class HasType a where
  getType :: a -> Type

instance HasType Scheme where
  getType (Scheme _ _ t) = t

-- | Get free type variables, according to some membership function
ftv :: HasType a => (TVarId -> Bool) -> a -> S.Set TVarId
ftv mem = go . getType
 where
  go :: Type -> S.Set TVarId
  go (TCon _ ts) = S.unions $ map go ts
  go (TVar v) | mem v     = S.empty
              | otherwise = S.singleton v

-- | May appear in type annotations; indicates a fresh free type variable.
pattern Hole :: Type
pattern Hole = TVar "_"

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b = TCon "->" [a, b]

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
isInt I8 = True
isInt _ = False

isUInt :: Type -> Bool
isUInt U64 = True
isUInt U32 = True
isUInt U16 = True
isUInt U8 = True
isUInt _ = False

isNum :: Type -> Bool
isNum t = isInt t || isUInt t

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
> foo (tuple -> Tup2 (a, b)) = "2-tuple of " ++ show a ++ " and " ++ show b
> foo t                      = "Some other kind of type: " ++ show t
-}
tuple :: Type -> TupleView
tuple (TCon "(,)"   [a, b]      ) = Tup2 (a, b)
tuple (TCon "(,,)"  [a, b, c]   ) = Tup3 (a, b, c)
tuple (TCon "(,,,)" [a, b, c, d]) = Tup4 (a, b, c, d)
tuple t@(TCon _ ts) | isTuple t   = TupN ts
tuple _                           = NotATuple

-- | Tests whether a 'Type' is a tuple of some arity.
isTuple :: Type -> Bool
isTuple (TCon n ts) | length ts >= 2 = n == tupName (length ts)
isTuple _                            = False

-- | Construct the name of the built-in tuple type of given arity.
--
-- Fails if arity is less than 2.
tupName :: Integral i => i -> TConId
tupName i
  | i >= 2    = fromString $ "(" ++ replicate (fromIntegral i - 1) ',' ++ ")"
  | otherwise = error $ "Cannot create tuple of arity: " ++ show (toInteger i)

instance Pretty Type where
  pretty (Arrow a b) = parens $ pretty a <+> "->" <+> pretty b
  pretty (List a   ) = brackets $ pretty a
  pretty (TCon n []) = pretty n
  pretty (TCon n ts) = parens $ hsep (pretty n : map pretty ts)
  pretty (TVar n   ) = pretty n

instance Dumpy Type where
  dumpy t = pretty t
