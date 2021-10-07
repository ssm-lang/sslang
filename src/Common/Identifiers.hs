{-# LANGUAGE DerivingVia #-}
{- | The types of identifiers used across the compiler.

These are defined as newtypes (rather than as type aliases) so that they cannot
be accidentally compared with one another.
-}
module Common.Identifiers
  ( Identifiable(..)
  , IsString(..)
  , fromId
  , TVarId(..)
  , TConId(..)
  , DConId(..)
  , TVarIdx(..)
  , FfiId(..)
  , VarId(..)
  , FieldId(..)
  , Binder
  , Identifier(..)
  ) where

import           Data.String                    ( IsString(..) )

import           Language.C                     ( Id(..) )
import           Language.C.Quote               ( ToIdent(..) )

newtype Identifier = Identifier String deriving Eq

class IsString i => Identifiable i where
  ident :: i -> String

instance IsString Identifier where
  fromString = Identifier

instance Identifiable Identifier where
  ident (Identifier i) = i

instance Show Identifier where
  show (Identifier i) = i

instance ToIdent Identifier where
  toIdent (Identifier i) = Id i

instance Semigroup Identifier where
  Identifier a <> Identifier b = Identifier $ a ++ b

instance Monoid Identifier where
  mempty = Identifier ""

fromId :: (Identifiable a, Identifiable b) => a -> b
fromId = fromString . ident

-- | ToIdentifier for type variable, e.g., "a".
newtype TVarId = TVarId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier

-- | ToIdentifier for type constructors, e.g., "Option".
newtype TConId = TConId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier

-- | ToIdentifier for data constructors, e.g., "None".
newtype DConId = DConId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier

-- | ToIdentifier for low-level identifiers, e.g., "ssm_activate".
newtype FfiId = FfiId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier

-- | ToIdentifier for user-defined variable, e.g., "x".
newtype VarId = VarId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier

-- | ToIdentifier for struct field names, e.g., "len"
newtype FieldId = FieldId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier

-- | de Bruijn index for type variables, e.g., "'0".
newtype TVarIdx = TVarIdx Int
  deriving Eq

instance Show TVarIdx where
  show (TVarIdx i) = "'" ++ show i

-- | The name (if any) a value is bound to.
type Binder = Maybe VarId
