{-# LANGUAGE DerivingVia #-}
{- | The types of identifiers used across the compiler.

These are defined as newtypes (rather than as type aliases) so that they cannot
be accidentally compared with one another.
-}
module Common.Identifiers
  ( Identifiable(..)
  , IsString(..)
  , fromId
  , TConId(..)
  , TVarId(..)
  , TVarIdx(..)
  , DConId(..)
  , FfiId(..)
  , VarId(..)
  , FieldId(..)
  , Binder
  , Identifier(..)
  , hasRepeated
  , isCons
  , isVar
  ) where

import           Data.Char                      ( isUpper )
import           Data.List                      ( group )
import           Data.String                    ( IsString(..) )

import           Language.C                     ( Id(..) )
import           Language.C.Quote               ( ToIdent(..) )

import           Prettyprinter                  ( Pretty(..) )

-- | A basic identifier: just a string
newtype Identifier = Identifier String deriving (Eq, Ord)

-- | Turn a general identifier into a string
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

instance Pretty Identifier where
  pretty = pretty . ident

-- | Convert between two types of identifiers
fromId :: (Identifiable a, Identifiable b) => a -> b
fromId = fromString . ident

-- | ToIdentifier for type constructors, e.g., @Option@
newtype TConId = TConId Identifier
  deriving Eq
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

instance Show TConId where
  show (TConId i) = "Ty" ++ show i

-- | ToIdentifier for type variable, e.g., @a@
newtype TVarId = TVarId Identifier
  deriving Eq
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

instance Show TVarId where
  show (TVarId i) = "tyvar" ++ show i

-- | de Bruijn index for type variables, e.g., @'0@
newtype TVarIdx = TVarIdx Int
  deriving Eq

instance Show TVarIdx where
  show (TVarIdx i) = "'" ++ show i

instance Pretty TVarIdx where
  pretty = pretty . show

-- | ToIdentifier for data constructors, e.g., @None@
newtype DConId = DConId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | ToIdentifier for low-level identifiers, e.g., @ssm_activate@
newtype FfiId = FfiId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | ToIdentifier for user-defined variable, e.g., @x@
newtype VarId = VarId Identifier
  deriving Eq
  deriving Ord via Identifier
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | ToIdentifier for struct field names, e.g., @len@
newtype FieldId = FieldId Identifier
  deriving Eq
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | A name to be bound; 'Nothing' represents a wildcard, e.g., @let _ = ...@
type Binder = Maybe VarId

-- | List of identifiers contains a repeated member.
hasRepeated :: [Identifier] -> Bool
hasRepeated = not . all ((== 1) . length) . group

-- | Whether an identifier refers to a type or data constructor.
isCons :: Identifier -> Bool
isCons i | null s    = False
         | otherwise = isUpper (head s) || head s == ':' && last s == ':'
  where s = ident i

-- | Whether an identifier refers to a type or data variable.
isVar :: Identifier -> Bool
isVar = not . isCons
