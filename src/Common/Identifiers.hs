{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , DConId(..)
  , VarId(..)
  , TVarIdx(..)
  , Binder
  , Identifier(..)
  , isCons
  , isVar
  , mangle
  , mangleVars
  ) where

import           Common.Pretty                  ( Pretty(..) )

import           Control.Monad.State            ( MonadState(..)
                                                , State
                                                , evalState
                                                )
import           Data.Char                      ( isUpper )
import           Data.Generics                  ( Data
                                                , Proxy(Proxy)
                                                , Typeable
                                                , everywhereM
                                                , mkM
                                                )
import qualified Data.Map                      as M
import           Data.String                    ( IsString(..) )

import           Language.C                     ( Id(..) )
import           Language.C.Quote               ( ToIdent(..) )

-- | A basic identifier: just a string
newtype Identifier = Identifier String deriving (Eq, Ord, Typeable, Data)

-- | Turn a general identifier into a string
class (IsString i, Ord i) => Identifiable i where
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
  deriving Ord
  deriving Typeable
  deriving Data
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
  deriving Ord
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
  deriving Ord

instance Show TVarIdx where
  show (TVarIdx i) = "'" ++ show i

instance Pretty TVarIdx where
  pretty = pretty . show

-- | ToIdentifier for data constructors, e.g., @None@
newtype DConId = DConId Identifier
  deriving Eq
  deriving Ord
  deriving Typeable
  deriving Data
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
  deriving Ord
  deriving Typeable
  deriving Data
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
  deriving Ord
  deriving Typeable
  deriving Data
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
  deriving Ord
  deriving Typeable
  deriving Data
  deriving Show via Identifier
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | A name to be bound; 'Nothing' represents a wildcard, e.g., @let _ = ...@
type Binder = Maybe VarId

-- | Whether an identifier refers to a type or data constructor.
isCons :: Identifiable a => a -> Bool
isCons i | null s    = False
         | otherwise = isUpper (head s) || head s == ':' && last s == ':'
  where s = ident i

-- | Whether an identifier refers to a type or data variable.
isVar :: Identifiable a => a -> Bool
isVar = not . isCons

{- | Mangle all identifiers in some data structure.

This function is useful for preserving the general syntactic structure of
a datum without inspecting the /specific/ identifiers used within. This is
useful for comparing ASTs modulo alpha renaming.

The @Proxy i@ parameter is used to specify exactly which kind of identifier to
mangle. For instance, to mangle all 'VarId' nodes:

> mangleVarId :: Data a => a -> a
> mangleVarId = mangle (Proxy :: VarId)
-}
mangle :: (Identifiable i, Data i, Data a) => Proxy i -> a -> a
mangle p d = everywhereM (mkM $ mang p) d `evalState` (0, M.empty)
 where
  mang :: (Identifiable i, Data i) => Proxy i -> i -> State (Int, M.Map i i) i
  mang _ i = do
    (ctr, idMap) <- get
    case M.lookup i idMap of
      Just i' -> return i'
      Nothing -> do
        let ctr' = ctr + 1
            i'   = fromString $ "mang" <> show ctr'
        put (ctr', M.insert i i' idMap)
        return i'

-- | Mangle all type and data variable identifiers.
mangleVars :: (Data a) => a -> a
mangleVars = mangle (Proxy :: Proxy VarId) . mangle (Proxy :: Proxy TVarId)
