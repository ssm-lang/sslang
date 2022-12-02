{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{- | The types of identifiers used across the compiler.

The types 'VarId', 'TVarId', 'DConId', and 'TConId' are used in the IR for data
variables, type variables, data constructors, and type constructors, which each
inhabit a separate namespace. The underlying 'Identifier' type is used in the
AST, before these different kinds of identifiers are differentiated by the
lowering phase.

These are defined as newtypes (rather than as type aliases) that ultimately
abstract over the underlying 'String' Identifier. We do this for a few reasons:

    * The particular newtype carries semantic meaning in the IR, e.g., 'VarId'
      is an identifier for data variables and nothing else. This prevents
      semantically distinct items from being used in place of one another
      (without explicit coercion). For example, this will prevent accidentally
      using a type variable identifier as a data constructor.

    * In the future, this may be extended to carry other (non-semantic)
      metadata, such as source code location.

    * It allows us to freely attach typeclass instances to this type without
      FlexibleInstances (since 'String' is a type synonym for @[Char]@).

Users should never need to specifically use the data constructor for each
newtype, e.g., @VarId (Identifier "foo")@. Instead, each identifier belongs to
the 'IsString' typeclass, that can be written as @fromString "foo"@, so that the
appropriate type can be inferred from the context where the identifier is used.
Furthermore, the 'fromString' call can be automatically inserted by the
OverloadedStrings GHC extension, meaning we can just write @"foo"@.

All identifier types are instances of the 'Identifiable' typeclass, which allows
us to write generic functions that operate over any kind of identifier. One can
explicitly convert from an identifier to another using the handy 'fromId'
helper; which /specific/ type of identifier may be inferred from the outer
context, or explicitly annotated (e.g., @fromId i :: VarId@ to construct
a 'VarId' out of identifier @i@).
-}
module Common.Identifiers
  ( Identifiable(..)
  , IsString(..)
  , fromId
  , showId
  , TConId(..)
  , TVarId(..)
  , DConId(..)
  , VarId(..)
  , TVarIdx(..)
  , CSym(..)
  , HasFreeVars(..)
  , Binder
  , Identifier(..)
  , isCons
  , isVar
  , mangle
  , mangleVars
  , isGenerated
  , genId
  , ungenId
  , tuple
  , tempTuple
  , cons
  , nil
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
import qualified Data.Set                      as S
import           Data.String                    ( IsString(..) )

import           Language.C                     ( Id(..) )
import           Language.C.Quote               ( ToIdent(..) )

-- | A type that may be used as a Sslang identifier.
class (IsString i, Ord i, Show i) => Identifiable i where
  ident :: i -> String -- ^ Obtain its underlying 'String' representation.

-- | Explicitly convert between two types of identifiers.
fromId :: (Identifiable a, Identifiable b) => a -> b
fromId = fromString . ident

-- | Convert a showable instance to some kind of identifier.
showId :: (Show a, Identifiable b) => a -> b
showId = fromString . show

{- | A generic Sslang identifier.

Used as the type for identifiers in the AST.

Also used as the base type for other identifiers (e.g., 'TConId', 'VarId'),
which derive their typeclass instances from this.
-}
newtype Identifier = Identifier String
  deriving (Eq, Ord, Typeable, Data)

instance IsString Identifier where
  fromString = Identifier

instance Identifiable Identifier where
  ident (Identifier i) = i

instance Show Identifier where
  show (Identifier i) = i

instance ToIdent Identifier where
  toIdent (Identifier i) = Id i

instance Semigroup Identifier where
  Identifier a <> Identifier b = Identifier $ a <> b

instance Monoid Identifier where
  mempty = Identifier mempty -- ""

instance Pretty Identifier where
  pretty = pretty . ident

-- | Identifier for type constructors, e.g., @Option@.
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
  show (TConId i) = show i

-- | ToIdentifier for type variable, e.g., @a@.
newtype TVarId = TVarId Identifier
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

instance Show TVarId where
  show (TVarId i) = show i

-- | Identifier for data constructors, e.g., @None@.
newtype DConId = DConId Identifier
  deriving Eq
  deriving Show
  deriving Ord
  deriving Typeable
  deriving Data
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | Identifier for data variables, e.g., @x@.
newtype VarId = VarId Identifier
  deriving Eq
  deriving Show
  deriving Ord
  deriving Typeable
  deriving Data
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | Identifier for C symbols, e.g., @printf@.
newtype CSym = CSym Identifier
  deriving Eq
  deriving Show
  deriving Ord
  deriving Typeable
  deriving Data
  deriving ToIdent via Identifier
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier
  deriving Pretty via Identifier

-- | de Bruijn index for type variables, e.g., @'0@
newtype TVarIdx = TVarIdx Int
  deriving Eq
  deriving Ord
  deriving Typeable
  deriving Data

instance Show TVarIdx where
  show (TVarIdx i) = "'" ++ show i

instance Pretty TVarIdx where
  pretty = pretty . show

-- | Terms @t@ that have free variables @i@
class Identifiable i => HasFreeVars t i | t -> i where
  freeVars :: t -> S.Set i

-- | A name to be bound; 'Nothing' represents a wildcard, e.g., @let _ = ...@.
type Binder = Maybe VarId

-- | Whether an identifier refers to a type or data constructor.
isCons :: Identifiable a => a -> Bool
isCons i | null s    = False
         | otherwise = isUpper (head s) || head s == ':' && last s == ':'
  where s = ident i

-- | Whether an identifier refers to a type or data variable.
--
-- Note that internal variables (i.e., 'isIVar') are also considered variables.
isVar :: Identifiable a => a -> Bool
isVar = not . isCons

-- | Whether an identifier is an compiler-generated variable name.
isGenerated :: Identifiable a => a -> Bool
isGenerated i | null s    = False
              | otherwise = head s == '(' && last s == ')'
  where s = ident i

-- | Generate an internal variable name (parenthesized) from some hint.
genId :: Identifiable a => a -> a
genId i | null s    = fromString "(_)"
        | otherwise = fromString $ "(" <> s <> ")"
  where s = ident i

-- | Filter out generated identifiers.
ungenId :: Identifiable a => a -> Maybe a
ungenId i | isGenerated i = Nothing
          | otherwise     = Just i

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
            i'   = genId $ fromString $ show ctr'
        put (ctr', M.insert i i' idMap)
        return i'

-- | Mangle all type and data variable identifiers.
mangleVars :: (Data a) => a -> a
mangleVars = mangle (Proxy :: Proxy VarId) . mangle (Proxy :: Proxy TVarId)

-- | the tuple identifier
tuple :: Identifier
tuple = Identifier "(,)"

-- | we'll use this temp tuple name for now due to the naming issue
tempTuple :: Identifier
tempTuple = Identifier "Pair"

-- | Cons identifier for Lists
cons :: Identifier
cons = Identifier "Cons"

-- | Nil identifier for Lists
nil :: Identifier
nil = Identifier "Nil"