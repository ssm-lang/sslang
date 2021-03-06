{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Type annotations, as collected from the Ast.
module IR.Types.Annotated
  ( Builtin(..)
  , Type(..)
  , TypeAnnote(..)
  , untyped
  ) where
import           Common.Identifiers             ( TConId
                                                , TVarId
                                                )
import           Common.Pretty
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           IR.Types.TypeSystem            ( Builtin(..)
                                                , TypeSystem(..)
                                                )

{- | A single term may be annotated by zero or more types.

When multiple exist, it should be assumed that they are equivalent, in the
sense that they can be unified.

Type annotations can be added using '<>' (from 'Semigroup'), while 'mempty'
represents no type annotation.
-}
newtype Type = Type [TypeAnnote]
  deriving Eq
  deriving Show
  deriving Typeable
  deriving Data
  deriving Semigroup  via [TypeAnnote]
  deriving Monoid     via [TypeAnnote]

-- | A type annotation.
data TypeAnnote
  = TBuiltin (Builtin Type)         -- ^ Builtin types
  | TCon TConId [Type]              -- ^ Type constructor, e.g., Option '0
  | TVar TVarId                     -- ^ Type variables, e.g., '0
  deriving (Show, Eq, Typeable, Data)

-- | 'Type' is a type system.
instance TypeSystem Type where
  projectBuiltin = Type . (: []) . TBuiltin

  -- | Unwrap the first TBuiltin annotation, if any.
  injectBuiltin (Type (TBuiltin t : _ )) = Just t
  injectBuiltin (Type (_          : ts)) = injectBuiltin $ Type ts
  injectBuiltin (Type []               ) = Nothing

-- | Convenience helper for no type annotations.
untyped :: Type
untyped = mempty

instance Pretty Type where
  pretty (Type (t : _)) = pretty t -- TODO: this only prints the outer-most ann
  pretty (Type []     ) = pretty "_"

instance Pretty TypeAnnote where
  pretty (TBuiltin t) = pretty t
  pretty (TCon t ts ) = parens $ hsep (pretty t : map pretty ts)
  pretty (TVar v    ) = pretty v
