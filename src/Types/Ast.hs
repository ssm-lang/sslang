{- |

Types straight from the AST.

For now, just the polymorphic typeclass types.

-}
{-# LANGUAGE DerivingVia #-}
module Types.Ast
  ( Builtin(..)
  , Type(..)
  , TypeAnnote(..)
  , untyped) where
import           Common.Identifiers             ( TConId
                                                , TVarIdx
                                                )
import           Types.TypeSystem               ( Builtin(..)
                                                , TypeSystem(..)
                                                )
{- | A single term may be annotated by zero or more types.

When multiple exist, it should be assumed that they are "equivalent", in the
sense that they can be unified.

Type annotations can be added using '<>' (from 'Semigroup'), while 'mempty'
represents no type annotation.
-}
newtype Type = Type [TypeAnnote]
  deriving Eq         via [TypeAnnote]
  deriving Semigroup  via [TypeAnnote]
  deriving Monoid     via [TypeAnnote]

-- | A type annotation.
data TypeAnnote
  = TBuiltin (Builtin Type)         -- ^ Builtin types
  | TCon TConId [Type]              -- ^ Type constructor, e.g., Option '0
  | TVar TVarIdx                    -- ^ Type variables, e.g., '0
  deriving Eq

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
