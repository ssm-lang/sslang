{- | Basic type system with only nullary type constants and builtin types.

Features (or lack thereof):

- No type variables
- No type classes
- No user-defined type application

This should be the last type system before the IR is lowered to a lower
representation.

Note that we do not flatten the type application for builtin type constructors
(e.g., Ref, Arrow, etc.) because their internal structure must be understood by
codegen. An alternate perspective is that these are left unapplied because these
are the same type constructors available in lower-level languages, so we can
easily lower them with a 1:1 encoding.
-}
module Types.Flat
  ( Type(..)
  , TypeDef(..)
  , Builtin(..)
  ) where

import           Common.Identifiers             ( DConId
                                                , TConId
                                                )
import           Common.Types                   ( Builtin(..)
                                                , TypeSystem(..)
                                                , TypeVariant(..)
                                                )

-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type) -- ^ Builtin types
  | TCon TConId             -- ^ Type constructors
  deriving Eq

-- | User-defined type.
newtype TypeDef = TypeDef
  { variants :: [(DConId, TypeVariant Type)]
  }

instance TypeSystem Type where
  unit = TBuiltin Unit
  void = TBuiltin Void
  ref t = TBuiltin $ Ref t
  arrow a b = TBuiltin $ Arrow a b
