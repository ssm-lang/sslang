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
module IR.Types.Flat
  ( Type(..)
  , Builtin(..)
  , flattenApp
  ) where

import           Common.Identifiers             ( TConId )
import           IR.Types.TypeSystem            ( Builtin(..)
                                                , TypeSystem(..)
                                                )

-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type) -- ^ Builtin types
  | TCon TConId             -- ^ Type constructors
  deriving Eq

instance TypeSystem Type where
  projectBuiltin = TBuiltin
  injectBuiltin (TBuiltin t) = Just t
  injectBuiltin _            = Nothing

-- | Flatten a list of type constructors into a
flattenApp :: TConId -> [Type] -> TConId
flattenApp = error "TODO"
