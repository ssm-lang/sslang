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
import           Prettyprinter

-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type) -- ^ Builtin types
  | TCon TConId             -- ^ Type constructors
  deriving (Eq, Show)

instance TypeSystem Type where
  projectBuiltin = TBuiltin
  injectBuiltin (TBuiltin t) = Just t
  injectBuiltin _            = Nothing

instance Pretty Type where
  pretty (TBuiltin a) = pretty "(todo: pretty print builtin type)"
  pretty (TCon  a) = pretty "(todo: pretty print tcon type)"

-- | Flatten a list of type constructors into a
flattenApp :: TConId -> [Type] -> TConId
flattenApp t [] = t
flattenApp t ts = error $ "flat cat: " ++ show t ++ " and " ++ concatMap show ts
