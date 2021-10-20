{- |

Types with Typeclasses, e.g., after type inference

For now, just the polymorphic types.

-}

{-# LANGUAGE DerivingVia #-}
module IR.Types.Classes
  ( Builtin(..)
  , Type(..)
  ) where

import           Common.Identifiers             ( TConId
                                                , TVarIdx
                                                )
import           IR.Types.TypeSystem            ( Builtin(..)
                                                , TypeSystem(..)
                                                )
import           Prettyprinter


-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type)         -- ^ Builtin types
  | TCon TConId [Type]              -- ^ Type constructor, e.g., Option '0
  | TVar TVarIdx                    -- ^ Type variables, e.g., '0
  deriving Eq

instance Pretty Type where
  pretty (TBuiltin a) = pretty "(todo: pretty print builtin type)"
  pretty (TCon a b) = pretty "(todo: pretty print tcon type)"
  pretty (TVar a) = pretty "(todo: pretty print varIdx type)"
-- | 'Type' is a type system.
instance TypeSystem Type where
  projectBuiltin = TBuiltin
  injectBuiltin (TBuiltin t) = Just t
  injectBuiltin _            = Nothing
