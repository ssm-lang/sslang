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

instance TypeSystem Type where
  projectBuiltin = TBuiltin
  injectBuiltin (TBuiltin t) = Just t
  injectBuiltin _            = Nothing

instance Pretty Type where
  pretty (TBuiltin b  ) = pretty b
  pretty (TCon tcon ts) = parens (hsep $ pretty tcon : map pretty ts)
  pretty (TVar tvar   ) = pretty tvar
