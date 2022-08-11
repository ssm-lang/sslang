{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{- |

Types with Typeclasses, e.g., after type inference

For now, just the polymorphic types.

-}
module IR.Types.Classes
  ( Builtin(..)
  , Type(..)
  , Scheme(..)
  ) where

import           Common.Identifiers             ( TConId
                                                , TVarId
                                                )
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           IR.Types.TypeSystem            ( Builtin(..)
                                                , TypeSystem(..)
                                                )
import           Prettyprinter

import           Data.List                      ( intercalate )


-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type)         -- ^ Builtin types
  | TCon TConId [Type]              -- ^ Type constructor, e.g., Option '0
  | TVar TVarId                     -- ^ Type variables, e.g., '0
  deriving (Eq, Ord, Typeable, Data)

-- | Type scheme.
data Scheme = Forall [TVarId] Type
  deriving (Eq, Ord, Typeable, Data, Show)

instance Pretty Scheme where
  pretty (Forall [] t) = pretty $ "Forall . " ++ show t
  pretty (Forall args t) =
    pretty $ "Forall " ++ unwords (map show args) ++ " . " ++ show t

instance TypeSystem Type where
  projectBuiltin = TBuiltin
  injectBuiltin (TBuiltin t) = Just t
  injectBuiltin _            = Nothing

instance Pretty Type where
  pretty (TBuiltin b  ) = pretty b
  pretty (TCon tcon ts) = parens (hsep $ pretty tcon : map pretty ts)
  pretty (TVar tvar   ) = pretty tvar

instance Show Type where
  show (TBuiltin t) = "#" ++ show t
  show (TCon tc args) = intercalate " " (show tc : map show args)
  show (TVar v) = show v
