{- | The types of identifiers used across the compiler.

These are defined as newtypes (rather than as type aliases) so that they cannot
be accidentally compared with one another.
-}
module Common.Identifiers where

import           Language.C                     ( Id(..) )
import           Language.C.Quote               ( ToIdent(..) )

import           Data.Loc                       ( noLoc )

-- | ToIdentifier for type variable, e.g., "a".
newtype TVarId = TVarId String deriving Eq

instance ToIdent TVarId where
  toIdent (TVarId i) = Id i

-- | de Bruijn index for type variables, e.g., "'0".
newtype TVarIdx = TVarIdx Int deriving Eq

-- | ToIdentifier for type constructors, e.g., "Option".
newtype TConId = TConId String deriving Eq

instance ToIdent TConId where
  toIdent (TConId i) = Id i

-- | ToIdentifier for data constructors, e.g., "None".
newtype DConId = DConId String deriving Eq

instance ToIdent DConId where
  toIdent (DConId i) = Id i

-- | ToIdentifier for low-level identifiers, e.g., "ssm_activate".
newtype FfiId = FfiId String deriving Eq

instance ToIdent FfiId where
  toIdent (FfiId i) = Id i

-- | ToIdentifier for user-defined variable, e.g., "x".
newtype VarId = VarId String deriving Eq

instance ToIdent VarId where
  toIdent (VarId i) = Id i

-- | ToIdentifier for struct field names, e.g., "len"
newtype FieldId = FieldId String deriving Eq

instance ToIdent FieldId where
  toIdent (FieldId i) = Id i

-- | The name (if any) a value is bound to.
type Binder = Maybe VarId

ident :: ToIdent t => t -> String
ident i = case toIdent i noLoc of
  Id     s _ -> s
  AntiId _ _ -> error "Trying to get ident of antiid"

-- TODO implement Show instances for these
