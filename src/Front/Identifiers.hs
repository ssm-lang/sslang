{-# LANGUAGE OverloadedStrings #-}

-- | Identifiers, associated metadata, and reserved identifiers.
module Front.Identifiers where

import Common.Default (Default (..))
import Common.Identifiers

import qualified Data.Map as M


-- | Where an identifier comes from.
data IdKind
  = -- | User- and library-defined identifiers, e.g., @foo@.
    User
  | -- | Builtin identifiers, e.g., @new@ and @deref@.
    Builtin
  | -- | Unused, but user should not be able to define.
    Reserved
  deriving (Show, Eq)


-- | Metadata associated with a data identifier.
newtype DataInfo = DataInfo {dataKind :: IdKind}


-- | Metadata associated with a type identifier.
newtype TypInfo = TypInfo {typKind :: IdKind}


instance Default DataInfo where
  def = DataInfo{dataKind = User}


instance Default TypInfo where
  def = TypInfo{typKind = User}


-- | Map of builtin types.
builtinTypes :: M.Map Identifier TypInfo
builtinTypes = M.fromList $ map mkBuiltin ["Int", "[]", "&", "()"]
 where
  mkBuiltin i = (i, TypInfo{typKind = Builtin})


-- | Map of builtin data.
builtinData :: M.Map Identifier DataInfo
builtinData =
  M.fromList $
    map
      mkBuiltin
      [ "-"
      , "+"
      , "*"
      , "/"
      , "%"
      , "=="
      , "!="
      , "<="
      , ">="
      , "<"
      , ">"
      , "deref"
      , "new"
      , "now"
      ]
 where
  mkBuiltin i = (i, DataInfo{dataKind = Builtin})
