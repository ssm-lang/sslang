{-# LANGUAGE OverloadedStrings #-}
module Front.Identifiers where

import           Common.Default                 ( Default(..) )
import           Common.Identifiers
import qualified Data.Map                      as M

data IdKind = User | Builtin | Reserved deriving (Show, Eq)

newtype DataInfo = DataInfo { dataKind :: IdKind }
newtype TypInfo = TypInfo { typKind :: IdKind }

instance Default DataInfo where
  def = DataInfo { dataKind = User }

instance Default TypInfo where
  def = TypInfo { typKind = User }

builtinTypes :: M.Map Identifier TypInfo
builtinTypes = M.fromList $ map mkBuiltin ["Int", "[]", "&"]
  where mkBuiltin i = (i, TypInfo { typKind = Builtin })

builtinData :: M.Map Identifier DataInfo
builtinData = M.fromList $ map mkBuiltin ["-", "+", "*", "/", "deref", "new"]
  where mkBuiltin i = (i, DataInfo { dataKind = Builtin })
