module IR.Pattern.Common where

import Common.Identifiers (
  DConId (..),
  Identifier (..),
  TConId (..),
 )

import qualified Data.Map as M
import qualified Data.Set as S
import qualified IR.IR as I


data CInfo = CInfo
  { cName :: Identifier
  , cType :: Identifier
  , cArity :: Int
  }
  deriving (Eq, Show)


data TInfo = TInfo
  { tName :: Identifier
  , tCSet :: S.Set Identifier
  }
  deriving (Eq, Show)


buildTypeMap :: [(TConId, I.TypeDef)] -> M.Map Identifier TInfo
buildTypeMap = foldr tAcc M.empty
 where
  tAcc td' tmap' =
    let (TConId typ, td) = td'
        clist = map (\(DConId cid, _) -> cid) $ I.variants td
        cset = S.fromList clist
     in M.insert typ (TInfo{tName = typ, tCSet = cset}) tmap'


buildConsMap :: [(TConId, I.TypeDef)] -> M.Map Identifier CInfo
buildConsMap = foldr cAcc M.empty
 where
  cAcc td' cmap' =
    let (TConId typ, td) = td'
        clist = I.variants td
        cAcc' tvp cmap'' =
          let (DConId cid, tvars) = tvp
              arity = case tvars of
                I.VariantUnnamed tl -> length tl
                I.VariantNamed tl -> length tl
              c = CInfo{cName = cid, cType = typ, cArity = arity}
           in M.insert cid c cmap''
     in foldr cAcc' cmap' clist
