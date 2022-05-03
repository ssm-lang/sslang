module Front.Pattern.Common where

import           Common.Identifiers             ( Identifier(..) )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Front.Ast                     as A

data CInfo = CInfo
  { cName  :: Identifier
  , cType  :: Identifier
  , cArity :: Int
  }
  deriving (Eq, Show)

data TInfo = TInfo
  { tName :: Identifier
  , tCSet :: S.Set Identifier
  }
  deriving (Eq, Show)

buildTypeMap :: [A.TypeDef] -> M.Map Identifier TInfo
buildTypeMap = foldr tAcc M.empty
 where
  tAcc td tmap' =
    let typ   = A.typeName td
        clist = map (\(A.VariantUnnamed cid _) -> cid) (A.typeVariants td)
        cset  = S.fromList clist
    in  M.insert typ (TInfo { tName = typ, tCSet = cset }) tmap'

buildConsMap :: [A.TypeDef] -> M.Map Identifier CInfo
buildConsMap = foldr cAcc M.empty
 where
  cAcc td cmap' =
    let typ   = A.typeName td
        clist = A.typeVariants td
        cAcc' (A.VariantUnnamed cid ts) cmap'' =
          let c = CInfo { cName = cid, cType = typ, cArity = length ts }
          in  M.insert cid c cmap''
    in  foldr cAcc' cmap' clist
