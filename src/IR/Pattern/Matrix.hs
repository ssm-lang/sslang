module IR.Pattern.Matrix where

import Common.Identifiers (
  Identifier (..),
 )
import qualified IR.IR as I
import qualified IR.Pattern.Vector as PV


data PatMat t = PatMat
  { nrow :: Int
  , ncol :: Int
  , toList :: [PV.PatVec t]
  }
  deriving (Eq, Show)


singleCol :: [I.Alt t] -> PatMat t
singleCol ps =
  let m = length ps
      pvs = [PV.singleton p | p <- ps]
   in PatMat{nrow = m, ncol = 1, toList = pvs}


singleRow :: [I.Alt t] -> PatMat t
singleRow ps =
  let n = length ps
      pvs = [PV.fromList ps]
   in PatMat{nrow = 1, ncol = n, toList = pvs}


fromPatVec :: PV.PatVec t -> PatMat t
fromPatVec pv = PatMat{nrow = 1, ncol = PV.ncol pv, toList = [pv]}


fromPatVecs :: [PV.PatVec t] -> PatMat t
fromPatVecs [] = error "Not enough information to construct pattern matrix"
fromPatVecs pvs =
  let emptyPatMat = emptyWithCols $ let pv : _ = pvs in PV.ncol pv
   in foldl (\pm pv -> extend pm (fromPatVec pv)) emptyPatMat pvs


emptyWithCols :: Int -> PatMat t
emptyWithCols n = PatMat{nrow = 0, ncol = n, toList = []}


hd :: PatMat t -> PV.PatVec t
hd = head . toList


extend :: PatMat t -> PatMat t -> PatMat t
extend pm1 pm2 =
  let m' = nrow pm1 + nrow pm2
      n' = ncol pm1
      l' = toList pm1 ++ toList pm2
   in PatMat{nrow = m', ncol = n', toList = l'}


specializeLit :: I.Literal -> PatMat t -> PatMat t
specializeLit lit pm =
  let pms = map specializeLit' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (ncol pm - 1)
  specializeLit' pv =
    let wildCase = fromPatVec $ PV.tl pv
     in case PV.hd pv of
          I.AltLit lit' _ ->
            if lit' == lit then fromPatVec $ PV.tl pv else noRow
          I.AltBinder _ -> wildCase
          I.AltData{} -> noRow


specializeCons :: Int -> Identifier -> PatMat t -> PatMat t
specializeCons arity i pm =
  let pms = map specializeCons' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (arity + ncol pm - 1)
  specializeCons' pv = case PV.hd pv of
    I.AltLit _ _ -> noRow
    I.AltBinder b@I.BindAnon{} ->
      let pv1 = PV.fromList $ replicate arity (I.AltBinder b)
          pv2 = PV.tl pv
       in fromPatVec $ PV.extend pv1 pv2
    I.AltBinder _ -> noRow
    I.AltData (I.DConId i') [] _ ->
      if i' == i then fromPatVec $ PV.tl pv else noRow
    I.AltData (I.DConId i') ps _ ->
      if i' == i
        then
          let pv1 = PV.fromList ps
              pv2 = PV.tl pv
           in fromPatVec $ PV.extend pv1 pv2
        else noRow


defaultize :: PatMat t -> PatMat t
defaultize pm =
  let pms = map defaultize' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (ncol pm - 1)
  defaultize' pv =
    let wildCase = fromPatVec $ PV.tl pv
     in case PV.hd pv of
          I.AltLit{} -> noRow
          I.AltBinder{} -> wildCase
          I.AltData{} -> noRow
