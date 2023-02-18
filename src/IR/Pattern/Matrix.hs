module IR.Pattern.Matrix where

import Common.Identifiers (
  Identifier (..),
 )
import qualified IR.IR as I
import qualified IR.Pattern.Vector as PV


data PatMat = PatMat
  { nrow :: Int
  , ncol :: Int
  , toList :: [PV.PatVec]
  }
  deriving (Eq, Show)


singleCol :: [I.Alt] -> PatMat
singleCol ps =
  let m = length ps
      pvs = [PV.singleton p | p <- ps]
   in PatMat{nrow = m, ncol = 1, toList = pvs}


singleRow :: [I.Alt] -> PatMat
singleRow ps =
  let n = length ps
      pvs = [PV.fromList ps]
   in PatMat{nrow = 1, ncol = n, toList = pvs}


fromPatVec :: PV.PatVec -> PatMat
fromPatVec pv = PatMat{nrow = 1, ncol = PV.ncol pv, toList = [pv]}


fromPatVecs :: [PV.PatVec] -> PatMat
fromPatVecs [] = error "Not enough information to construct pattern matrix"
fromPatVecs pvs =
  let emptyPatMat = emptyWithCols $ let pv : _ = pvs in PV.ncol pv
   in foldl (\pm pv -> extend pm (fromPatVec pv)) emptyPatMat pvs


emptyWithCols :: Int -> PatMat
emptyWithCols n = PatMat{nrow = 0, ncol = n, toList = []}


hd :: PatMat -> PV.PatVec
hd = head . toList


extend :: PatMat -> PatMat -> PatMat
extend pm1 pm2 =
  let m' = nrow pm1 + nrow pm2
      n' = ncol pm1
      l' = toList pm1 ++ toList pm2
   in PatMat{nrow = m', ncol = n', toList = l'}


specializeLit :: I.Literal -> PatMat -> PatMat
specializeLit lit pm =
  let pms = map specializeLit' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (ncol pm - 1)
  specializeLit' pv =
    let wildCase = fromPatVec $ PV.tl pv
     in case PV.hd pv of
          I.AltLit lit' ->
            if lit' == lit then fromPatVec $ PV.tl pv else noRow
          I.AltBinder Nothing -> wildCase
          I.AltBinder _ -> wildCase
          I.AltData _ [] -> noRow
          I.AltData _ _ -> noRow


specializeCons :: Int -> Identifier -> PatMat -> PatMat
specializeCons arity i pm =
  let pms = map specializeCons' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (arity + ncol pm - 1)
  specializeCons' pv =
    let wildCase =
          let pv1 = PV.fromList (replicate arity (I.AltBinder Nothing))
              pv2 = PV.tl pv
           in fromPatVec $ PV.extend pv1 pv2
     in case PV.hd pv of
          I.AltLit _ -> noRow
          I.AltBinder Nothing -> wildCase
          I.AltBinder _ -> wildCase
          I.AltData (I.DConId i') [] ->
            if i' == i then fromPatVec $ PV.tl pv else noRow
          I.AltData _ _ -> noRow


defaultize :: PatMat -> PatMat
defaultize pm =
  let pms = map defaultize' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (ncol pm - 1)
  defaultize' pv =
    let wildCase = fromPatVec $ PV.tl pv
     in case PV.hd pv of
          I.AltLit _ -> noRow
          I.AltBinder Nothing -> wildCase
          I.AltBinder _ -> wildCase
          I.AltData _ [] -> noRow
          I.AltData _ _ -> noRow
