module Front.Pattern.Matrix where

import Common.Identifiers (
  Identifier (..),
  isVar,
 )
import qualified Front.Ast as A
import qualified Front.Pattern.Vector as PV


data PatMat = PatMat
  { nrow :: Int
  , ncol :: Int
  , toList :: [PV.PatVec] -- list of PV
  }
  deriving (Eq, Show)


singleCol :: [A.Pat] -> PatMat
singleCol ps =
  let m = length ps
      pvs = [PV.singleton p | p <- ps]
   in PatMat{nrow = m, ncol = 1, toList = pvs}


singleRow :: [A.Pat] -> PatMat
singleRow ps =
  let n = length ps
      pvs = [PV.fromList ps]
   in PatMat{nrow = 1, ncol = n, toList = pvs}


fromPatVec :: PV.PatVec -> PatMat
fromPatVec pv = PatMat{nrow = 1, ncol = PV.ncol pv, toList = [pv]}


emptyWithCols :: Int -> PatMat
emptyWithCols n = PatMat{nrow = 0, ncol = n, toList = []}


hd :: PatMat -> PV.PatVec
hd = head . toList


tl :: PatMat -> PatMat
tl pm =
  PatMat{nrow = nrow pm - 1, ncol = ncol pm, toList = tail $ toList pm}


extend :: PatMat -> PatMat -> PatMat
extend pm1 pm2 =
  let m' = nrow pm1 + nrow pm2
      n' = ncol pm1
      l' = toList pm1 ++ toList pm2
   in PatMat{nrow = m', ncol = n', toList = l'}


prepend :: PV.PatVec -> PatMat -> PatMat
prepend pv pm =
  let m' = 1 + nrow pm
      n' = ncol pm
      l' = pv : toList pm
   in PatMat{nrow = m', ncol = n', toList = l'}


specializeLit :: A.Literal -> PatMat -> PatMat
specializeLit lit pm =
  let pms = map specializeLit' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (ncol pm - 1)
  specializeLit' pv =
    let wildCase = fromPatVec $ PV.tl pv
        passThrough p =
          let pv' = p `PV.append` PV.tl pv
              pm' = fromPatVec pv'
           in specializeLit lit pm'
     in case PV.hd pv of
          A.PatLit lit' -> if lit' == lit then fromPatVec $ PV.tl pv else noRow
          A.PatId i -> if isVar i then wildCase else noRow
          A.PatWildcard -> wildCase
          A.PatAs _ p -> passThrough p
          A.PatAnn _ p -> passThrough p
          A.PatTup _ -> noRow
          A.PatApp _ -> noRow


specializeTup :: Int -> PatMat -> PatMat
specializeTup n pm =
  let pms = map specializeTup' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (n + ncol pm - 1)
  specializeTup' pv =
    let wildCase =
          let pv1 = PV.fromList (replicate n A.PatWildcard)
              pv2 = PV.tl pv
           in fromPatVec $ pv1 `PV.extend` pv2
        passThrough p =
          let pv' = p `PV.append` PV.tl pv
              pm' = fromPatVec pv'
           in specializeTup n pm'
     in case PV.hd pv of
          A.PatTup ps ->
            let pv1 = PV.fromList ps
                pv2 = PV.tl pv
             in fromPatVec $ pv1 `PV.extend` pv2
          A.PatId i -> if isVar i then wildCase else noRow
          A.PatWildcard -> wildCase
          A.PatAs _ p -> passThrough p
          A.PatAnn _ p -> passThrough p
          A.PatApp _ -> noRow
          A.PatLit _ -> noRow


specializeCons :: Int -> Identifier -> PatMat -> PatMat
specializeCons arity i pm =
  let pms = map specializeTup' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (arity + ncol pm - 1)
  specializeTup' pv =
    let wildCase =
          let pv1 = PV.fromList (replicate arity A.PatWildcard)
              pv2 = PV.tl pv
           in fromPatVec $ pv1 `PV.extend` pv2
        passThrough p =
          let pv' = p `PV.append` PV.tl pv
              pm' = fromPatVec pv'
           in specializeCons arity i pm'
     in case PV.hd pv of
          A.PatId i' ->
            if isVar i
              then wildCase
              else (if i' == i then fromPatVec $ PV.tl pv else noRow)
          A.PatApp ((A.PatId i') : ps) ->
            if i' == i
              then
                let pv1 = PV.fromList ps
                    pv2 = PV.tl pv
                 in fromPatVec $ pv1 `PV.extend` pv2
              else noRow
          A.PatApp _ -> error "can't happen"
          A.PatWildcard -> wildCase
          A.PatAs _ p -> passThrough p
          A.PatAnn _ p -> passThrough p
          A.PatLit _ -> noRow
          A.PatTup _ -> noRow


defaultize :: PatMat -> PatMat
defaultize pm =
  let pms = map defaultize' (toList pm) in foldr extend noRow pms
 where
  noRow = emptyWithCols (ncol pm - 1)
  defaultize' pv =
    let wildCase = fromPatVec $ PV.tl pv
        passThrough p =
          let pv' = p `PV.append` PV.tl pv
              pm' = fromPatVec pv'
           in defaultize pm'
     in case PV.hd pv of
          A.PatId i -> if isVar i then wildCase else noRow
          A.PatWildcard -> wildCase
          A.PatAs _ p -> passThrough p
          A.PatAnn _ p -> passThrough p
          A.PatTup _ -> noRow
          A.PatApp _ -> noRow
          A.PatLit _ -> noRow
