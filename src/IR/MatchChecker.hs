-- | Pattern match anomaly checker
-- | Based on: "Warnings for pattern matching" by Luc Maranget
-- |
-- | Checks: non-exhaustion, useless clause

module IR.MatchChecker
    ( C(..)
    , CSet(..)
    , P(..)
    , PV(..)
    , PM(..)
    , useful
    , isExhaustive
    , uselessClause
    , cSet
    , fromListPV
    , fromListPM
    , isSubCSet
    , eqCSet
    , specializedPM
    , specializedPV
    , defaultPM
    , justPV
    , tailPV
    ) where

import           Common.Identifiers                       ( DConId )

data C = C
    { getArity :: Int
    , getC     :: DConId
    }
    deriving (Eq, Show)

newtype CSet = CSet { getCSetL :: [C]}

data P
  = PWild
  | PCon C [P]
  | POr P P
 deriving (Eq ,Show)

data PV = PV
    { getPVN :: Int -- number of elements
    , getPVL :: [P] -- list of patterns
    }
    deriving (Eq, Show)

data PM = PM
    { getPMM :: Int -- number of rows
    , getPMN :: Int -- number of columns
    , getPML :: [PV] -- list of PV
    }
    deriving (Eq, Show)

isExhaustive :: (CSet -> Bool) -> [P] -> Bool
isExhaustive isCompleteCSet ps =
    let pm = fromListPM ps in not $ useful isCompleteCSet pm (justPV PWild)

uselessClause :: (CSet -> Bool) -> [P] -> Maybe P
uselessClause isCompleteCSet ps =
    let n      = length ps
        trials = [1 .. n]
        pms    = [ (ntrial, fromListPM $ take ntrial ps) | ntrial <- trials ]
        uselessOnes =
            [ ps !! ntrial
            | (ntrial, pm) <- pms
            , useful isCompleteCSet pm (justPV (ps !! ntrial))
            ]
    in  case uselessOnes of
            []      -> Nothing
            (p : _) -> Just p

useful :: (CSet -> Bool) -> PM -> PV -> Bool
useful isCompleteCSet pm pv | getPMN pm < 0  = undefined
                            | getPMN pm == 0 = baseCase
                            | otherwise      = inductiveCase
  where
    useful'       = useful isCompleteCSet
    baseCase      = getPMM pm == 0
    inductiveCase = case head (getPVL pv) of
        PCon c _ -> useful' (specializedPM c pm) (specializedPV c pv)
        PWild ->
            let cset = cSet pm
                f ck = useful' (specializedPM ck pm) (specializedPV ck pv)
            in  if isCompleteCSet cset
                    then or $ mapCSet f cset
                    else useful' (defaultPM pm) (tailPV pv)
        POr r1 r2 ->
            let u1 = useful' pm (r1 `consP` tailPV pv)
                u2 = useful' pm (r2 `consP` tailPV pv)
            in  u1 || u2

specializedPM :: C -> PM -> PM
specializedPM c pm =
    let
        width = getArity c + getPMN pm - 1
        specializedRowAcc :: PV -> PM -> PM
        specializedRowAcc pv pmAcc = case head (getPVL pv) of
            PCon c' _ -> if c' == c
                then
                     -- case 1
                     (let sVec = specializedPV c pv in sVec `consPV` pmAcc)
                else
                     -- case 2
                     pmAcc

            PWild ->
                -- case 3
                let sVec = specializedPV c pv in sVec `consPV` pmAcc
            POr r1 r2 ->
                -- case 4
                let
                    n    = getPVN pv
                    row1 = PV n (r1 : tail (getPVL pv))
                    row2 = PV n (r2 : tail (getPVL pv))
                    rows = specializedPM
                        c
                        (row1 `consPV` (row2 `consPV` emptyPM n))
                in
                    rows `appendPM` pmAcc
    in
        foldr specializedRowAcc (emptyPM width) (getPML pm)

specializedPV :: C -> PV -> PV
specializedPV c pv =
    let width = getArity c + getPVN pv - 1
    in  case head (getPVL pv) of
            PCon c' rs ->
                if c' == c then PV width (rs ++ tail (getPVL pv)) else undefined
            PWild ->
                let sPats = replicate (getArity c) PWild ++ tail (getPVL pv)
                in  PV width sPats
            POr _ _ -> undefined

defaultPM :: PM -> PM
defaultPM pm =
    let
        width = getPMN pm - 1
        defaultRowAcc :: PV -> PM -> PM
        defaultRowAcc pv pmAcc = case head (getPVL pv) of
            PCon _ _ -> pmAcc
            PWild    -> let ps = tail $ getPVL pv in PV width ps `consPV` pmAcc
            POr r1 r2 ->
                let
                    n    = getPVN pv
                    row1 = PV n (r1 : tail (getPVL pv))
                    row2 = PV n (r2 : tail (getPVL pv))
                    rows = defaultPM $ row1 `consPV` (row2 `consPV` emptyPM n)
                in
                    rows `appendPM` pmAcc
    in
        foldr defaultRowAcc (emptyPM width) (getPML pm)

cSet :: PM -> CSet
cSet pm = CSet $ concatMap getPVC (getPML pm)
  where
    getPC (PCon c _)  = [c]
    getPC PWild       = []
    getPC (POr r1 r2) = getPC r1 ++ getPC r2
    getPVC pv = getPC (head $ getPVL pv)


consPV :: PV -> PM -> PM
consPV pVec pm = PM (1 + getPMM pm) (getPMN pm) (pVec : getPML pm)

emptyPM :: Int -> PM
emptyPM n = PM 0 n []

appendPM :: PM -> PM -> PM
appendPM pm1 pm2 =
    let m' = (getPMM pm1) + (getPMM pm2)
        l' = (getPML pm1) ++ (getPML pm2)
    in  PM m' (getPMN pm1) l'

consP :: P -> PV -> PV
consP p pv = PV (1 + getPVN pv) (p : getPVL pv)

tailPV :: PV -> PV
tailPV pv = PV (getPVN pv - 1) (tail (getPVL pv))

mapCSet :: (C -> b) -> CSet -> [b]
mapCSet f (CSet l) = map f l

fromListPV :: [P] -> PV
fromListPV ps = let n = length ps in PV { getPVN = n, getPVL = ps }

fromListPM :: [P] -> PM
fromListPM ps =
    let m   = length ps
        pvs = map (\p -> PV { getPVN = 1, getPVL = [p] }) ps
    in  PM { getPMM = m, getPMN = 1, getPML = pvs }

justPV :: P -> PV
justPV p = PV { getPVN = 1, getPVL = [p] }

isSubCSet :: CSet -> CSet -> Bool
isSubCSet cs1 cs2 = all (`elem` getCSetL cs2) (getCSetL cs1)

eqCSet :: CSet -> CSet -> Bool
eqCSet cs1 cs2 = (cs1 `isSubCSet` cs2) && (cs2 `isSubCSet` cs1)
