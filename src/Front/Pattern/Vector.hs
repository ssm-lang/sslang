module Front.Pattern.Vector where

import Common.Identifiers (isVar)
import qualified Front.Ast as A


data PatVec = PatVec
  { ncol :: Int
  , toList :: [A.Pat]
  }
  deriving (Eq, Show)


fromList :: [A.Pat] -> PatVec
fromList ps = let n = length ps in PatVec{ncol = n, toList = ps}


singleton :: A.Pat -> PatVec
singleton p = PatVec{ncol = 1, toList = [p]}


extend :: PatVec -> PatVec -> PatVec
extend pv1 pv2 =
  let n' = ncol pv1 + ncol pv2
      l' = toList pv1 ++ toList pv2
   in PatVec{ncol = n', toList = l'}


append :: A.Pat -> PatVec -> PatVec
append p pv =
  let n' = 1 + ncol pv
      l' = p : toList pv
   in PatVec{ncol = n', toList = l'}


hd :: PatVec -> A.Pat
hd = head . toList


tl :: PatVec -> PatVec
tl pv = PatVec{ncol = ncol pv - 1, toList = tail $ toList pv}


{-
From "Warnings for Pattern Matching"
-}
specialize :: PatVec -> PatVec
specialize pv = case hd pv of
  A.PatWildcard -> error "wrong usage"
  A.PatId i -> if isVar i then error "wrong usage" else tl pv
  A.PatLit _ -> tl pv
  A.PatTup ps -> fromList ps `extend` tl pv
  A.PatApp ((A.PatId _) : ps) -> fromList ps `extend` tl pv
  _ -> error "wrong usage"


specializeWild :: Int -> PatVec -> PatVec
specializeWild arity pv = case hd pv of
  A.PatWildcard -> wildCase
  A.PatId i -> if isVar i then wildCase else error "wrong usage"
  _ -> error "wrong usage"
 where
  wildCase =
    let pv1 = fromList $ replicate arity A.PatWildcard
        pv2 = tl pv
     in pv1 `extend` pv2
