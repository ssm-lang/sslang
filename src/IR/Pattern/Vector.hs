module IR.Pattern.Vector where

import Common.Identifiers (isVar)
import qualified IR.IR as I


data PatVec = PatVec
  { ncol :: Int
  , toList :: [I.Alt]
  }
  deriving (Eq, Show)


fromList :: [I.Alt] -> PatVec
fromList ps = let n = length ps in PatVec{ncol = n, toList = ps}


singleton :: I.Alt -> PatVec
singleton p = PatVec{ncol = 1, toList = [p]}


extend :: PatVec -> PatVec -> PatVec
extend pv1 pv2 =
  let n' = ncol pv1 + ncol pv2
      l' = toList pv1 ++ toList pv2
   in PatVec{ncol = n', toList = l'}


hd :: PatVec -> I.Alt
hd pv = case toList pv of
  (x : xs) -> x
  _ -> error "head on empty list"


tl :: PatVec -> PatVec
tl pv = PatVec{ncol = ncol pv - 1, toList = tail $ toList pv}


-- TODO: double check that this is correct
specialize :: PatVec -> PatVec
specialize pv = case hd pv of
  I.AltData i [] -> tl pv
  I.AltData i ps -> fromList ps `extend` tl pv
  I.AltBinder _ -> error "wrong usage"
  _ -> error "wrong usage"


specializeWild :: Int -> PatVec -> PatVec
specializeWild arity pv = case hd pv of
  I.AltBinder _ -> wildCase
  _ -> error "wrong usage"
 where
  wildCase =
    let pv1 = fromList $ replicate arity (I.AltBinder Nothing)
        pv2 = tl pv
     in pv1 `extend` pv2
