module IR.Pattern.Vector where

import qualified IR.IR as I


data PatVec t = PatVec
  { ncol :: Int
  , toList :: [I.Alt t]
  }
  deriving (Eq, Show)


fromList :: [I.Alt t] -> PatVec t
fromList ps = let n = length ps in PatVec{ncol = n, toList = ps}


singleton :: I.Alt t -> PatVec t
singleton p = PatVec{ncol = 1, toList = [p]}


extend :: PatVec t -> PatVec t -> PatVec t
extend pv1 pv2 =
  let n' = ncol pv1 + ncol pv2
      l' = toList pv1 ++ toList pv2
   in PatVec{ncol = n', toList = l'}


hd :: PatVec t -> I.Alt t
hd pv = case toList pv of
  (x : _) -> x
  _ -> error "head on empty list"


tl :: PatVec t -> PatVec t
tl pv = PatVec{ncol = ncol pv - 1, toList = tail $ toList pv}


-- TODO: double check that this is correct
specialize :: PatVec t -> PatVec t
specialize pv = case hd pv of
  I.AltLit _ _ -> tl pv
  I.AltData _ [] _ -> tl pv
  I.AltData _ ps _ -> extend (fromList ps) (tl pv)
  I.AltBinder _ -> error "wrong usage"


specializeWild :: Int -> PatVec t -> PatVec t
specializeWild arity pv = case hd pv of
  I.AltBinder b -> wildCase
    where
      wildCase =
        let binder = I.BindAnon $ I.extract b
            pv1 = fromList $ replicate arity (I.AltBinder binder)
            pv2 = tl pv
        in extend pv1 pv2
  _ -> error "wrong usage"

