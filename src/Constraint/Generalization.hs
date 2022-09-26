module Constraint.Generalization where

import qualified Constraint.InfiniteArray as IA
import qualified Constraint.OccursCheck as OC
import qualified Constraint.ShadowMap as SM
import Constraint.SolverM (SolverM)
import Constraint.Structure (Structure, isLeaf, leaf, projectNonLeaf)
import Constraint.Unifier (baseRank)
import qualified Constraint.Unifier as U
import qualified Constraint.UnionFind as U
import Constraint.Utils (modifySTRef, throwTypeError)
import Control.Monad (filterM, foldM, forM_, unless, when, zipWithM, zipWithM_)
import Control.Monad.ST.Trans (STRef, newSTRef, readSTRef, writeSTRef)

data Gen s = Gen
  { genPool :: IA.InfiniteArray s [U.Variable s],
    genYoung :: STRef s Int
  }

initGen :: SolverM s (Gen s)
initGen = do
  ia <- IA.new 8 []
  y <- newSTRef $ baseRank - 1
  return
    Gen
      { genPool = ia,
        genYoung = y
      }

register :: Gen s -> U.Variable s -> Int -> SolverM s ()
register (Gen {genPool = pool}) v r = do
  p <- IA.get pool r
  IA.set pool r (v : p)

flexible :: Gen s -> Maybe (Structure (U.Variable s)) -> SolverM s (U.Variable s)
flexible g@Gen {genYoung = young} so = do
  r <- readSTRef young
  d <- U.makeDesc so r U.Flexible
  v <- U.fresh d
  register g v r
  return v

rigid :: Gen s -> SolverM s (U.Variable s)
rigid g@Gen {genYoung = young} = do
  r <- readSTRef young
  d <- U.makeDesc leaf r U.Rigid
  v <- U.fresh d
  register g v r
  return v

enter :: Gen s -> SolverM s ()
enter (Gen {genPool = pool, genYoung = young}) = do
  modifySTRef young (+ 1)
  y <- readSTRef young
  p <- IA.get pool y
  unless (null p) $ throwTypeError "pool associated with the updated current rank must be empty"

data Generation s = Generation
  { inhabitants :: [U.Variable s],
    byRank :: IA.InfiniteArray s [U.Variable s],
    isYoung :: U.Variable s -> SolverM s Bool
  }

discoverYoungGeneration :: Gen s -> SolverM s (Generation s)
discoverYoungGeneration (Gen {genPool = pool, genYoung = young}) = do
  y <- readSTRef young
  inhs <- IA.get pool y
  table <- SM.new
  arr <- IA.new (y + 1) []
  let f v = do
        d <- U.descriptor v
        SM.replace table (U.descId d) ()
        r <- readSTRef (U.descRank d)
        st <- readSTRef (U.descStatus d)
        unless (st /= U.Generic) $ throwTypeError "there shouldn't be generic type variables in pool"
        unless (U.baseRank <= r && r <= y) $ throwTypeError $ "illegal rank: " ++ show r
        IA.update arr r (v :)
  mapM_ f inhs
  let isy v = do
        d <- U.descriptor v
        SM.member table (U.descId d)
  return
    Generation
      { inhabitants = inhs,
        byRank = arr,
        isYoung = isy
      }

updateRanks :: Gen s -> Generation s -> SolverM s ()
updateRanks (Gen {genYoung = young}) generation = do
  y <- readSTRef young
  visited <- U.freshMark
  forM_
    [baseRank .. y]
    ( \k ->
        let trav v = do
              d <- U.descriptor v
              st <- readSTRef (U.descStatus d)
              unless (st /= U.Generic) $ throwTypeError "downward propagation can never reach a generic variable"
              m <- readSTRef (U.descMark d)
              if m == visited
                then do
                  r <- readSTRef (U.descRank d)
                  unless (r <= k) $ throwTypeError "visited type variable has lower rank than expected"
                else do
                  writeSTRef (U.descMark d) visited
                  U.adjustRank d k
                  isy <- isYoung generation v
                  when isy $ do
                    r <- readSTRef (U.descRank d)
                    unless (r == k) $ throwTypeError $ "rank must be equal to " ++ show k
                    struc <- readSTRef (U.descStructure d)
                    unless (isLeaf struc) $ do
                      let s = projectNonLeaf struc
                      r' <-
                        foldM
                          ( \accu child -> do
                              r' <- trav child
                              return $ max r' accu
                          )
                          U.baseRank
                          s
                      U.adjustRank d r'
              readSTRef (U.descRank d)
         in do
              l <- IA.get (byRank generation) k
              mapM_ trav l
    )

generalize :: Gen s -> Generation s -> SolverM s [U.Variable s]
generalize g@(Gen {genYoung = young}) generation =
  let f v = do
        y <- readSTRef young
        b1 <- not <$> U.redundant v
        b2 <- do
          d <- U.descriptor v
          r <- readSTRef (U.descRank d)
          if r < y
            then register g v r >> return False
            else do
              unless (r == y) $ throwTypeError "generalize: rank must be the same as young"
              writeSTRef (U.descStatus d) U.Generic
              struc <- readSTRef (U.descStructure d)
              return $ isLeaf struc
        return $ b1 && b2
   in filterM f (inhabitants generation)

data Scheme s = Scheme
  { schemeRoot :: U.Variable s,
    schemeGenerics :: [U.Variable s],
    schemeQuantifiers :: [U.Variable s]
  }

trivial :: U.Variable s -> Scheme s
trivial root =
  Scheme
    { schemeRoot = root,
      schemeGenerics = [],
      schemeQuantifiers = []
    }

schemify :: U.Variable s -> SolverM s (Scheme s)
schemify root = do
  visited <- U.freshMark
  accu <- newSTRef []
  let trav v = do
        d <- U.descriptor v
        st <- readSTRef (U.descStatus d)
        m <- readSTRef (U.descMark d)
        when (st == U.Generic && m /= visited) $ do
          writeSTRef (U.descMark d) visited
          modifySTRef accu (v :)
          struc <- readSTRef (U.descStructure d)
          mapM_ (mapM_ trav) struc
  trav root
  generics <- readSTRef accu
  let hasStructure v = do
        d <- U.descriptor v
        struc <- readSTRef (U.descStructure d)
        return $ isLeaf struc
  quantifiers <- filterM hasStructure generics
  return
    Scheme
      { schemeRoot = root,
        schemeGenerics = generics,
        schemeQuantifiers = quantifiers
      }

exit :: Gen s -> [U.Variable s] -> SolverM s ([U.Variable s], [Scheme s])
exit g@Gen {genPool = pool, genYoung = young} roots = do
  y <- readSTRef young
  unless (y >= U.baseRank) $ throwTypeError "more exits than enters"
  generation <- discoverYoungGeneration g
  updateRanks g generation

  let isGeneralizable d = (==) <$> readSTRef (U.descRank d) <*> pure y
  trav <- OC.newOccursCheck isGeneralizable
  l <- IA.get (byRank generation) y
  mapM_ trav l

  quantifiers <- generalize g generation
  schemes <- mapM schemify roots
  IA.set pool y []
  modifySTRef young (\y' -> y' - 1)
  return (quantifiers, schemes)

instantiate :: Gen s -> Scheme s -> SolverM s ([U.Variable s], U.Variable s)
instantiate g Scheme {schemeGenerics = generics, schemeQuantifiers = quantifiers, schemeRoot = root} =
  do
    mapping <- do
      zipWithM
        ( \i v -> do
            d <- U.descriptor v
            st <- readSTRef $ U.descStatus d
            unless (st == U.Generic) $ throwTypeError "can only instantiate generic type variables"
            writeSTRef (U.descMark d) i
            flexible g leaf
        )
        [0 .. length generics]
        generics
    let copy v = do
          d <- U.descriptor v
          st <- readSTRef $ U.descStatus d
          if st == U.Generic
            then do
              i <- readSTRef $ U.descMark d
              return $ mapping !! i
            else return v
    zipWithM_
      ( \v v' -> do
          d <- U.descriptor v
          struc <- readSTRef $ U.descStructure d
          copiedStruc <- mapM (mapM copy) struc
          d' <- U.descriptor v'
          writeSTRef (U.descStructure d') copiedStruc
      )
      generics
      mapping

    qs' <- mapM copy quantifiers
    root' <- copy root

    return (qs', root')
