{- | Generalization and Instantiation -}

module IR.Types.Constraint.Generalize where

import           Control.Monad                  ( foldM
                                                , forM_
                                                , unless
                                                , when, zipWithM_, zipWithM
                                                )
import           Control.Monad                  ( filterM )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.ST.Trans         ( newSTRef
                                                , readSTRef
                                                )
import qualified IR.Types.Constraint.InfiniteArray
                                               as IA
import qualified IR.Types.Constraint.OccursCheck
                                               as OC
import qualified IR.Types.Constraint.ShadowMap as SM
import           IR.Types.Constraint.Type       ( Infer
                                                , Rank
                                                , Status(..)
                                                , Structure
                                                , UScheme(..)
                                                , UVar
                                                , freshMark
                                                , getDescriptor
                                                , getMark
                                                , getPool
                                                , getRank
                                                , getStatus
                                                , getStructure
                                                , getTag
                                                , getYoungRank
                                                , incYoungRank
                                                , internalError
                                                , isLeaf
                                                , leaf
                                                , makeDescriptor
                                                , makeUVar
                                                , outermostRank
                                                , projectNonLeaf
                                                , setMark
                                                , setRank
                                                , setStatus, setStructure, decYoungRank
                                                )
import qualified IR.Types.Constraint.UnionFind as UF
import           IR.Types.Constraint.Utils      ( modifySTRef )



-- | Rank logic for UVar's


register :: UVar s -> Rank -> Infer s ()
register v r = do
  pool <- getPool
  p    <- IA.get pool r
  IA.set pool r (v : p)

flexible :: Structure (UVar s) -> Infer s (UVar s)
flexible s = do
  youngRank <- getYoungRank
  d         <- makeDescriptor Nothing s youngRank Flexible
  v         <- makeUVar d
  register v youngRank
  return v

rigid :: Infer s (UVar s)
rigid = do
  youngRank <- getYoungRank
  d         <- makeDescriptor Nothing leaf youngRank Rigid
  v         <- makeUVar d
  register v youngRank
  return v



-- | Housekeeping for entering a CLet


enter :: Infer s ()
enter = do
  pool <- getPool
  incYoungRank
  youngRank <- getYoungRank
  -- sanity check
  p         <- IA.get pool youngRank
  unless (null p) $ throwError $ internalError
    "pool associated with the updated current rank must be empty"



-- | Generalization


data Generation s = Generation
  { inhabitants :: [UVar s]
  , byRank      :: IA.InfiniteArray s [UVar s]
  , isYoung     :: UVar s -> Infer s Bool
  }


discoverYoungGeneration :: Infer s (Generation s)
discoverYoungGeneration = do
  pool      <- getPool
  youngRank <- getYoungRank

  inhs      <- IA.get pool youngRank
  table     <- SM.new
  arr       <- IA.new (youngRank + 1) []

  let addToArr v = do
        d <- getDescriptor v
        SM.replace table (getTag d) ()
        r  <- getRank d
        st <- getStatus d
        unless (st /= Generic) $ throwError $ internalError
          "there shouldn't be generic type variables in pool"
        unless (outermostRank <= r && r <= youngRank)
          $  throwError
          $  internalError
          $  "illegal rank: "
          ++ show r
        IA.update arr r (v :)

  mapM_ addToArr inhs

  let isy v = do
        d <- getDescriptor v
        SM.member table (getTag d)

  return Generation { inhabitants = inhs, byRank = arr, isYoung = isy }


updateRanks :: Generation s -> Infer s ()
updateRanks generation = do
  youngRank <- getYoungRank
  visited   <- freshMark
  forM_
    [outermostRank .. youngRank]
    (\k ->
      let trav v = do
            d  <- getDescriptor v
            st <- getStatus d
            unless (st /= Generic) $ throwError $ internalError
              "downward propagation can never reach a generic variable"
            m <- getMark d
            if m == visited
              then do
                r <- getRank d
                unless (r <= k) $ throwError $ internalError
                  "visited type variable has lower rank than expected"
              else do
                setMark d visited
                setRank d k
                isy <- isYoung generation v
                when isy $ do
                  r <- getRank d
                  unless (r == k)
                    $  throwError
                    $  internalError
                    $  "rank must be equal to "
                    ++ show k
                  struc <- getStructure d
                  unless (isLeaf struc) $ do
                    let s = projectNonLeaf struc
                    r' <- foldM
                      (\accu child -> do
                        r' <- trav child
                        return $ max r' accu
                      )
                      outermostRank
                      s
                    setRank d r'
            getRank d
      in  do
            l <- IA.get (byRank generation) k
            mapM_ trav l
    )


generalize :: Generation s -> Infer s [UVar s]
generalize generation = do
  filterM canGen (inhabitants generation)
 where
  canGen v = do
    youngRank <- getYoungRank
    b1        <- not <$> UF.redundant v
    b2        <- do
      d <- getDescriptor v
      r <- getRank d
      if r < youngRank
        then do
          register v r
          return False
        else do
          unless (r == youngRank) $ throwError $ internalError
            "generalize: rank must be the same as young"
          setStatus d Generic
          struc <- getStructure d
          return $ isLeaf struc
    return $ b1 && b2



-- | UScheme utilities


-- a trivial scheme is semantically like a type
trivial :: UVar s -> UScheme s
trivial root =
  UScheme { uschemeRoot = root, uschemeGenerics = [], uschemeQuantifiers = [] }

-- generalize a UVar into a UScheme
schemify :: UVar s -> Infer s (UScheme s)
schemify root = do
  visited <- freshMark
  accu    <- newSTRef []

  -- traverse union-find graph
  let trav v = do
        d  <- getDescriptor v
        st <- getStatus d
        m  <- getMark d
        when (st == Generic && m /= visited) $ do
          setMark d visited
          modifySTRef accu (v :)
          struc <- getStructure d
          mapM_ (mapM_ trav) struc

  trav root
  generics <- readSTRef accu

  -- check whether v has some structure
  let hasStructure v = do
        d     <- getDescriptor v
        struc <- getStructure d
        return $ isLeaf struc

  quantifiers <- filterM hasStructure generics

  return UScheme { uschemeRoot        = root
                 , uschemeGenerics    = generics
                 , uschemeQuantifiers = quantifiers
                 }

-- | A lot of housekeeping for exiting a CLet

exit :: [UVar s] -> Infer s ([UVar s], [UScheme s])
exit roots = do
  pool      <- getPool
  youngRank <- getYoungRank
  unless (youngRank >= outermostRank) $ throwError $ internalError "more exits than enters"
  generation <- discoverYoungGeneration
  updateRanks generation

  let isGeneralizable d = (==) <$> getRank d <*> pure youngRank
  trav <- OC.newOccursCheck isGeneralizable
  l    <- IA.get (byRank generation) youngRank
  mapM_ trav l

  quantifiers <- generalize generation
  schemes     <- mapM schemify roots
  IA.set pool youngRank []
  decYoungRank
  return (quantifiers, schemes)


-- | Instianting UScheme into a UVar, along with the instantiated quantifiers

instantiate :: UScheme s -> Infer s ([UVar s], UVar s)
instantiate UScheme { uschemeGenerics = generics, uschemeQuantifiers = quantifiers, uschemeRoot = root }
  = do
    mapping <- do
      zipWithM
        (\i v -> do
          d  <- getDescriptor v
          st <- getStatus d
          unless (st == Generic)
            $ throwError $ internalError "can only instantiate generic type variables"
          setMark d i
          flexible leaf
        )
        [0 .. length generics]
        generics
    let copy v = do
          d  <- getDescriptor v
          st <- getStatus d
          if st == Generic
            then do
              i <- getMark d
              return $ mapping !! i
            else return v
    zipWithM_
      (\v v' -> do
        d           <- getDescriptor v
        struc       <- getStructure d
        copiedStruc <- mapM (mapM copy) struc
        d'          <- getDescriptor v'
        setStructure d' copiedStruc
      )
      generics
      mapping

    qs'   <- mapM copy quantifiers
    root' <- copy root

    return (qs', root')
