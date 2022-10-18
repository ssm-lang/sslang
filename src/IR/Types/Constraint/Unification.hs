{- | Unification of structures

This is an implementation of unification. It uses the UnionFind module.
-}

module IR.Types.Constraint.Unification
  ( unify
  ) where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.ST.Trans         ( STRef
                                                , newSTRef
                                                , readSTRef
                                                , writeSTRef
                                                )
import           IR.Types.Constraint.Type       ( Descriptor(..)
                                                , Infer
                                                , Rank
                                                , Status(..)
                                                , Structure
                                                , UType(..)
                                                , UVar
                                                , getRank
                                                , getStatus
                                                , getStructure
                                                , getTag
                                                , internalError
                                                , isLeaf
                                                , makeDescriptor
                                                )
import           IR.Types.Constraint.UnionFind  ( equivalent
                                                , union'
                                                )



-- | Unify two unification variables


unify :: UVar s -> UVar s -> Infer s ()
unify v1 v2 = do
  q <- makeQueue
  unify' q v1 v2



-- | Internal implementations of 'unify'


unify' :: Queue s -> UVar s -> UVar s -> Infer s ()
unify' qref v1 v2 = do
  union' v1 v2 (unifyDescriptor qref)
  unifyPending qref


unifyPending :: Queue s -> Infer s ()
unifyPending qref = do
  x <- pop qref
  case x of
    Just (v1, v2) -> unify' qref v1 v2
    Nothing       -> return ()



-- | Unify descriptors


unifyDescriptor :: Queue s -> Descriptor s -> Descriptor s -> Infer s (Descriptor s)
unifyDescriptor q d1 d2 = do
  struc  <- unifyStructure q d1 d2
  rank   <- unifyRank d1 d2
  status <- unifyStatus d1 d2 rank struc
  makeDescriptor (Just $ getTag d1) struc rank status


unifyStructure
  :: Queue s -> Descriptor s -> Descriptor s -> Infer s (Structure (UVar s))
unifyStructure q d1 d2 = do
  s1 <- getStructure d1
  s2 <- getStructure d2
  mergeStructure (insert q) s1 s2


unifyRank :: Descriptor s -> Descriptor s -> Infer s Rank
unifyRank d1 d2 = do
  r1 <- getRank d1
  r2 <- getRank d2
  return $ min r1 r2


unifyStatus
  :: Descriptor s
  -> Descriptor s
  -> Rank
  -> Structure (UVar s)
  -> Infer s Status
unifyStatus d1 d2 rank struc = do
  s1 <- getStatus d1
  r1 <- getRank d1
  s2 <- getStatus d2
  r2 <- getRank d2
  case (s1, r1, s2, r2) of
    (Generic, _, _, _) ->
      throwError $ internalError "can't unify generic type variables"
    (_, _, Generic, _) ->
      throwError $ internalError "can't unify generic type variables"
    (Flexible, _, Flexible, _) -> return Flexible
    (Rigid, _, Rigid, _) ->
      throwError $ internalError "can't unify two rigid type variables"
    (Rigid   , rigidRank, Flexible, _        ) -> oneRigidCase rigidRank
    (Flexible, _        , Rigid   , rigidRank) -> oneRigidCase rigidRank
 where
  oneRigidCase :: Rank -> Infer s Status
  oneRigidCase rigidRank = do
    when (rank < rigidRank) $ throwError $ internalError
      "can't lower rank of a rigid variable"
    unless (isLeaf struc) $ throwError $ internalError
      "can't assign non-leaf structure to a rigid variable"
    return Rigid



-- | Merge structures


mergeStructure
  :: (UVar s -> UVar s -> Infer s ())
  -> Structure (UVar s)
  -> Structure (UVar s)
  -> Infer s (Structure (UVar s))
mergeStructure _ Nothing   so        = return so
mergeStructure _ so        Nothing   = return so
mergeStructure f (Just s1) (Just s2) = Just <$> mergeUType f s1 s2


mergeUType
  :: (UVar s -> UVar s -> Infer s ())
  -> UType (UVar s)
  -> UType (UVar s)
  -> Infer s (UType (UVar s))
mergeUType f s1@(UTCon m ls) (UTCon n rs)
  | m /= n
  = throwError
    $  internalError
    $  "unable to unify "
    ++ show m
    ++ " with "
    ++ show n
  | otherwise
  = do
    mergeList f ls rs
    return s1


mergeList
  :: (UVar s -> UVar s -> Infer s ()) -> [UVar s] -> [UVar s] -> Infer s ()
mergeList f l1 l2
  | length l1 /= length l2 = throwError $ internalError
    "unable to unify due to different number of type constructor parameters"
  | otherwise = mapM_ (uncurry f) (zip l1 l2)



-- | Queue data structure, used internally by the 'unify' function


type Queue s = STRef s [(UVar s, UVar s)]


makeQueue :: Infer s (Queue s)
makeQueue = newSTRef []


pop :: Queue s -> Infer s (Maybe (UVar s, UVar s))
pop qref = do
  q <- readSTRef qref
  case q of
    []       -> return Nothing
    (x : xs) -> do
      writeSTRef qref xs
      return $ Just x


insert :: Queue s -> UVar s -> UVar s -> Infer s ()
insert qref v1 v2 = do
  q    <- readSTRef qref
  iseq <- equivalent v1 v2
  unless iseq $ writeSTRef qref ((v1, v2) : q)
