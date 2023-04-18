{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module IR.Constraint.UnionFind (
  Point,
  fresh,
  union,
  equivalent,
  redundant,
  get,
  set,
  modify,
) where

{- This is based on the following implementations:

  - https://hackage.haskell.org/package/union-find-0.2/docs/src/Data-UnionFind-IO.html
  - http://yann.regis-gianas.org/public/mini/code_UnionFind.html

It seems like the OCaml one came first, but I am not sure.

Compared to the Haskell implementation, the major changes here include:

  1. No more reallocating PointInfo when changing the weight
  2. Using the strict modifyIORef

-}

import Control.Monad (when)
import Control.Monad.Trans (
  MonadIO,
  liftIO,
 )
import Data.IORef (
  IORef,
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Word (Word32)


-- POINT

newtype Point a
  = Pt (IORef (PointInfo a))
  deriving (Eq)


data PointInfo a
  = Info {-# UNPACK #-} !(IORef Word32) {-# UNPACK #-} !(IORef a)
  | Link {-# UNPACK #-} !(Point a)


-- HELPERS

fresh :: MonadIO m => a -> m (Point a)
fresh value = do
  weight <- liftIO $ newIORef 1
  desc <- liftIO $ newIORef value
  link <- liftIO $ newIORef (Info weight desc)
  return (Pt link)


repr :: MonadIO m => Point a -> m (Point a)
repr point@(Pt ref) = do
  pInfo <- liftIO $ readIORef ref
  case pInfo of
    Info _ _ -> return point
    Link point1@(Pt ref1) -> do
      point2 <- repr point1
      when (point2 /= point1) $ do
        pInfo1 <- liftIO $ readIORef ref1
        liftIO $ writeIORef ref pInfo1
      return point2


get :: MonadIO m => Point a -> m a
get point@(Pt ref) = do
  pInfo <- liftIO $ readIORef ref
  case pInfo of
    Info _ descRef -> liftIO $ readIORef descRef
    Link (Pt ref1) -> do
      link' <- liftIO $ readIORef ref1
      case link' of
        Info _ descRef -> liftIO $ readIORef descRef
        Link _ -> get =<< repr point


set :: MonadIO m => Point a -> a -> m ()
set point@(Pt ref) newDesc = do
  pInfo <- liftIO $ readIORef ref
  case pInfo of
    Info _ descRef -> liftIO $ writeIORef descRef newDesc
    Link (Pt ref1) -> do
      link' <- liftIO $ readIORef ref1
      case link' of
        Info _ descRef -> liftIO $ writeIORef descRef newDesc
        Link _ -> do
          newPoint <- repr point
          set newPoint newDesc


modify :: MonadIO m => Point a -> (a -> a) -> m ()
modify point@(Pt ref) func = do
  pInfo <- liftIO $ readIORef ref
  case pInfo of
    Info _ descRef -> liftIO $ modifyIORef' descRef func
    Link (Pt ref1) -> do
      link' <- liftIO $ readIORef ref1
      case link' of
        Info _ descRef -> liftIO $ modifyIORef' descRef func
        Link _ -> do
          newPoint <- repr point
          modify newPoint func


union :: (MonadIO m, MonadFail m) => Point a -> Point a -> a -> m ()
union p1 p2 newDesc = do
  point1@(Pt ref1) <- repr p1
  point2@(Pt ref2) <- repr p2

  Info w1 d1 <- liftIO $ readIORef ref1
  Info w2 d2 <- liftIO $ readIORef ref2

  if point1 == point2
    then liftIO $ writeIORef d1 newDesc
    else do
      weight1 <- liftIO $ readIORef w1
      weight2 <- liftIO $ readIORef w2

      let !newWeight = weight1 + weight2

      if weight1 >= weight2
        then do
          liftIO $ writeIORef ref2 (Link point1)
          liftIO $ writeIORef w1 newWeight
          liftIO $ writeIORef d1 newDesc
        else do
          liftIO $ writeIORef ref1 (Link point2)
          liftIO $ writeIORef w2 newWeight
          liftIO $ writeIORef d2 newDesc


equivalent :: MonadIO m => Point a -> Point a -> m Bool
equivalent p1 p2 = do
  v1 <- repr p1
  v2 <- repr p2
  return (v1 == v2)


redundant :: MonadIO m => Point a -> m Bool
redundant (Pt ref) = do
  pInfo <- liftIO $ readIORef ref
  case pInfo of
    Info _ _ -> return False
    Link _ -> return True
