module Constraint.ShadowMap where

import Constraint.SolverM (SolverM)
import Constraint.Utils (modifySTRef)
import Control.Monad.ST.Trans (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Map.Strict as M

newtype Map s k a = ShadowMap (STRef s (M.Map k [a]))

new :: SolverM s (Map s k a)
new = ShadowMap <$> newSTRef M.empty

lookup :: Ord k => Map s k a -> k -> SolverM s (Maybe a)
lookup (ShadowMap ref) k = do
  m <- readSTRef ref
  case M.lookup k m of
    Nothing -> return Nothing
    Just [] -> do
      writeSTRef ref $ M.delete k m
      return Nothing
    Just (x : _) -> return $ Just x

add :: Ord k => Map s k a -> k -> a -> SolverM s ()
add (ShadowMap ref) k v = do
  let add' m = case M.lookup k m of
        Nothing -> M.insert k [v] m
        Just vs -> M.insert k (v : vs) m
  modifySTRef ref add'

remove :: Ord k => Map s k a -> k -> SolverM s ()
remove (ShadowMap ref) k = do
  let remove' m = case M.lookup k m of
        Nothing -> m
        Just [] -> M.delete k m
        Just [_] -> M.delete k m
        Just (_ : vs) -> M.insert k vs m
  modifySTRef ref remove'

member :: Ord k => Map s k a -> k -> SolverM s Bool
member (ShadowMap ref) k = do
  m <- readSTRef ref
  return $ M.member k m

notMember :: Ord k => Map s k a -> k -> SolverM s Bool
notMember (ShadowMap ref) k = do
  m <- readSTRef ref
  return $ M.notMember k m
