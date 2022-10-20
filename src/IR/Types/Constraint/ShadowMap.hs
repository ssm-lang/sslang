-- |

module IR.Types.Constraint.ShadowMap
  ( Map
  , new
  , lookup
  , add
  , replace
  , remove
  , member
  , notMember
  ) where

import           Control.Monad.ST.Trans         ( STRef
                                                , STT
                                                , newSTRef
                                                , readSTRef
                                                , writeSTRef
                                                )
import qualified Data.Map.Strict               as M
import           IR.Types.Constraint.Utils      ( modifySTRef )
import           Prelude                 hiding ( lookup )

newtype Map s k a = ShadowMap (STRef s (M.Map k [a]))

new :: Monad m => STT s m (Map s k a)
new = ShadowMap <$> newSTRef M.empty

lookup :: (Monad m, Ord k) => Map s k a -> k -> STT s m (Maybe a)
lookup (ShadowMap ref) k = do
  m <- readSTRef ref
  case M.lookup k m of
    Nothing -> return Nothing
    Just [] -> do
      writeSTRef ref $ M.delete k m
      return Nothing
    Just (x : _) -> return $ Just x

add :: (Monad m, Ord k) => Map s k a -> k -> a -> STT s m ()
add (ShadowMap ref) k v = do
  let add' m = case M.lookup k m of
        Nothing -> M.insert k [v] m
        Just vs -> M.insert k (v : vs) m
  modifySTRef ref add'

replace :: (Monad m, Ord k) => Map s k a -> k -> a -> STT s m ()
replace (ShadowMap ref) k v = modifySTRef ref (M.insert k [v])

remove :: (Monad m, Ord k) => Map s k a -> k -> STT s m ()
remove (ShadowMap ref) k = do
  let remove' m = case M.lookup k m of
        Nothing       -> m
        Just []       -> M.delete k m
        Just [_     ] -> M.delete k m
        Just (_ : vs) -> M.insert k vs m
  modifySTRef ref remove'

member :: (Monad m, Ord k) => Map s k a -> k -> STT s m Bool
member (ShadowMap ref) k = do
  m <- readSTRef ref
  return $ M.member k m

notMember :: (Monad m, Ord k) => Map s k a -> k -> STT s m Bool
notMember (ShadowMap ref) k = do
  m <- readSTRef ref
  return $ M.notMember k m

