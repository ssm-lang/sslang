-- |

module IR.Types.Constraint.InfiniteArray
  ( InfiniteArray
  , new
  , get
  , set
  , update
  ) where

import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Control.Monad.ST.Trans         ( STArray
                                                , STRef
                                                , STT
                                                , boundsSTArray
                                                , newSTArray
                                                , newSTRef
                                                , readSTArray
                                                , readSTRef
                                                , writeSTArray
                                                , writeSTRef
                                                )
-- import           IR.Types.Constraint.Type       ( Infer )

data InfiniteArray s a = InfiniteArray
  { defa  :: a
  , table :: STRef s (STArray s Int a)
  }

new :: Monad m => Int -> a -> STT s m (InfiniteArray s a)
new defaultSize x = do
  t <- newSTArray (0, defaultSize) x
  r <- newSTRef t
  return $ InfiniteArray x r

get :: Monad m => InfiniteArray s b -> Int -> STT s m b
get a i = do
  ensure a i
  t <- readSTRef (table a)
  readSTArray t i

set :: Monad m => InfiniteArray s e -> Int -> e -> STT s m ()
set a i x = do
  ensure a i
  t <- readSTRef (table a)
  writeSTArray t i x

update :: Monad m => InfiniteArray s e -> Int -> (e -> e) -> STT s m ()
update a i f = do
  ensure a i
  t <- get a i
  set a i (f t)

newLength :: Int -> Int -> Int
newLength l i = if i < l then l else newLength (2 * l) i

ensure :: Monad m => InfiniteArray s a -> Int -> STT s m ()
ensure a i = do
  t <- readSTRef (table a)
  let (_, l) = boundsSTArray t
  when (i >= l) $ do
    t' <- newSTArray (0, newLength (2 * l) i) (defa a)
    forM_
      [0 .. (l - 1)]
      (\i' -> do
        x <- readSTArray t i'
        writeSTArray t' i x
      )
    writeSTRef (table a) t'
