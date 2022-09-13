module Constraint.InfiniteArray
  ( InfiniteArray,
    new,
    get,
    set,
    update,
  )
where

import Constraint.SolverM (SolverM)
import Control.Monad (forM_, when)
import Control.Monad.ST.Trans
  ( STArray,
    STRef,
    boundsSTArray,
    newSTArray,
    newSTRef,
    readSTArray,
    readSTRef,
    writeSTArray,
    writeSTRef,
  )

data InfiniteArray s a = InfiniteArray
  { defa :: a,
    table :: STRef s (STArray s Int a)
  }

new :: Int -> a -> SolverM s (InfiniteArray s a)
new defaultSize x = do
  t <- newSTArray (0, defaultSize) x
  r <- newSTRef t
  return $ InfiniteArray x r

get :: InfiniteArray s b -> Int -> SolverM s b
get a i = do
  ensure a i
  t <- readSTRef (table a)
  readSTArray t i

set :: InfiniteArray s e -> Int -> e -> SolverM s ()
set a i x = do
  ensure a i
  t <- readSTRef (table a)
  writeSTArray t i x

update :: InfiniteArray s e -> Int -> (e -> e) -> SolverM s ()
update a i f = do
  ensure a i
  t <- get a i
  set a i (f t)

newLength :: Int -> Int -> Int
newLength l i =
  if i < l
    then l
    else newLength (2 * l) i

ensure :: InfiniteArray s a -> Int -> SolverM s ()
ensure a i = do
  t <- readSTRef (table a)
  let (_, l) = boundsSTArray t
  when (i >= l) $ do
    t' <- newSTArray (0, newLength (2 * l) i) (defa a)
    forM_
      [0 .. (l - 1)]
      ( \i' -> do
          x <- readSTArray t i'
          writeSTArray t' i x
      )
    writeSTRef (table a) t'
