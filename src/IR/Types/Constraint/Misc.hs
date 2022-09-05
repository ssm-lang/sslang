module IR.Types.Constraint.Misc
  ( modifySTRef
  , foldrM
  ) where

import           Control.Monad.ST.Trans
import           IR.Types.Constraint.Inference  ( InferM )

modifySTRef :: STRef s a -> (a -> a) -> InferM s ()
modifySTRef ref f = do
  x <- readSTRef ref
  writeSTRef ref (f x)

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d []       = return d
foldrM f d (x : xs) = f x =<< foldrM f d xs
