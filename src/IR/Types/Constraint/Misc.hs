module IR.Types.Constraint.Misc
  ( modifySTRef
  ) where

import           Control.Monad.ST.Trans
import           IR.Types.Constraint.Inference  ( InferM )

modifySTRef :: STRef s a -> (a -> a) -> InferM s ()
modifySTRef ref f = do
  x <- readSTRef ref
  writeSTRef ref (f x)
