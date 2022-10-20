-- |

module IR.Types.Constraint.Utils
  ( modifySTRef
  ) where
import           Control.Monad.ST.Trans         ( STRef
                                                , STT
                                                , readSTRef
                                                , writeSTRef
                                                )

modifySTRef :: Monad m => STRef s a -> (a -> a) -> STT s m ()
modifySTRef ref f = do
  x <- readSTRef ref
  writeSTRef ref (f x)
