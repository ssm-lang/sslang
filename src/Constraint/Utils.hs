module Constraint.Utils where

import Common.Compiler (Error (..), fromString, throwError)
import Constraint.SolverM (SolverM)
import Control.Monad.ST.Trans

modifySTRef :: STRef s a -> (a -> a) -> SolverM s ()
modifySTRef ref f = do
  x <- readSTRef ref
  writeSTRef ref (f x)

throwVariableScopeEscapeError :: SolverM s a
throwVariableScopeEscapeError = throwTypeError "variable scope escape"

throwTypeError :: String -> SolverM s a
throwTypeError = throwError . TypeError . fromString
