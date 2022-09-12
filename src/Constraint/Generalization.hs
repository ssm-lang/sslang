module Constraint.Generalization where

import Constraint.SolverM (SolverM, count)
import Constraint.Structure (Structure)
import qualified Constraint.Unifier as U
import Control.Monad.State.Class (get, put)

data Scheme s = Scheme
  { root :: U.Variable s,
    generics :: [U.Variable s],
    quantifiers :: [U.Variable s]
  }

freshId :: SolverM s Int
freshId = do
  ctx <- get
  put ctx {count = count ctx + 1}
  return $ count ctx

flexible :: Maybe (Structure (U.Variable s)) -> SolverM s (U.Variable s)
flexible = undefined

rigid :: SolverM s (U.Variable s)
rigid = undefined

instantiate :: Scheme s -> SolverM s ([U.Variable s], U.Variable s)
instantiate = undefined
