module Constraint.SolverM where

import Common.Compiler (Pass (..))
import Control.Monad.ST.Trans
import Control.Monad.State.Lazy (StateT (..))

data SolverCtx = SolverCtx
  { currId :: Int,
    currMark :: Int
  }

type SolverM s a = STT s (StateT SolverCtx Pass) a

initSolverCtx :: SolverCtx
initSolverCtx = SolverCtx {currId = 0, currMark = 0}
