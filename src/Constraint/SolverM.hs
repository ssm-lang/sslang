module Constraint.SolverM where

import Common.Compiler (Pass (..))
import Control.Monad.ST.Trans
import Control.Monad.State.Lazy (StateT (..))

newtype SolverCtx = SolverCtx
  { count :: Int
  }

type SolverM s a = STT s (StateT SolverCtx Pass) a

initSolverCtx :: SolverCtx
initSolverCtx = SolverCtx {count = 0}
