{-# LANGUAGE RankNTypes #-}
module Elaboration.Scratch where

import           Common.Compiler                ( Pass(..) )
import           Control.Monad.ST.Trans
import           Control.Monad.State.Lazy       ( StateT(..)
                                                , evalStateT
                                                )

type SolverMonad s = STT s (StateT Int Pass)

newtype Solver s a = Solver (SolverMonad s a)

runSolver :: Pass Int
runSolver = evalStateT
  (runSTT $ do
    return 2
  )
  0
