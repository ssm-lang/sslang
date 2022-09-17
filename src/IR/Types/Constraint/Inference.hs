module IR.Types.Constraint.Inference
  ( inferProgram
  , InferM
  ) where

import qualified Common.Compiler               as Compiler
import           Control.Monad.ST.Trans
import           IR.IR

type InferM s a = STT s Compiler.Pass a

inferProgram :: Program [Type] -> Compiler.Pass (Program Type)
inferProgram p = undefined


