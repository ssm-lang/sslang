module IR.Types.Inference
  ( inferProgram
  ) where

import           IR.IR
import           IR.Types.Type
import           IR.Types.Unification

import qualified Common.Compiler               as Compiler

inferProgram :: Program Annotations -> Compiler.Pass (Program Type)
inferProgram = undefined
