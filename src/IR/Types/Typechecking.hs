-- | Entry point to the type checking phase of the compiler.
module IR.Types.Typechecking
  ( typecheckProgram
  ) where

import qualified Common.Compiler               as Compiler
import           IR.IR                          ( Annotations
                                                , Program(..)
                                                , Type
                                                )
import           IR.Types.Inference             ( inferProgram )

-- | Type check an optionally annotated program.
typecheckProgram :: Program Annotations -> Compiler.Pass (Program Type)
typecheckProgram = inferProgram -- for now, we just use HM inference
