{-# LANGUAGE ViewPatterns #-}
module IR.Types.Typechecking
  ( typecheckProgram
  ) where

import qualified Common.Compiler               as Compiler
import           IR.IR                          ( Annotations
                                                , Program(..)
                                                , Type
                                                )
import           IR.Types.Inference             ( inferProgram )

-- | Typecheck a program. For now, we just use HM inference.
typecheckProgram :: Program Annotations -> Compiler.Pass (Program Type)
typecheckProgram = inferProgram
