{- |

The "Mono" IR: After removing polymorphism

The expressions use the Types.Mono types

-}

module IR.Mono (
  module IR.IR -- Export, e.g., all the polymorphic IR data constructors
  , MIProgram
  , MIExpr
  , MIAlt
  , MIPrimFun
  ) where

import IR.IR
import qualified Types.Mono

type MIProgram = IR.IR.Program Types.Mono.Type
type MIExpr = IR.IR.Expr Types.Mono.Type
type MIAlt = IR.IR.Alt Types.Mono.Type
type MIPrimFun = IR.IR.PrimFun
