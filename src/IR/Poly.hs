{- |

The "Poly" IR: After removing typeclass information

The expressions use the Types.Poly types

-}

module IR.Poly (
  module IR.IR -- Export, e.g., all the polymorphic IR data constructors
  , PIProgram
  , PIExpr
  , PIAlt
  , PIPrimFun
  ) where

import IR.IR
import qualified Types.Poly

type PIProgram = IR.IR.Program Types.Poly.Type
type PIExpr = IR.IR.Expr Types.Poly.Type
type PIAlt = IR.IR.Alt Types.Poly.Type
type PIPrimFun = IR.IR.PrimFun
