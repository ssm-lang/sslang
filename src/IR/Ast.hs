{- |

The "Ast" IR: designed to be generated directly from the AST, i.e.,
with no type inference, etc.

The expressions use the Types.Ast types

-}

module IR.Ast (
  module IR.IR -- Export, e.g., all the polymorphic IR data constructors
  , AIProgram
  , AIExpr
  , AIAlt
  , AIPrimFun
  ) where

import IR.IR
import qualified Types.Ast

type AIProgram = IR.IR.Program Types.Ast.Type
type AIExpr = IR.IR.Expr Types.Ast.Type
type AIAlt = IR.IR.Alt Types.Ast.Type
type AIPrimFun = IR.IR.PrimFun
