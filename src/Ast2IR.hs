module Ast2IR ( astToIR ) where

import qualified Ast as A
import IR.Ast as I

astToIR :: A.Program -> I.AIProgram
astToIR (A.Program decls) = I.Program []
