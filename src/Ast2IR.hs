module Ast2IR ( astToIR ) where

import qualified Ast as A
import qualified Types.Ast as A
import qualified Common.IR as L

astToIR :: A.Program -> L.Program A.Type
astToIR (A.Program _) = error "TODO"
