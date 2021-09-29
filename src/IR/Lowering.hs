module IR.Lowering
  ( astToIR
  ) where

import qualified Ast                           as A
import qualified IR.IR                         as L
import qualified Types.Ast                     as A

astToIR :: A.Program -> L.Program A.Type
astToIR (A.Program _) = error "TODO"
