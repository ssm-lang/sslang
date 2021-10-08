module IR where

import           Codegen.Codegen                ( genProgram )
import qualified Common.Compiler               as Compiler
import           IR.IR                          ( Program(..) )

import qualified Types.Flat                    as Flat
import qualified Language.C                    as C

yieldAbstraction :: Program a -> Compiler.Pass (Program a)
yieldAbstraction = return

lambdaLift :: Program a -> Compiler.Pass (Program a)
lambdaLift = return

defunctionalize :: Program a -> Compiler.Pass (Program a)
defunctionalize = return

inferDrops :: Program a -> Compiler.Pass (Program a)
inferDrops = return

codegen :: Program Flat.Type -> Compiler.Pass [C.Definition]
codegen = genProgram
