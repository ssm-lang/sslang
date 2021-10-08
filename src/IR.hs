module IR where

import           Codegen.Codegen                ( genProgram )
import           Common.Errors                  ( CompileError(..) )
import           IR.IR                          ( Program(..) )

import qualified Types.Flat                    as Flat
import qualified Language.C                    as C

yieldAbstraction :: Program a -> Either CompileError (Program a)
yieldAbstraction = return

lambdaLift :: Program a -> Either CompileError (Program a)
lambdaLift = return

defunctionalize :: Program a -> Either CompileError (Program a)
defunctionalize = return

inferDrops :: Program a -> Either CompileError (Program a)
inferDrops = return

codegen :: Program Flat.Type -> Either CompileError [C.Definition]
codegen = genProgram
