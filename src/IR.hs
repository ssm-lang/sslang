module IR where

import           Codegen.Codegen                ( genProgram )
import qualified Common.Compiler               as Compiler
import qualified IR.IR                         as I

import qualified Language.C                    as C
import qualified Text.PrettyPrint.Mainland     as C
import qualified Text.PrettyPrint.Mainland.Class
                                               as C

import qualified Types.Flat                    as Flat

yieldAbstraction :: I.Program a -> Compiler.Pass (I.Program a)
yieldAbstraction = return

lambdaLift :: I.Program a -> Compiler.Pass (I.Program a)
lambdaLift = return

defunctionalize :: I.Program a -> Compiler.Pass (I.Program a)
defunctionalize = return

inferDrops :: I.Program a -> Compiler.Pass (I.Program a)
inferDrops = return

codegen :: I.Program Flat.Type -> Compiler.Pass [C.Definition]
codegen = genProgram

prettyC :: [C.Definition] -> Compiler.Pass String
prettyC = return . C.pretty 120 . C.pprList
