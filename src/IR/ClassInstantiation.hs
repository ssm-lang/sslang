module IR.ClassInstantiation where

import qualified Common.Compiler               as Compiler
import qualified IR.IR                         as I

instProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
instProgram = return -- TODO
