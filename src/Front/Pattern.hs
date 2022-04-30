module Front.Pattern where

import           Common.Compiler                ( Pass )
import qualified Front.Ast                     as A
import qualified Front.Pattern.Anomaly         as Anomaly
import qualified Front.Pattern.Desugar         as Desugar

desugarProgram :: A.Program -> Pass A.Program
desugarProgram = Desugar.desugarProgram

checkAnomaly :: A.Program -> Pass ()
checkAnomaly = Anomaly.checkProgram
