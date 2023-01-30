module Front.Pattern where

import           Common.Compiler                ( Pass )
import qualified Front.Ast                     as A
import qualified Front.Pattern.Anomaly         as Anomaly

checkAnomaly :: A.Program -> Pass ()
checkAnomaly = Anomaly.checkProgram
