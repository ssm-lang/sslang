module IR.Pattern (checkAnomaly) where

import Common.Compiler (Pass)
import qualified IR.IR as I
import qualified IR.Pattern.Anomaly as Anomaly


checkAnomaly :: I.Program t -> Pass ()
checkAnomaly = Anomaly.checkProgram
