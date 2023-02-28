module IR.Constraint.PrettyPrint where

import           IR.Constraint.Type

import           Prettyprinter
import           Prettyprinter.Render.String

import           Common.Compiler

-- Make Constraint an instance of Pretty!!!

printConstraint :: Constraint -> String
printConstraint = "test"