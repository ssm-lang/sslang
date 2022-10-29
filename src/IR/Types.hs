-- | Wrapper module around other types-related modules and definitions.
module IR.Types
  ( module IR.Types.Type
  , typecheckProgram
  ) where

-- import           IR.Constraint.Typechecking     ( typecheckProgram )
import           IR.Types.Type
import           IR.Types.Typechecking          ( typecheckProgram )
