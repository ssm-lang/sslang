module Common.Errors where

-- | Types of compiler errors that can be thrown during compilation.
data CompileError
  = UnexpectedError String  -- ^ Internal compiler error; shouldn't be reachable
  | TypeError
