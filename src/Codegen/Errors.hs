module Codegen.Errors where

-- | Types of compiler errors that can be thrown during codegen.
newtype CodegenError
  = CodegenErrorMsg String  -- ^ Internal compiler error; shouldn't be reachable
