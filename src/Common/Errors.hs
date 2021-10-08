module Common.Errors where

-- | Types of compiler errors that can be thrown during compilation.
data CompileError
  = UnexpectedError String  -- ^ Internal compiler error; shouldn't be reachable
  | TypeError String
  | ParseError String       -- ^ Parse error
  | AstError String         -- ^ Some error in the AST
  deriving Show
