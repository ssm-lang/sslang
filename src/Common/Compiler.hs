{-# LANGUAGE DerivingVia #-}
module Common.Compiler
  ( ErrorMsg
  , Error(..)
  , Pass(..)
  , runPass
  , throw
  ) where

import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                , runExcept
                                                , throwError
                                                )

-- | Convenience
type ErrorMsg = String

-- | Types of compiler errors that can be thrown during compilation.
data Error
  = UnexpectedError ErrorMsg  -- ^ Internal compiler error; shouldn't be reachable
  | TypeError ErrorMsg        -- ^ Type error
  | ScopeError ErrorMsg       -- ^ Scope error
  | NameError ErrorMsg        -- ^ Name error
  | PatternError ErrorMsg     -- ^ Pattern error
  | LexError ErrorMsg         -- ^ Lex error
  | ParseError ErrorMsg       -- ^ Parse error
  | AstError ErrorMsg         -- ^ Some error in the AST
  deriving Show

newtype Pass a = Pass (Except Error a)
  deriving Functor                    via (Except Error)
  deriving Applicative                via (Except Error)
  deriving Monad                      via (Except Error)
  deriving (MonadError Error) via (Except Error)

instance MonadFail Pass where
  fail = throw . UnexpectedError

-- | Invoke a compiler pass
runPass :: Pass a -> Either Error a
runPass (Pass p) = runExcept p

-- | Throw an error from within a compiler pass
throw :: Error -> Pass a
throw = throwError
