{-# LANGUAGE DerivingVia #-}
module Common.Compiler
  ( Error(..)
  , Pass(..)
  , runPass
  , throw
  ) where

import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                , runExcept
                                                , throwError
                                                )

-- | Types of compiler errors that can be thrown during compilation.
data Error
  = UnexpectedError String  -- ^ Internal compiler error; shouldn't be reachable
  | TypeError String
  | LexError String         -- ^ Lex error
  | ParseError String       -- ^ Parse error
  | AstError String         -- ^ Some error in the AST
  deriving Show

-- | A compiler pass, able to return errors
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
