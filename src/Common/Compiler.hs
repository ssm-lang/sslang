{-# LANGUAGE DerivingVia #-}
module Common.Compiler
  ( ErrorMsg
  , Error(..)
  , Pass(..)
  , runPass
  , dump
  , throw
  , passIO
  ) where

import           Common.Pretty                  ( Pretty(pretty) )
import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                , runExcept
                                                , throwError
                                                )

import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPrint
                                                , stderr
                                                )

-- | Convenience
type ErrorMsg = String

-- | Types of compiler errors that can be thrown during compilation.
data Error
  = Dump String               -- ^ Halt compiler to dump output (not an error)
  | UnexpectedError ErrorMsg  -- ^ Internal compiler error; shouldn't be reachable
  | TypeError ErrorMsg        -- ^ Type error
  | ScopeError ErrorMsg       -- ^ Scope error
  | NameError ErrorMsg        -- ^ Name error
  | PatternError ErrorMsg     -- ^ Pattern error
  | LexError ErrorMsg         -- ^ Lex error
  | ParseError ErrorMsg       -- ^ Parse error
  | AstError ErrorMsg         -- ^ Some error in the AST
  deriving (Show, Eq)

newtype Pass a = Pass (Except Error a)
  deriving Functor                    via (Except Error)
  deriving Applicative                via (Except Error)
  deriving Monad                      via (Except Error)
  deriving (MonadError Error) via (Except Error)

instance MonadFail Pass where
  fail = throw . UnexpectedError

-- | Invoke a compiler pass.
runPass :: Pass a -> Either Error a
runPass (Pass p) = runExcept p

-- | Throw an error from within a compiler pass.
throw :: Error -> Pass a
throw = throwError

-- | Dump pretty-printable output from within a compiler pass.
dump :: Pretty a => a -> Pass x
dump = throwError . Dump . show . pretty

-- | Execute compiler pass in I/O monad, exiting upon exception.
passIO :: Pass a -> IO a
passIO p = case runPass p of
  Left  (Dump s) -> putStrLn s >> exitSuccess
  Left  e        -> hPrint stderr e >> exitFailure
  Right a        -> return a
