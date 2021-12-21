{-# LANGUAGE DerivingVia #-}
-- | Data types and helpers used to compose the compiler pipeline.
module Common.Compiler
  ( ErrorMsg
  , Error(..)
  , Pass(..)
  , fromString
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

import           Data.String                    ( IsString(..) )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPrint
                                                , stderr
                                                )

-- | Type for error messages.
newtype ErrorMsg = ErrorMsg String deriving (Show)

instance IsString ErrorMsg where
  fromString = ErrorMsg

instance Eq ErrorMsg where
  _ == _ = True

-- | Types of compiler errors that can be thrown during compilation.
data Error
  = Dump String               -- ^ Halt compiler to dump output (not an error)
  | UnexpectedError ErrorMsg  -- ^ Internal error; should be unreachable
  | TypeError ErrorMsg        -- ^ Round peg in square hole
  | ScopeError ErrorMsg       -- ^ Identifier is out of scope
  | NameError ErrorMsg        -- ^ Invalid naming convention at binding
  | PatternError ErrorMsg     -- ^ Malformed pattern
  | LexError ErrorMsg         -- ^ Error encountered by scanner
  | ParseError ErrorMsg       -- ^ Error encountered by parser
  deriving (Show, Eq)

-- | The compiler pipeline monad; supports throwing errors, logging, etc.
newtype Pass a = Pass (Except Error a)
  deriving Functor                    via (Except Error)
  deriving Applicative                via (Except Error)
  deriving Monad                      via (Except Error)
  deriving (MonadError Error)         via (Except Error)

instance MonadFail Pass where
  fail = throw . UnexpectedError . ErrorMsg

-- | Invoke a compiler 'Pass'.
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
