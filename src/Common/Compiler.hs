{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Data types and helpers used to compose the compiler pipeline.
module Common.Compiler (
  ErrorMsg,
  Error (..),
  Warning (..),
  Pass (..),
  MonadError (..),
  MonadWriter (..),
  fromString,
  runPass,
  dump,
  unexpected,
  warn,
  todo,
  passIO,
  liftEither,
  typeError,
) where

import Common.Pretty (Pretty (pretty))
import Control.Monad.Except (
  Except,
  MonadError (..),
  liftEither,
  runExcept,
  throwError,
 )
import Control.Monad.Writer.Strict (
  MonadWriter (..),
  WriterT (..),
 )
import Data.String (IsString (..))
import System.Exit (
  exitFailure,
  exitSuccess,
 )
import System.IO (
  hPrint,
  stderr,
 )


-- | Type for error messages.
newtype ErrorMsg = ErrorMsg String
  deriving (Show)
  deriving (Semigroup) via String
  deriving (Monoid) via String


instance IsString ErrorMsg where
  fromString = ErrorMsg


instance Eq ErrorMsg where
  _ == _ = True


-- | Types of compiler errors that can be thrown during compilation.
data Error
  = -- | Halt compiler to dump output (not an error)
    Dump String
  | -- | Internal error; should be unreachable
    UnexpectedError ErrorMsg
  | -- | "It's a research artifact"
    UnimplementedError ErrorMsg
  | -- | Round peg in square hole
    TypeError ErrorMsg
  | -- | Identifier is out of scope
    ScopeError ErrorMsg
  | -- | Invalid naming convention at binding
    NameError ErrorMsg
  | -- | Malformed pattern
    PatternError ErrorMsg
  | -- | Error encountered by scanner
    LexError ErrorMsg
  | -- | Error encountered by parser
    ParseError ErrorMsg
  deriving (Show, Eq)


-- | Types of compiler warnings that can be logged during compilation.
data Warning
  = -- | Warning about type
    TypeWarning ErrorMsg
  | -- | Warning related to identifier names
    NameWarning ErrorMsg
  | -- | Warning related to patterns
    PatternWarning ErrorMsg
  deriving (Show, Eq)


-- | Type alias for underlying compiler pipeline monad.
type PassMonad = WriterT [Warning] (Except Error)


-- | The compiler pipeline monad; supports throwing errors, logging, etc.
newtype Pass a = Pass (PassMonad a)
  deriving (Functor) via PassMonad
  deriving (Applicative) via PassMonad
  deriving (Monad) via PassMonad
  deriving (MonadError Error) via PassMonad
  deriving (MonadWriter [Warning]) via PassMonad


instance MonadFail Pass where
  fail = throwError . UnexpectedError . fromString


-- | Invoke a compiler 'Pass'.
runPass :: Pass a -> Either Error (a, [Warning])
runPass (Pass p) = runExcept (runWriterT p)


-- | Dump pretty-printable output from within a compiler pass.
dump :: Pretty a => a -> Pass x
dump = throwError . Dump . show . pretty


-- | Report unexpected compiler error and halt pipeline.
unexpected :: (MonadError Error m) => String -> m a
unexpected = throwError . UnexpectedError . fromString


-- | Log a compiler warning.
warn :: MonadWriter [Warning] m => Warning -> m ()
warn w = pass $ return ((), (++ [w]))


-- | Report unexpected compiler error and halt pipeline.
todo :: (MonadError Error m) => String -> m a
todo = throwError . UnimplementedError . fromString


-- | Execute compiler pass in I/O monad, exiting upon exception.
passIO :: Pass a -> IO (a, [Warning])
passIO p = case runPass p of
  Left (Dump s) -> putStrLn s >> exitSuccess
  Left e -> hPrint stderr e >> exitFailure
  Right a -> return a


-- | Throw a type error with some String error message.
typeError :: (MonadError Error m) => String -> m a
typeError = throwError . TypeError . fromString
