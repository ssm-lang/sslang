module Front where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import           Front.Check                    ( checkRoutineSignatures )
import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parse )
import           Front.Scanner                  ( alexMonadScan
                                                , runAlex
                                                )
import           Front.Token                    ( Token(..)
                                                , TokenType(..)
                                                )

import           Control.Monad.Except           ( liftEither )
import           Data.Bifunctor                 ( first )

tokenStream :: String -> Compiler.Pass [Token]
tokenStream = liftEither . first Compiler.LexError . (`runAlex` tokenize)
 where
  tokenize = do
    tok <- alexMonadScan
    case tok of
      Token (_, TEOF) -> return []
      Token _         -> (:) tok <$> tokenize

parseSource :: String -> Compiler.Pass A.Program
parseSource = liftEither . first Compiler.ParseError . flip runAlex parse

desugarAst :: A.Program -> Compiler.Pass A.Program
desugarAst = return . parseOperators

checkAst :: A.Program -> Compiler.Pass ()
checkAst prog = if checkRoutineSignatures prog
  then return ()
  else Compiler.throw $ Compiler.AstError "Type signature mismatch" -- TODO: better error message
