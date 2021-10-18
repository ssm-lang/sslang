module Front where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import           Front.Check                    ( checkRoutineSignatures )
import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parse )
import           Front.Scanner                  ( Token(..)
                                                , TokenType(..)
                                                , alexMonadScan
                                                , runAlex
                                                )

import           Control.Monad.Except           ( liftEither )
import           Data.Bifunctor                 ( first )

-- | Scan the string into a list of tokens (for debugging)
tokenStream :: String -> Compiler.Pass [Token]
tokenStream = liftEither . first Compiler.LexError . (`runAlex` tokenize)
 where
  tokenize = do
    tok <- alexMonadScan
    case tok of
      Token _ TEOF -> return []
      Token _ _    -> (:) tok <$> tokenize

-- | Scan and parse the string into the AST
parseSource :: String -> Compiler.Pass A.Program
parseSource = liftEither . first Compiler.ParseError . flip runAlex parse

-- | Desugar various AST constructs
desugarAst :: A.Program -> Compiler.Pass A.Program
desugarAst = return . parseOperators

-- | Check the structure of the AST, throwing errors as necessary
checkAst :: A.Program -> Compiler.Pass ()
checkAst prog = if checkRoutineSignatures prog
  then return ()
  else Compiler.throw $ Compiler.AstError "Type signature mismatch" -- TODO: better error message
