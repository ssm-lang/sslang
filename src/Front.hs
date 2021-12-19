module Front
  ( tokenStream
  , parseSource
  , desugarAst
  , checkAst
  ) where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import           Front.Check                    ( checkTopSignatures )
import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parseProgram )
import           Front.Scanner                  ( scanTokens )
import           Front.Scope                    ( scopeProgram )
import           Front.Token                    ( Token(..) )

import           Control.Monad.Except           ( liftEither )
import           Data.Bifunctor                 ( first )

-- | Scan the string into a list of tokens (for debugging)
tokenStream :: String -> Compiler.Pass [Token]
tokenStream = liftEither . first Compiler.LexError . scanTokens

-- | Scan and parse the string into the AST
parseSource :: String -> Compiler.Pass A.Program
parseSource = liftEither . first Compiler.ParseError . parseProgram

-- | Desugar various AST constructs
desugarAst :: A.Program -> Compiler.Pass A.Program
desugarAst = return . parseOperators

-- | Check the structure of the AST, throwing errors as necessary
checkAst :: A.Program -> Compiler.Pass ()
checkAst prog = do
  scopeProgram prog
  if checkTopSignatures prog
    then return ()
    else Compiler.throw $ Compiler.AstError "Type signature mismatch" -- TODO: better error message
