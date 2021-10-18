module Front where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import           Front.Check                    ( checkTopSignatures )
import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parseProgram )
import           Front.Scanner                  ( scanTokens )
import           Front.Token                    ( Token(..) )

import           Control.Monad.Except           ( liftEither )
import           Data.Bifunctor                 ( first )

tokenStream :: String -> Compiler.Pass [Token]
tokenStream = liftEither . first Compiler.LexError . scanTokens

parseSource :: String -> Compiler.Pass A.Program
parseSource = liftEither . first Compiler.ParseError . parseProgram

desugarAst :: A.Program -> Compiler.Pass A.Program
desugarAst = return . parseOperators

checkAst :: A.Program -> Compiler.Pass ()
checkAst prog = if checkTopSignatures prog
  then return ()
  else Compiler.throw $ Compiler.AstError "Type signature mismatch" -- TODO: better error message
