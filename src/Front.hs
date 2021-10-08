module Front where

import           Common.Errors                  ( CompileError(..) )

import qualified Front.Ast                     as A
import qualified IR.IR                         as L
import qualified Types.Ast                     as L

import           Front.Check                    ( checkRoutineSignatures )
import           Front.Lowering                 ( lowerProgram )
import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parse )
import           Front.Scanner                  ( runAlex )

import           Data.Bifunctor                 ( first )

parseSource :: String -> Either CompileError A.Program
parseSource = first ParseError . flip runAlex parse

desugarAst :: A.Program -> Either CompileError A.Program
desugarAst = return . parseOperators

checkAst :: A.Program -> Either CompileError ()
checkAst prog = if checkRoutineSignatures prog
  then return ()
  else Left $ AstError "Type signature mismatch" -- TODO: better error message

lowerAst :: A.Program -> Either CompileError (L.Program L.Type)
lowerAst = return . lowerProgram
