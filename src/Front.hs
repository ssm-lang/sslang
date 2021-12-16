{- | Front end of the compiler pipeline.

Throughout this stage, high-level syntax is progressively parsed and desugared
into simpler AST constructs.
-}
module Front where

import           Common.Compiler                ( Error(..)
                                                , Pass
                                                , dump
                                                , throw
                                                )
import           Common.Default                 ( Default(..) )

import qualified Front.Ast                     as A

import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parseProgram )
import           Front.Scanner                  ( scanTokens )
import           Front.Token                    ( prettyTokens )

import           Control.Monad.Except           ( liftEither )
import           Data.Bifunctor                 ( first )

import           Common.Pretty                  ( Pretty(pretty) )
import           Control.Monad                  ( when )
import           System.Console.GetOpt          ( ArgDescr(..)
                                                , OptDescr(..)
                                                )

-- | Operation modes for the front end compiler stage.
data Mode
  = DumpTokens
  | DumpAst
  | DumpAstParsed
  | DumpAstFinal
  | Continue
  deriving (Eq, Show)

-- | Compiler options for the front end compiler stage.
newtype Options = Options { optMode :: Mode } deriving (Eq, Show)

instance Default Options where
  def = Options { optMode = Continue }

-- | CLI options for the front end compiler stage.
options :: [OptDescr (Options -> Options)]
options =
  [ Option ""
           ["dump-tokens"]
           (NoArg $ setMode DumpTokens)
           "Print the token stream"
  , Option ""
           ["dump-ast"]
           (NoArg $ setMode DumpAst)
           "Print the AST, before operators are parsed"
  , Option ""
           ["dump-ast-parsed"]
           (NoArg $ setMode DumpAstParsed)
           "Print the AST after operators are parsed"
  , Option ""
           ["dump-ast-final"]
           (NoArg $ setMode DumpAstFinal)
           "Print the AST after all desugaring, just before lowering to IR"
  ]
 where
  setMode :: Mode -> Options -> Options
  setMode m o = o { optMode = m }

-- | Front end compiler stage.
run :: Options -> String -> Pass A.Program
run opt src = do
  when (optMode opt == DumpTokens)
    $ either (throw . LexError) (dump . prettyTokens)
    $ scanTokens src

  ast <- liftEither $ first ParseError $ parseProgram src
  when (optMode opt == DumpAst) $ dump $ show $ pretty ast

  astP <- parseOperators ast
  when (optMode opt == DumpAstParsed) $ dump $ show $ pretty astP

  when (optMode opt == DumpAstFinal) $ dump $ show $ pretty astP
  return astP
