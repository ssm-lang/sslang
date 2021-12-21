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
import           Front.Scope                    ( scopeProgram )
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
  = Continue            -- ^ Compile end-to-end (default).
  | DumpTokens          -- ^ Print the token stream from the scanner.
  | DumpAst             -- ^ Print the initial parsed AST, before operators are parsed.
  | DumpAstParsed       -- ^ Print the AST after operators are parsed.
  | DumpAstFinal        -- ^ Print the AST after all desugaring, just before lowering.
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
           "Print the token stream from the scanner"
  , Option ""
           ["dump-ast"]
           (NoArg $ setMode DumpAst)
           "Print the initial parsed AST, before operators are parsed"
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

-- | Parse a fully-formed AST from some String input.
parseAst :: Options -> String -> Pass A.Program
parseAst opt src = do
  when (optMode opt == DumpTokens)
    $ either (throw . LexError) (dump . prettyTokens)
    $ scanTokens src

  ast <- liftEither $ first ParseError $ parseProgram src
  when (optMode opt == DumpAst) $ dump $ show $ pretty ast

  astP <- parseOperators ast
  when (optMode opt == DumpAstParsed) $ dump $ show $ pretty astP

  -- TODO: other desugaring

  when (optMode opt == DumpAstFinal) $ dump $ show $ pretty astP
  return astP

-- | Semantic checking on an AST.
checkAst :: Options -> A.Program -> Pass ()
checkAst _opt ast = do
  scopeProgram ast

-- | Front end compiler stage.
run :: Options -> String -> Pass A.Program
run opt src = do
  ast <- parseAst opt src
  checkAst opt ast
  return ast
