{-# LANGUAGE OverloadedStrings #-}

{- | Front end of the compiler pipeline.

Throughout this stage, high-level syntax is progressively parsed and desugared
into simpler AST constructs.
-}
module Front where

import Common.Compiler (
  Pass,
  dump,
 )
import Common.Default (Default (..))

import qualified Front.Ast as A

import Front.DesugarLists (desugarLists)
import Front.DesugarPatTup (desugarPatTup)
import Front.DesugarStrings (desugarStrings)
import Front.ParseOperators (parseOperators)
import Front.Parser (parseProgram)
import Front.Scanner (scanTokens)
import Front.Scope (scopeProgram)
import Front.Token (prettyTokens)

import Common.Pretty (Pretty (pretty))
import Control.Monad (when)
import System.Console.GetOpt (
  ArgDescr (..),
  OptDescr (..),
 )


-- | Operation modes for the front end compiler stage.
data Mode
  = -- | Compile end-to-end (default).
    Continue
  | -- | Print the token stream from the scanner.
    DumpTokens
  | -- | Print the initial parsed AST, before operators are parsed.
    DumpAst
  | -- | Print the AST after operators are parsed.
    DumpAstParsed
  | -- | Print the AST after all desugaring, just before lowering.
    DumpAstFinal
  deriving (Eq, Show)


-- | Compiler options for the front end compiler stage.
newtype Options = Options {optMode :: Mode} deriving (Eq, Show)


instance Default Options where
  def = Options{optMode = Continue}


-- | CLI options for the front end compiler stage.
options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ""
      ["dump-tokens"]
      (NoArg $ setMode DumpTokens)
      "Print the token stream from the scanner"
  , Option
      ""
      ["dump-ast"]
      (NoArg $ setMode DumpAst)
      "Print the initial parsed AST, before operators are parsed"
  , Option
      ""
      ["dump-ast-parsed"]
      (NoArg $ setMode DumpAstParsed)
      "Print the AST after operators are parsed"
  , Option
      ""
      ["dump-ast-final"]
      (NoArg $ setMode DumpAstFinal)
      "Print the AST after all desugaring, just before lowering to IR"
  ]
 where
  setMode :: Mode -> Options -> Options
  setMode m o = o{optMode = m}


-- | Parse a fully-formed AST from some String input.
parseAst :: Options -> String -> Pass A.Program
parseAst opt src = do
  when (optMode opt == DumpTokens) $ do
    ts <- scanTokens src
    dump $ prettyTokens ts

  ast <- parseProgram src
  when (optMode opt == DumpAst) $ dump $ show $ pretty ast

  astP <- parseOperators ast
  when (optMode opt == DumpAstParsed) $ dump $ show $ pretty astP

  astS <- desugarStrings astP
  astL <- desugarLists astS
  astT <- desugarPatTup astL

  when (optMode opt == DumpAstFinal) $ dump $ show $ pretty astT
  return astT


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
