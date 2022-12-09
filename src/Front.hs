{-# LANGUAGE OverloadedStrings #-}
{- | Front end of the compiler pipeline.

Throughout this stage, high-level syntax is progressively parsed and desugared
into simpler AST constructs.
-}
module Front where

import           Common.Compiler                ( Pass
                                                , dump
                                                )
import           Common.Default                 ( Default(..) )

import qualified Front.Ast                     as A

import           Front.DesugarLists             ( desugarLists )
import           Front.DesugarStrings           ( desugarStrings )
import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parseProgram )
import qualified Front.Pattern                 as Pattern
import           Front.Scanner                  ( scanTokens )
import           Front.Scope                    ( scopeProgram )
import           Front.Token                    ( prettyTokens )

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
  when (optMode opt == DumpTokens) $ do
    ts <- scanTokens src
    dump $ prettyTokens ts

  ast <- parseProgram src
  when (optMode opt == DumpAst) $ dump $ show $ pretty ast

  astP <- parseOperators ast
  when (optMode opt == DumpAstParsed) $ dump $ show $ pretty astP

  astS <- desugarStrings astP
  astL <- desugarLists astS

  -- TODO: other desugaring
  let astTuple2 = insertTypeDef pairDef astL
  let astTuple3 = insertTypeDef pair3Def astTuple2
  let astTuple4 = insertTypeDef pair4Def astTuple3
  Pattern.checkAnomaly astTuple4
  astD <- Pattern.desugarProgram astTuple4


  when (optMode opt == DumpAstFinal) $ dump $ show $ pretty astD
  return astD
  where insertTypeDef typedef (A.Program xs) = A.Program (A.TopType typedef : xs)
        pairDef = A.TypeDef { A.typeName = "Pair", A.typeParams = ["a","b"], A.typeVariants = [A.VariantUnnamed "Pair" [A.TCon "a", A.TCon "b"]]}
        pair3Def = A.TypeDef { A.typeName = "Pair3", A.typeParams = ["a","b","c"], A.typeVariants = [A.VariantUnnamed "Pair3" [A.TCon "a", A.TCon "b", A.TCon "c"]]}
        pair4Def = A.TypeDef { A.typeName = "Pair4", A.typeParams = ["a","b","c","d"], A.typeVariants = [A.VariantUnnamed "Pair4" [A.TCon "a", A.TCon "b", A.TCon "c",A.TCon "d"]]}
    
-- | Semantic checking on an AST.
checkAst :: Options -> A.Program -> Pass ()
checkAst _opt ast = do
  scopeProgram ast
  Pattern.checkAnomaly ast

-- | Front end compiler stage.
run :: Options -> String -> Pass A.Program
run opt src = do
  ast <- parseAst opt src
  checkAst opt ast
  return ast
