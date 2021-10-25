module Tests.PrettyAstSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , pending
                                                , shouldBe
                                                )

import           Data.Bifunctor                 ( bimap )
import           Data.Either                    ( fromRight )
import           Front                          ( renderAst )
import           Front.Ast
import           Front.Parser                   ( parseProgram )

renderAndParse :: Program -> Either String Program
renderAndParse = parseProgram . renderAst

spec :: Spec
spec = do
  it "prints a basic function with a loop and some waits" $ do
    let input = parseProgram $ unlines
          [ "main (clk : &Int) ="
          , "  loop"
          , "    wait clk"
          , "    wait clk"
          , "    wait clk"
          ]
        output = input >>= renderAndParse
    input `shouldBe` output

  it "prints a function with a postfix type signature" $ do
    let input = parseProgram $ unlines
          [ "main clk oth: &Int -> &(Int, Int) -> () ="
          , "  wait clk"
          , "  oth <- 5"
          ]
        output = input >>= renderAndParse
    input `shouldBe` output

  it "prints a function with an inline type signature" $ do
    let input = parseProgram $ unlines
          [ "main (clk: &Int, oth: &(Int, Int)) -> () ="
          , "  wait clk"
          , "  oth <- 5"
          ]
        output = input >>= renderAndParse
    input `shouldBe` output

  it "prints a function with no type signature" $ do
    let input = parseProgram $ unlines
          [ "main clk oth ="
          , "  wait clk"
          , "  oth <- 5"
          ]
        output = input >>= renderAndParse
    input `shouldBe` output
