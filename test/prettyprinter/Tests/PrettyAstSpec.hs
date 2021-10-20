module Tests.PrettyAstSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , pending
                                                , shouldBe
                                                )

import           Front                          ( renderAst )
import           Front.Ast
import           Front.Parser                   ( parseProgram )

parseAndRender :: String -> String
parseAndRender input = case parseProgram input of
  Left  e   -> error e
  Right ast -> renderAst ast

spec :: Spec
spec = do
  it "prints a basic function with a loop and some waits" $ do
    let input = unlines
          [ "main (clk : &Int) ="
          , "  loop"
          , "    wait clk"
          , "    wait clk"
          , "    wait clk"
          ]
        pAst1 = parseAndRender input
        pAst2 = parseAndRender pAst1
    pAst1 `shouldBe` pAst2
  it "prints a function with a postfix type signature" $ do
    let input = unlines
          [ "main clk oth: &Int -> &(Int, Int) -> () ="
          , "  wait clk"
          , "  oth <- 5"
          ]
        pAst1 = parseAndRender input
        pAst2 = parseAndRender pAst1
    pAst2 `shouldBe` pAst1
  it "prints a function with an inline type signature" $ do
    let input = unlines
          [ "main (clk: &Int, oth: &(Int, Int)) -> () ="
          , "  wait clk"
          , "  oth <- 5"
          ]
        pAst1 = parseAndRender input
        pAst2 = parseAndRender pAst1
    pAst2 `shouldBe` pAst1
