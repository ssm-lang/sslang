{-# LANGUAGE QuasiQuotes #-}
module Tests.PrettyAstSpec where

import Sslang.Test ( here, it, shouldPassExactlyAs, Spec, Pass )

import           Common.Pretty                  ( spaghetti )
import           Front.Ast                      ( Program )
import           Front.Parser                   ( parseProgram )

renderAndParse :: Program -> Pass Program
renderAndParse = parseProgram . spaghetti

spec :: Spec
spec = do
  it "prints a basic function with a loop and some waits" $ do
    let input = parseProgram [here|
          main (clk : &Int) =
            loop
              wait clk
              wait clk
              wait clk
        |]
        output = input >>= renderAndParse
    input `shouldPassExactlyAs` output

  it "prints a function with a postfix type signature" $ do
    let input = parseProgram [here|
          main clk oth: &Int -> &(Int, Int) -> () =
            wait clk
            oth <- 5
        |]
        output = input >>= renderAndParse
    input `shouldPassExactlyAs` output

  it "prints a function with an inline type signature" $ do
    let input = parseProgram [here|
          main (clk: &Int, oth: &(Int, Int)) -> () =
            wait clk
            oth <- 5
        |]
        output = input >>= renderAndParse
    input `shouldPassExactlyAs` output

  it "prints a function with no type signature" $ do
    let input = parseProgram [here|
          main clk oth =
            wait clk
            oth <- 5
        |]
        output = input >>= renderAndParse
    input `shouldPassExactlyAs` output

  it "prints a function with a pattern match" $ do
    let input = parseProgram [here|
          main (x: Int, clk : &Int) =
            match x
              id = wait clk
              _  = wait clk
        |]
        output = input >>= renderAndParse
    input `shouldPassExactlyAs` output
