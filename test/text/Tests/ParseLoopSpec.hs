{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ParseLoopSpec where

import           Sslang.Test

import           Front.Ast
import           Front.Parser                   ( parseProgram )


spec :: Spec
spec = do
  it "parses a basic function with a loop and some waits" $ do
    let
      input = [here|
        main (clk : &Int) =
          loop
            wait clk
            wait clk
            wait clk
        |]
      output = Program
        [ TopDef $ DefFn
            "main"
            [PatAnn (TApp (TCon "&") (TCon "Int")) (PatId "clk")]
            TypNone
            (Loop
              (Seq (Wait [Id "clk"]) (Seq (Wait [Id "clk"]) (Wait [Id "clk"])))
            )
        ]
    parseProgram input `shouldProduce` output
