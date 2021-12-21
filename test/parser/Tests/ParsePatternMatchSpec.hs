{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ParsePatternMatchSpec where

import           Sslang.Test

import           Front.Ast
import           Front.Parser                   ( parseProgram )

spec :: Spec
spec = do
  it "parses a basic function with a pattern match" $ do
    let
      input = [here|
        main (x: Int, clk : &Int) =
          match x
            id = wait clk
            _  = wait clk
      |]
      output = Program
        [ DefFn
            "main"
            [ PatTup
                [ PatAnn (TCon "Int")                   (PatId "x")
                , PatAnn (TApp (TCon "&") (TCon "Int")) (PatId "clk")
                ]
            ]
            TypNone
            (Match
              (Id "x")
              [(PatId "id", Wait [Id "clk"]), (PatWildcard, Wait [Id "clk"])]
            )
        ]
    parseProgram input `shouldProduce` output
