{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ScanConditionalsSpec where

import           Sslang.Test

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )



spec :: Spec
spec = do
  it "scans inline conditionals" $ do
    let input = [here|
          if a { b }
        |]
        output =
          [ TIf
          , TId "a"
          , TLbrace
          , TId "b"
          , TRbrace
          ]
    scanTokenTypes input `shouldProduce` output

  it "scans conditionals blocks" $ do
    let input = [here|
          if a
            b
        |]
        output =
          [ TIf
          , TId "a"
          , TLbrace
          , TId "b"
          , TRbrace
          ]
    scanTokenTypes input `shouldProduce` output

  it "scans loops blocks" $ do
    let input = [here|
          loop
            some_action
            some_other_action
        |]
        output =
          [ TLoop
          , TLbrace
          , TId "some_action"
          , TSemicolon
          , TId "some_other_action"
          , TRbrace
          ]
    scanTokenTypes input `shouldProduce` output

  it "scans inline loops" $ do
    let input = [here|
          loop some_action
        |]
        output  =
          [ TLoop
          , TLbrace
          , TId "some_action"
          , TRbrace
          ]
    scanTokenTypes input `shouldProduce` output
