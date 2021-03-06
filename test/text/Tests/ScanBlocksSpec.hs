{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ScanBlocksSpec where

import           Sslang.Test

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )

spec :: Spec
spec = do
  it "supports line continuations" $ do
    let input = [here|
          some long line
              is continued with more indentation
          but this is not
        |]
        output =
          [ TId "some"
          , TId "long"
          , TId "line"
          , TId "is"
          , TId "continued"
          , TId "with"
          , TId "more"
          , TId "indentation"
          , TDBar
          , TId "but"
          , TId "this"
          , TId "is"
          , TId "not"
          ]
    scanTokenTypes input `shouldProduce` output

  it "supports layout blocks with implicit braces" $ do
    let input = [here|
          loop a
               b
        |]
        output =
          [ TLoop
          , TLbrace
          , TId "a"
          , TSemicolon
          , TId "b"
          , TRbrace
          ]
    scanTokenTypes input `shouldProduce` output

  it "supports layout blocks with implicit braces, followed by continuation" $ do
    let input = [here|
          loop a
                b
               c
           d
               e
        |]
        output =
          [ TLoop
          , TLbrace
          , TId "a"
          , TId "b"
          , TSemicolon
          , TId "c"
          , TRbrace
          , TId "d"
          , TId "e"
          ]
    scanTokenTypes input `shouldProduce` output

  it "supports layout blocks with explicit braces" $ do
    let input = [here|
          loop {
              g
          x
            }
        |]
        output =
          [ TLoop
          , TLbrace
          , TId "g"
          , TId "x"
          , TRbrace
          ]
    scanTokenTypes input `shouldProduce` output

  it "supports layout blocks with explicit braces and semicolons" $ do
    let input = [here|
          loop {
            g;
              x
          }
        |]
        output =
          [ TLoop
          , TLbrace
          , TId "g"
          , TSemicolon
          , TId "x"
          , TRbrace
          ]
    scanTokenTypes input `shouldProduce` output
