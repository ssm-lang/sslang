{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ScanCommentsSpec where

import           Sslang.Test

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )

spec :: Spec
spec = do
  it "ignores single-line comments" $ do
    scanTokenTypes "// no" `shouldProduce` []
  it "scans tokens before single-line comments" $ do
    scanTokenTypes "42 // no" `shouldProduce` [TInteger 42]
    scanTokenTypes "24// no" `shouldProduce` [TInteger 24]

  it "supports leading comments" $ do
    let input = [here|

        // hello

        42

        |]
        -- The leading TDBar is perhaps unexpected, but is tolerated by our
        -- parser.
        output = [TDBar, TInteger 42]
    scanTokenTypes input `shouldProduce` output

  it "supports trailing comments" $ do
    let input = [here|

        42

        // hello

        |]
        output = [TInteger 42]
    scanTokenTypes input `shouldProduce` output
