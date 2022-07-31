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
    scanTokenTypes "42 // no" `shouldProduce` [TInteger 42]
    scanTokenTypes "24// no" `shouldProduce` [TInteger 24]

  it "ignore block comments" $ do
    scanTokenTypes "/*no*/" `shouldProduce` []
    scanTokenTypes "42 /*no*/" `shouldProduce` [TInteger 42]
    scanTokenTypes "24/*no*/" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope*/24" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope*/ 24" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope*/24/*no*/" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope*/ 24 /*no*/" `shouldProduce` [TInteger 24]

  it "ignore nested comments" $ do
    scanTokenTypes "/* /* no*/neither*/" `shouldProduce` []
    scanTokenTypes "42 /*no/*neither*/*/" `shouldProduce` [TInteger 42]
    scanTokenTypes "24/*nope/*no*/neither*/" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope/*no*/neither*/24" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope/*no*/neither*/ 24" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope/*no*/neither*/24/*no/*nope*/neither*/" `shouldProduce` [TInteger 24]
    scanTokenTypes "/*nope/*no*/neither*/ 24 /*no/*nope*/neither*/" `shouldProduce` [TInteger 24]

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

  it "supports multi-line comments" $ do
    let input = [here|

        /* Hello
          this is a*/

        42

        /* Multi-line
        */
        |]
        output = [TDBar, TInteger 42]
    scanTokenTypes input `shouldProduce` output

  it "supports nested multi-line comments" $ do
    let input = [here|

        /* Hello
          this is a
          /* Multi-line comment*/*/

        42 /* Hello this is a
        /* Multi-line comment*/*/
        |]
        output = [TDBar, TInteger 42]
    scanTokenTypes input `shouldProduce` output

  it "ignores mixed nested comments" $ do
    let input = [here|
        // /* this does not start a multi-line comment

        42 /* this does not
        ignore the // end delimiter */
        |]
        output = [TDBar, TInteger 42]
    scanTokenTypes input `shouldProduce` output
