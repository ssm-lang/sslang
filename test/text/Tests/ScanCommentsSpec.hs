{-# LANGUAGE OverloadedStrings #-}
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
