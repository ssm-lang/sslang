{-# LANGUAGE OverloadedStrings #-}
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
