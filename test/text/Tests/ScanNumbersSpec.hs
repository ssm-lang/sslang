{-# LANGUAGE OverloadedStrings #-}
module Tests.ScanNumbersSpec where

import           Sslang.Test

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..))

spec :: Spec
spec = do
  it "scans a simple integer" $ do
    scanTokenTypes "123" `shouldProduce` [TInteger 123]

  it "scans a simple integer surrounded by spaces" $ do
    scanTokenTypes " 321 " `shouldProduce` [TInteger 321]

  it "scans consecutive, space-separated numbers" $ do
    scanTokenTypes " 123 321 " `shouldProduce` [TInteger 123, TInteger 321]
