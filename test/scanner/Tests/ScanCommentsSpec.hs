{-# LANGUAGE OverloadedStrings #-}
module Tests.ScanCommentsSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , shouldBe
                                                )

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )

spec :: Spec
spec = do
  it "ignores single-line comments" $ do
    scanTokenTypes "// no" `shouldBe` Right []
  it "scans tokens before single-line comments" $ do
    scanTokenTypes "42 // no" `shouldBe` Right [TInteger 42]
    scanTokenTypes "24// no" `shouldBe` Right [TInteger 24]
