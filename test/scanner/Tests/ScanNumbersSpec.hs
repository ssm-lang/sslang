module Tests.ScanNumbersSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , shouldBe
                                                )

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..))

spec :: Spec
spec = do
  it "scans a simple integer" $ do
    scanTokenTypes "123" `shouldBe` Right [TInteger 123]

  it "scans a simple integer surrounded by spaces" $ do
    scanTokenTypes " 321 " `shouldBe` Right [TInteger 321]

  it "scans consecutive, space-separated numbers" $ do
    scanTokenTypes " 123 321 " `shouldBe` Right [TInteger 123, TInteger 321]
