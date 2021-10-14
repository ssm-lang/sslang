module Tests.ScanConditionalsSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , shouldBe
                                                , pendingWith
                                                )

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )



spec :: Spec
spec = do
  it "scans inline conditionals" $ do
    pendingWith "help needed"

  it "scans conditionals blocks" $ do
    pendingWith "help needed"

  it "scans loops blocks" $ do
    pendingWith "help needed"

  it "scans inline loops" $ do
    pendingWith "help needed"
