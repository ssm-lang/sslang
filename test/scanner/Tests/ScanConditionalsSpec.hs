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
    let input = "if a { b }"
        output =
          [ TIf
          , TId "a"
          , TLbrace
          , TId "b"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans conditionals blocks" $ do
    let input = unlines
          [ "if a"
          , "  b"
          ]
        output =
          [ TIf
          , TId "a"
          , TLbrace
          , TId "b"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans loops blocks" $ do
    let input = unlines
          [ "loop"
          , "  some_action"
          , "  some_other_action"
          ]
        output =
          [ TLoop
          , TLbrace
          , TId "some_action"
          , TSemicolon
          , TId "some_other_action"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans inline loops" $ do
    let input = "loop some_action"
        output  =
          [ TLoop
          , TLbrace
          , TId "some_action"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output
