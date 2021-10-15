module Tests.ScanBlocksSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , shouldBe
                                                , pending
                                                )

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )



spec :: Spec
spec = do
  it "supports line continuations" $ do
    let input = unlines
          [ "some long line"
          , "    is continued with more indentation"
          , "but this is not"
          ]
        output =
          [ TId "some"
          , TId "long"
          , TId "line"
          , TId "is"
          , TId "continued"
          , TId "with"
          , TId "more"
          , TId "indentation"
          , TSemicolon
          , TId "but"
          , TId "this"
          , TId "is"
          , TId "not"
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports do blocks with implicit braces" $ do
    let input = unlines
          [ "f do a"
          , "     b"
          ]
        output =
          [ TId "f"
          , TLbrace
          , TId "a"
          , TSemicolon
          , TId "b"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports do blocks with implicit braces, followed by continuation" $ do
    let input = unlines
          [ "f do a"
          , "     b"
          , "    c"
          , "   d"
          ]
        output =
          [ TId "f"
          , TLbrace
          , TId "a"
          , TSemicolon
          , TId "b"
          , TRbrace
          , TId "c"
          , TId "d"
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports do blocks with explicit braces" $ do
    let input = unlines
          [ "f {"
          , "  g"
          , "    x"
          , "}"
          ]
        output =
          [ TId "f"
          , TLbrace
          , TId "g"
          , TId "x"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports do blocks with explicit braces and semicolons" $ do
    let input = unlines
          [ "f {"
          , "  g;"
          , "    x"
          , "}"
          ]
        output =
          [ TId "f"
          , TLbrace
          , TId "g"
          , TSemicolon
          , TId "x"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output
