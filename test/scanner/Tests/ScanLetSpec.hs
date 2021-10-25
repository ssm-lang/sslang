module Tests.ScanLetSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , pendingWith
                                                , shouldBe
                                                )

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )


spec :: Spec
spec = do
  it "scans basic let definition" $ do
    let input = "do let variable = definition"
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "variable"
          , TEq
          , TLbrace
          , TId "definition"
          , TRbrace
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans sequenced let definitions" $ do
    let input = unlines
          [ "do"
          , "  let a = a_def"
          , "  let b = b_def"
          , "  c"
          ]
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TId "a_def"
          , TRbrace
          , TRbrace
          , TSemicolon
          , TLet
          , TLbrace
          , TId "b"
          , TEq
          , TLbrace
          , TId "b_def"
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "c"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output


  it "scans inline nested let definitions" $ do
    let input = "let a = { let b = { c }; d }; e"
        output =
          [ TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TLet
          , TLbrace
          , TId "b"
          , TEq
          , TLbrace
          , TId "c"
          , TRbrace
          , TSemicolon
          , TId "d"
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "e"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans grouped let-bindings" $ do
    let input = unlines
          [ "do"
          , " let a = a_block"
          , "     b = b_block_1"
          , "         b_block_2"
          , "           b_block_2_cont"
          , " outside_let"
          ]
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TId "a_block"
          , TRbrace
          , TDBar
          , TId "b"
          , TEq
          , TLbrace
          , TId "b_block_1"
          , TSemicolon
          , TId "b_block_2"
          , TId "b_block_2_cont"
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "outside_let"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports explicit braces around definition" $ do
    let input = unlines
          [ "do"
          , " let a = { a_block_1_1"
          , "             a_block_1_2;"
          , "           a_block_2_1"
          , "             a_block_2_2 }"
          , " outside_let"
          ]
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TId "a_block_1_1"
          , TId "a_block_1_2"
          , TSemicolon
          , TId "a_block_2_1"
          , TId "a_block_2_2"
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "outside_let"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports explicit braces around bind group" $ do
    let input = unlines
          [ "do"
          , " let { a = b"
          , "   ||  b = a }"
          ]
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TId "b"
          , TRbrace
          , TDBar
          , TId "b"
          , TEq
          , TLbrace
          , TId "a"
          , TRbrace
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output
    -- pendingWith "help needed"
    -- let { a = b
    --   || b = a }
    --
    -- though this looks rather unsightly
    -- TODO(hans): The scanner isn't actually OK with this

  it "supports parenthesized definition in block" $ do
    let input = unlines
          [ "do"
          , " let a = (a"
          , "          b c)"
          , "          d"
          , "         e"
          ]
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TLparen
          , TId "a"
          , TId "b"
          , TId "c"
          , TRparen
          , TId "d"
          , TSemicolon
          , TId "e"
          , TRbrace
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports nested let-block" $ do
    let input = unlines
          [ "do"
          , "  let a = let b = c"
          , "              c = b"
          , "  d"
          ]
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TLet
          , TLbrace
          , TId "b"
          , TEq
          , TLbrace
          , TId "c"
          , TRbrace
          , TDBar
          , TId "c"
          , TEq
          , TLbrace
          , TId "b"
          , TRbrace
          , TRbrace
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "d"
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports let-bound function definitions" $ do
    let input = unlines
          [ "do"
          , "  let f = let g x = 1"
          , "          g"
          ]
        output =
          [ TDo
          , TLbrace
          , TLet
          , TLbrace
          , TId "f"
          , TEq
          , TLbrace
          , TLet
          , TLbrace
          , TId "g"
          , TId "x"
          , TEq
          , TLbrace
          , TInteger 1
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "g"
          , TRbrace
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output
