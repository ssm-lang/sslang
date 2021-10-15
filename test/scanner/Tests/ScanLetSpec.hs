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
    let input = "let variable = definition"
        output =
          [ TLet
          , TLbrace
          , TId "variable"
          , TEq
          , TLbrace
          , TId "definition"
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans sequenced let definitions" $ do
    let input = unlines
          [ "let a = a_def"
          , "let b = b_def"
          , "c"
          ]
        output =
          [ TLet
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
          ]
    scanTokenTypes input `shouldBe` Right output


  it "scans nested let definitions" $ do
    let input = "let a = let b = c; d"
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
          , TSemicolon
          , TId "d"
          , TRbrace
          , TRbrace
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans block let definitions" $ do
    let input = unlines
          [ "let a = a_block_1_1"
          , "          a_block_1_2"
          , "        a_block_2_1"
          , "          a_block_2_2"
          , "outside_let"
          ]
        output =
          [ TLet
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
          ]
    scanTokenTypes input `shouldBe` Right output

  it "scans grouped let-bindings" $ do
    let input = unlines
          [ "let a = a_block"
          , "    b = b_block_1"
          , "        b_block_2"
          , "          b_block_2_cont"
          , "outside_let"
          ]
        output =
          [ TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TId "a_block"
          , TRbrace
          , TAnd
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
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports explicit braces around definition" $ do
    let input = unlines
          [ "let a = { a_block_1_1"
          , "            a_block_1_2;"
          , "          a_block_2_1"
          , "            a_block_2_2 }"
          , "outside_let"
          ]
        output =
          [ TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TLbrace -- Inserts extra brace pair?
          , TId "a_block_1_1"
          , TId "a_block_1_2"
          , TSemicolon
          , TId "a_block_2_1"
          , TId "a_block_2_2"
          , TRbrace
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "outside_let"
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports explicit braces around bind group" $ do
    pendingWith "help needed"
    -- let { a = b
    --   and b = a }
    --
    -- though this looks rather unsightly
    -- TODO(hans): The scanner isn't actually OK with this

  it "supports parenthesized definition in block" $ do
    let input = unlines
          [ "let a = (a"
          , "b c)"
          , "        d"
          ]
        output =
          [ TLet
          , TLbrace
          , TId "a"
          , TEq
          , TLbrace
          , TLparen
          , TId "a"
          , TId "b"
          , TId "c"
          , TRparen
          , TSemicolon
          , TId "d"
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports nested let-block" $ do
    let input = unlines
          [ "let a = let b = c"
          , "            c = b"
          , "        d"
          ]
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
          , TAnd
          , TId "c"
          , TEq
          , TLbrace
          , TId "b"
          , TRbrace
          , TRbrace
          , TSemicolon
          , TId "d"
          , TRbrace
          , TRbrace
          ]
    scanTokenTypes input `shouldBe` Right output

  it "supports let-bound function definitions" $ do
    let input = unlines
          [ "let f = let g x = 1"
          , "        g"
          ]
        output =
          [ TLet
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
          ]
    scanTokenTypes input `shouldBe` Right output
