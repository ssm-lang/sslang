module Tests.ParsePatternMatchSpec where

import           Front.Ast
import           Front.Parser                   ( parseProgram )
import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )
import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , pending
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  it "parses a basic function with a pattern match" $ do
    let
      input = unlines
        [ "main (x: Int, clk : &Int) ="
        , "  match x"
        , "    id => wait clk"
        , "    _ => wait clk"
        ]
      output = Program
        [ DefFn
            "main"
            [ PatTup
                [ PatAnn (TCon "Int")                   (PatId "x")
                , PatAnn (TApp (TCon "&") (TCon "Int")) (PatId "clk")
                ]
            ]
            TypNone
            (Match
              (Id "x")
              [(PatId "id", Wait [Id "clk"]), (PatWildcard, Wait [Id "clk"])]
            )
        ]
    parseProgram input `shouldBe` Right output
