module Tests.ParseLoopSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , pending
                                                , shouldBe
                                                )

import           Front.Ast
import           Front.Parser                   ( parseProgram )


spec :: Spec
spec = do
  it "parses a basic function with a loop and some waits" $ do
    let
      input = unlines
        [ "main (clk : &Int) ="
        , "  loop"
        , "    wait clk"
        , "    wait clk"
        , "    wait clk"
        ]
      output = Program
        [ Function
            "main"
            [Bind "clk" (Just (TApp (TCon "&") (TCon "Int")))]
            (Loop
              (Seq (Seq (Wait [Id "clk"]) (Wait [Id "clk"])) (Wait [Id "clk"]))
            )
            (ReturnType (TCon "()"))
        ]
    parseProgram input `shouldBe` Right output
