module Tests.ScanOperatorsSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , shouldBe
                                                )

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )

spec :: Spec
spec = do
  it "scans basic operators as TOps" $ mapM_
    (\op -> scanTokenTypes op `shouldBe` Right [TOp op])
    [ "!"
    , "#"
    , "$"
    , "%"
    , "*"
    , "+"
    , "."
    , "/"
    , "<"
    , "='"
    , ">"
    , "?"
    , "@_"
    , "\\"
    , "^"
    , "|\""
    , "-"
    , "~"
    , "!_:\"'"
    ]

  it "scans infix identifiers as TOps" $ mapM_
    (\op -> scanTokenTypes ("`" ++ op ++ "`") `shouldBe` Right [TOp op])
    ["add", "plus", "foo_42_bar", "foo_bar_'2"]
