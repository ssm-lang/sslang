{-# LANGUAGE OverloadedStrings #-}
module Tests.ScanOperatorsSpec where

import           Sslang.Test

import           Common.Identifiers             ( fromString )
import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )

spec :: Spec
spec = do
  it "scans basic operators as TOps" $ do
    let t :: HasCallStack => String -> Expectation
        t op = scanTokenTypes op `shouldProduce` [TOp $ fromString op]
    t "!"
    t "#"
    t "%"
    t "*"
    t "+"
    t "."
    t "/"
    t "<"
    t "='"
    t ">"
    t "?"
    t "@_"
    t "\\"
    t "^"
    t "|\""
    t "-"
    t "~"
    t "!_:\"'"

  it "scans infix identifiers as TOps" $ do
    let
      t :: HasCallStack => String -> Expectation
      t op =
        scanTokenTypes ("`" ++ op ++ "`") `shouldProduce` [TOp $ fromString op]

    t "add"
    t "plus"
    t "foo_42_bar"
    t "foo_bar_'2"
