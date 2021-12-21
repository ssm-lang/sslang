{-# LANGUAGE OverloadedStrings #-}
module Tests.ScanOperatorsSpec where

import           Sslang.Test

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )
import           Common.Identifiers             ( fromString )

spec :: Spec
spec = do
  it "scans basic operators as TOps" $ mapM_
    (\op -> scanTokenTypes op `shouldProduce` [TOp $ fromString op])
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
    (\op -> scanTokenTypes ("`" ++ op ++ "`") `shouldProduce` [TOp $ fromString op])
    ["add", "plus", "foo_42_bar", "foo_bar_'2"]
