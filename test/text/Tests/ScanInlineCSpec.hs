{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ScanInlineCSpec where

import           Sslang.Test

import           Control.Monad                  ( forM_ )
import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )

mkBlock :: String -> String
mkBlock s = "$$" ++ s ++ "$$"

groupChunks :: [TokenType] -> [TokenType]
groupChunks = foldr groupChunks' []
groupChunks' (TCBlock s1) (TCBlock s2 : ts) = TCBlock (s1 ++ s2) : ts
groupChunks' t            ts                = t : ts

unescapeSigils :: String -> String
unescapeSigils = foldr unescapeSigils' []
unescapeSigils' '$' ('$' : '$' : '$' : ss) = '$' : '$' : ss
unescapeSigils' s   ss                     = s : ss

spec :: Spec
spec = do
  it "scans C symbols" $ do
    scanTokenTypes "$hello" `shouldProduce` [TCSym "hello"]
    scanTokenTypes "$foo $bar" `shouldProduce` [TCSym "foo", TCSym "bar"]
    scanTokenTypes "24 $abc 42"
      `shouldProduce` [TInteger 24, TCSym "abc", TInteger 42]


  it "scans embedded C blocks" $ do
    let contents = "block contents"
    scanTokenTypes (mkBlock contents) `shouldProduce` [TCBlock contents]

  it "scans multi-line C blocks" $ do
    let contents = [here|
            multi-line
            C block contents
          |]
    scanTokenTypes (mkBlock contents) `shouldProduce` [TCBlock contents]

  it "scans multi-line C blocks containing comments" $ do
    let contents = [here|
            contents containing
            C-style // comments
            /* like this */ one and this /* one // */
          |]
    scanTokenTypes (mkBlock contents) `shouldProduce` [TCBlock contents]


  it "scans C block containing regular sigils" $ do
    let contents = [here|
            blocks containing sigils like $ this
            should be ok
          |]
    (groupChunks <$> scanTokenTypes (mkBlock contents))
      `shouldProduce` [TCBlock contents]


  it "scans C block containing escaped double sigils" $ do
    let contents = [here|
            blocks containing
            escaped block end tokens
            i.e., this: $$$$
            shoudl still be ok
          |]
    (groupChunks <$> scanTokenTypes (mkBlock contents))
      `shouldProduce` [TCBlock $ unescapeSigils contents]
