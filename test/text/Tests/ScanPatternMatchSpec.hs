{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ScanPatternMatchSpec where

import           Sslang.Test

import           Front.Scanner                  ( scanTokenTypes )
import           Front.Token                    ( TokenType(..) )



spec :: Spec
spec = do
    it "scans pattern matches" $ do
        let input = [here|
              main (x: Int, clk : &Int) =
                match x
                  id => wait clk
                  _ => wait clk
            |]
            output =
                [ TId "main"
                , TLparen
                , TId "x"
                , TColon
                , TId "Int"
                , TComma
                , TId "clk"
                , TColon
                , TAmpersand
                , TId "Int"
                , TRparen
                , TEq
                , TLbrace
                , TMatch
                , TId "x"
                , TLbrace
                , TId "id"
                , TDRarrow
                , TLbrace
                , TWait
                , TLbrace
                , TId "clk"
                , TRbrace
                , TRbrace
                , TBar
                , TUnderscore
                , TDRarrow
                , TLbrace
                , TWait
                , TLbrace
                , TId "clk"
                , TRbrace
                , TRbrace
                , TRbrace
                , TRbrace
                ]
        scanTokenTypes input `shouldProduce` output
