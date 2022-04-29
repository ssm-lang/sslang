{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ParseBlockSpec where

import           Sslang.Test

import           Front.Ast
import           Front.Parser                   ( parseProgram )

shouldParse :: HasCallStack => String -> Expectation
shouldParse = shouldPass . parseProgram

spec :: Spec
spec = do
  it "parses basic layout-next-token blocks" $ do
    shouldParse [here|
      main (clk : &Int) =
        loop
          clk
    |]
    shouldParse [here|
      main (clk : &Int) =
        loop
          wait clk
    |]
    shouldParse [here|
      main (clk : &Int) =
        loop
          wait clk
          loop
            wait clk
    |]
    shouldParse [here|
      main (clk : &Int) =
        loop
          wait clk
          loop
            wait clk
          loop
            loop
              wait clk
        loop
          wait clk
    |]
    shouldParse [here|
      main (clk : &Int) =
        loop
          wait clk
          loop
            wait clk
    |]

  it "parses basic layout-next-line blocks" $ do
    shouldParse [here|
      type MyType
        Foo1 Int
        Foo2 Int
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          wait clk
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          if f clk
            clk
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          if f clk
            wait clk
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          if f clk
            wait clk
          if False
            23
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          if f clk
            clk
          if False
            23
    |]

  it "parses if-else blocks" $ do
    shouldParse [here|
      main (clk : &Int) =
        if True
          32
        else
          23
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          if False
            32
        else
          23
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          if False
            32
          else
            12
        else
          23
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          if False
            32
          else
            12
    |]
    shouldParse [here|
      main (clk : &Int) =
        if True
          23
        else
          if False
            32
          else
            12
    |]
    -- TODO: add failing test cases for dangling else

  it "parses let-blocks" $ do
    shouldParse [here|
      main (clk : &Int) =
        let x = 3
        1
    |]
    shouldParse [here|
      main (clk : &Int) =
        let x = 3
        let y = 3
        1
    |]
    shouldParse [here|
      main (clk : &Int) =
        let x = 3
            y = 3
        1
    |]
    shouldParse [here|
      main (clk : &Int) =
        let x = 3
                4
            y = 3
        1
    |]
    shouldParse [here|
      main (clk : &Int) =
        let x = let y = 3
                    z = 22
                1
            y = 126
                3
        True
    |]
    -- TODO: adding pending test cases for
    -- let x =
    --   ...
