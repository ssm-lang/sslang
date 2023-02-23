{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.AnomalySpec where

import           Sslang.Test

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Front
import qualified Front.Ast                     as A
import qualified IR
import qualified IR.IR                         as I
import           IR.Pattern.Anomaly          ( checkProgram )
import qualified IR.Pattern.Matrix          as PM

import           Control.Monad                  ( (>=>) )

doAnomalyCheck :: String -> Pass (I.Program I.Type)
doAnomalyCheck = Front.run def >=> IR.lower def >=> IR.typecheck def >=> IR.anomalycheck

spec :: Spec
spec = do
  describe "pure stuff" $ do
    it "defaultize" $ do
      PM.emptyWithCols 0
        `shouldBe` (PM.defaultize . PM.singleCol $ [I.AltLit (I.LitIntegral 1)])
  describe "allows valid matches" $ do
    it "allows simple data constructor match" $ do
      shouldPass $ doAnomalyCheck [here|
        type Foo
          Foo1
          Foo2

        f x =
          match x
            Foo1 = 1
            Foo2 = 2
      |]
    it "allows simple parametrized data constructor match" $ do
      shouldPass $ doAnomalyCheck [here|
        type Foo a
          Foo1 a
          Foo2

        f x =
          match x
            Foo1 _ = 1
            Foo2 = 2
      |]
    it "allows recursive parametrized data constructor match" $ do
      shouldPass $ doAnomalyCheck [here|
        type Foo a
          Foo1 a
          Foo2

        f x =
          match x
            Foo1 1 = 1
            Foo1 2 = 2
            Foo1 _ = 3
            Foo2 = 4
      |]
    it "allows List data type match" $ do
      shouldPass $ doAnomalyCheck [here|
        type List a
          Cons a (List a)
          Nil

        f x =
          match x
            Cons _ _ = 1
            Nil = 2
      |]
    it "allows integer match" $ do
      shouldPass $ doAnomalyCheck [here|
        f x =
          match x
            1 = 1
            2 = 2
            _ = 3
      |]
    it "allows tuple match" $ do
      shouldPass $ doAnomalyCheck [here|
        f x =
          match x
            (1, 2) = 1
            (_, _) = 2
      |]

  describe "detects non-exhaustive matches" $ do
    it "detects integer literal non-exhaustive match" $ do
      shouldFail $ doAnomalyCheck [here|
        x =
          match 1
            1 = 1
            2 = 2
      |]
    it "detects integer literal non-exhaustive match" $ do
      shouldFail $ doAnomalyCheck [here|
        x =
          match 1
            1 = 1
            2 = 2
      |]
    it "detects data constructor non-exhaustive match" $ do
      shouldFail $ doAnomalyCheck [here|
        type List a
          Cons a (List a)
          Nil
        f x =
          match x
            Cons _ _ = 1
      |]
