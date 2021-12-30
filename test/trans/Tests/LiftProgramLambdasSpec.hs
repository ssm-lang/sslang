{-# LANGUAGE QuasiQuotes #-}
module Tests.LiftProgramLambdasSpec where

import           Sslang.Test

import qualified Front
import qualified IR
import qualified IR.IR                         as I
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.Types.Poly                 as Poly

parseLift :: String -> Pass (I.Program Poly.Type)
parseLift s =
  Front.run def s
    >>= IR.lower def
    >>= IR.ann2Class def
    >>= IR.class2Poly def
    >>= liftProgramLambdas

spec :: Spec
spec = do
  it "lifts a lambda without free variables" $ do
    let unlifted = parseLift [here|
          bar: Int = 5
          baz x: Int -> Int =
            x + 1
          foo y: Int -> Int =
            let adder (z: Int) -> Int = z + 1
            adder y
        |]
        lifted = parseLift [here|
          bar: Int = 5
          baz x: Int -> Int =
            x + 1
          anon0 z: Int -> Int =
            z + 1
          foo y: Int -> Int =
            let adder = anon0
            adder y
        |]
    unlifted `shouldPassAs` lifted

  it "lifts a lambda with free variables" $ do
    let unlifted = parseLift [here|
          bar: Int = 5
          baz x: Int -> Int =
            x + 1
          foo y: Int -> Int =
            let w = 1
                adder (z: Int) -> Int = z + bar + w
            adder y
        |]
        lifted = parseLift [here|
          bar: Int = 5
          baz x: Int -> Int =
            x + 1
          anon0 (w: Int) (z: Int) -> Int =
            z + bar + w
          foo y: Int -> Int =
            let w = 1
                adder = anon0 w
            adder y
        |]
    lifted `shouldPassAs` unlifted

  it "lifts nested lambdas with free variables" $ do
    let unlifted = parseLift [here|
          foo (x: Int) (y: Int) -> Int =
            let z = 5
                g (a: Int) -> Int =
                  let h (b: Int) -> Int = a + b + x
                  h z
            g y
        |]
        lifted = parseLift [here|
          anon0 (a: Int) (x: Int) (b: Int) -> Int =
            a + b + x
          anon1 (x: Int) (z: Int) (a: Int) -> Int =
            let h = anon0 a x
            h z
          foo (x: Int) (y: Int) -> Int =
            let z = 5
                g = anon1 x z
            g y
        |]
    lifted `shouldPassAs` unlifted
