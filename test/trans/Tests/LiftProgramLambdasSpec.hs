{-# LANGUAGE QuasiQuotes #-}
module Tests.LiftProgramLambdasSpec where

import           Sslang.Test

import qualified Front
import qualified IR
import qualified IR.IR                         as I
import           IR.LambdaLift                  ( liftProgramLambdas )

parseLift :: String -> Pass (I.Program I.Type)
parseLift s =
  Front.run def s
    >>= IR.lower def
    >>= IR.typecheck def
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
          foo_adder_anon z: Int -> Int =
            z + 1
          foo y: Int -> Int =
            let adder = foo_adder_anon
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
          foo_adder_anon0 (w: Int) (z: Int) -> Int =
            z + bar + w
          foo y: Int -> Int =
            let w = 1
                adder = foo_adder_anon0 w
            adder y
        |]
    lifted `shouldPassAs` unlifted

  it "lifts lambdas with free variables in let bindings" $ do
    let unlifted = parseLift [here|
          bar: Int = 5
          baz x: Int -> Int =
            x + 1
          foo y: Int -> Int =
            let w = 1
                adder (z: Int) -> Int = z + bar + w
                dec (z: Int) -> Int = z - w
            adder y + dec y
        |]
        lifted = parseLift [here|
          bar: Int = 5
          baz x: Int -> Int =
            x + 1
          foo_adder_anon0 (w: Int) (z: Int) -> Int =
            z + bar + w
          foo_dec_anon1 (w: Int) (z: Int) -> Int =
            z - w
          foo y: Int -> Int =
            let w = 1
                adder = foo_adder_anon0 w
                dec = foo_dec_anon1 w
            adder y + dec y
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
          foo_g_anon0_h_anon1 (a: Int) (x: Int) (b: Int) -> Int =
            a + b + x
          foo_g_anon0 (x: Int) (z: Int) (a: Int) -> Int =
            let h = foo_g_anon0_h_anon1 a x
            h z
          foo (x: Int) (y: Int) -> Int =
            let z = 5
                g = foo_g_anon0 x z
            g y
        |]
    lifted `shouldPassAs` unlifted
