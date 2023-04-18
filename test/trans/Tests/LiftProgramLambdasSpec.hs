{-# LANGUAGE QuasiQuotes #-}
module Tests.LiftProgramLambdasSpec where

import           Sslang.Test

import qualified Front
import qualified IR
import qualified IR.IR                         as I
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.SegmentLets                 ( segmentLets )

import qualified Data.Map                      as M

parseLift :: String -> Pass (I.Program I.Type)
parseLift s =
  Front.run def s
    >>= IR.lower def
    >>= IR.typecheck def
    >>= segmentLets
    >>= liftProgramLambdas
    >>= \p -> return p { I.varNames = M.empty }
    -- we purge varNames before comparison because those don't matter here

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
          foo_adder z: Int -> Int =
            z + 1
          foo y: Int -> Int =
            let adder = foo_adder
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
          foo_adder (w: Int) (z: Int) -> Int =
            z + bar + w
          foo y: Int -> Int =
            let w = 1
                adder = foo_adder w
            adder y
        |]
    unlifted `shouldPassAs` lifted

  it "lifts lambdas with free variables in let bindings" $ do
    pendingWith "typecheck reorders top-level defs"
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
          foo_adder (w: Int) (z: Int) -> Int =
            z + bar + w
          foo_dec (w: Int) (z: Int) -> Int =
            z - w
          foo y: Int -> Int =
            let w = 1
                adder = foo_adder w
                dec = foo_dec w
            adder y + dec y
        |]
    unlifted `shouldPassAs` lifted

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
          foo_g_h (a: Int) (x: Int) (b: Int) -> Int =
            a + b + x
          foo_g (x: Int) (z: Int) (a: Int) -> Int =
            let h = foo_g_h a x
            h z
          foo (x: Int) (y: Int) -> Int =
            let z = 5
                g = foo_g x z
            g y
        |]
    unlifted `shouldPassAs` lifted
