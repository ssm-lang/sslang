{-# LANGUAGE QuasiQuotes #-}
-- | Sanity check test suite for equivalent and unequivalent programs.
module Tests.NormalizeSpec where

import           Sslang.Test

import           Common.Identifiers             ( mangleVars )
import qualified Front
import qualified IR
import qualified IR.IR                         as I
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.Types.Annotated            as I

parseIR :: String -> Pass (I.Program I.Type)
parseIR s = Front.run def s >>= IR.lower def

spec :: Spec
spec = do
  it "accepts equivalence under data variable renaming" $ do
    let foo0 = parseIR [here|
          foo x y = x + y
          bar z = foo z
        |]
        foo1 = parseIR [here|
          foo y x = y + x
          bar z = foo z
        |]
        foo2 = parseIR [here|
          bar z foo = z + foo
          yy x = bar x
        |]
        foo0' = parseIR [here|
          foo x y = y + x
          bar z = foo z
        |]
        foo1' = parseIR [here|
          foo y x = x + y
          bar z = foo z
        |]
        foo2' = parseIR [here|
          foo x y = x + y
          bar z = bar z
        |]
    foo0 `shouldPassAs` foo1
    foo0 `shouldPassAs` foo2
    foo1 `shouldPassAs` foo2
    fmap mangleVars foo0 `shouldPassButNotAs` fmap mangleVars foo0'
    fmap mangleVars foo0 `shouldPassButNotAs` fmap mangleVars foo1'
    fmap mangleVars foo0 `shouldPassButNotAs` fmap mangleVars foo2'

    let p0 = parseIR [here|
          bar: Int = 5
          baz x: Int -> Int =
            x + 1
          foo y: Int -> Int =
            let adder (z: Int) -> Int = z + bar
            adder y
        |]
        p1 = parseIR [here|
          z: Int = 5
          a b: Int -> Int =
            b + 1
          c d: Int -> Int =
            let e (f: Int) -> Int = f + z
            e d
        |]
    p0 `shouldPassAs` p1

  it "accepts equivalence under type variable renaming" $ do
    let i0 = parseIR [here|
          id a : a -> a = a
        |]
        i1 = parseIR [here|
          id a : b -> b = a
        |]
        i2 = parseIR [here|
          id b : a -> a = b
        |]
        i3 = parseIR [here|
          id b : b -> b = b
        |]
    i0 `shouldPassAs` i1
    i0 `shouldPassAs` i2
    i0 `shouldPassAs` i3
    i1 `shouldPassAs` i2
    i1 `shouldPassAs` i3
    i2 `shouldPassAs` i3

    let const0 = parseIR [here|
          const a b : a -> b -> a = a
        |]
        const1 = parseIR [here|
          const b a : a -> b -> a = b
        |]
        const2 = parseIR [here|
          const b a : b -> a -> b = b
        |]
        const0' = parseIR [here|
          const a b : a -> b -> a = b
        |]
        const1' = parseIR [here|
          const a b : b -> a -> a = a
        |]
    const0 `shouldPassAs` const1
    const0 `shouldPassAs` const2
    fmap mangleVars const0 `shouldPassButNotAs` fmap mangleVars const0'
    fmap mangleVars const0 `shouldPassButNotAs` fmap mangleVars const1'
