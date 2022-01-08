{-# LANGUAGE QuasiQuotes #-}
module Tests.TypeInferSpec where

import           Sslang.Test

import qualified Front
import qualified IR
import qualified IR.IR                         as I
import           IR.HM                         ( inferProgram )
import qualified IR.Types.Classes              as Cls

parseInfer :: String -> Pass (I.Program Cls.Type)
parseInfer s = Front.run def s >>= IR.lower def >>= inferProgram

spec :: Spec
spec = do
  it "infers programs with minimal type annotations" $ do
    let fully = parseInfer [here|
          toggle(led : &Int) -> () =
            (led: &Int) <- (1 - deref (led: &Int): Int)
          slow(led : &Int) -> () =
            let e1 = (new () : &())
            loop
              ((toggle: &Int -> ()) (led: &Int): ())
              after 30 , (e1: &()) <- ()
              wait (e1: &())
          fast(led : &Int) -> () =
            let e2 = (new () : &())
            loop
              ((toggle: &Int -> ()) (led: &Int): ())
              after 20 , (e2: &()) <- ()
              wait (e2: &())
          main(led : &Int) -> ((), ()) =
            par ((slow: &Int -> ()) (led: &Int): ())
                ((fast: &Int -> ()) (led: &Int): ())
          |]
        minimal = parseInfer [here|
          toggle(led) =
            led <- 1 - deref led
          slow(led) =
            let e1 = new ()
            loop
              toggle led
              after 30 , e1 <- ()
              wait e1
          fast(led) =
            let e2 = new ()
            loop
              toggle led
              after 20 , e2 <- ()
              wait e2
          main(led) =
            par slow led
                fast led
          |]
    fully `shouldPassAs` minimal

  it "inferred programs takes type annotations into account" $ do
    let fully = parseInfer [here|
          id(x : Int) =
            x : Int
          |]
        partial1 = parseInfer [here|
          id(x : Int) =
            x
          |]
        partial2 = parseInfer [here|
          id(x) =
            x : Int
          |]
        none = parseInfer [here|
          id(x) =
            x
          |]
    fully `shouldPassAs` partial1
    fully `shouldPassAs` partial2
    fully `shouldPassButNotAs` none

  it "expressions with incompatible type annotations do not type check" $ do
    let bad1 = parseInfer [here|
          id(x : Int) =
            (x : &Int)
          |]
        bad2 = parseInfer [here|
          id(x : &Int) =
            x+1-1
          |]
        good1 = parseInfer [here|
          id(x : Int) =
            x+1-1
          |]
        good2 = parseInfer [here|
          id(x) =
            x+1-1
          |]
    shouldFail bad1
    shouldFail bad2
    good1 `shouldPassAs` good2

  it "type inference can correctly handle some tricky cases" $ do
    let tricky1a = parseInfer [here|
          f(x : Int) =
            let y = x
            y
          |]
        tricky1b = parseInfer [here|
          f(x : Int) -> Int =
            let y = x
            y
          |]
    tricky1a `shouldPassAs` tricky1b
    let tricky2a = parseInfer [here|
          f(x : Int) =
            let g (z : ()) = x
            g
          |]
        tricky2b = parseInfer [here|
          f(x : Int) -> (() -> Int) =
            let g (z : ()) = x
            g
          |]
    tricky2a `shouldPassAs` tricky2b
    let tricky3a = parseInfer [here|
          f(x : Int) =
            let g (x : ()) = x
            g
          |]
        tricky3b = parseInfer [here|
          f(x : Int) -> (() -> ()) =
            let g (x : ()) = x
            g
          |]
    tricky3a `shouldPassAs` tricky3b

  {- More test functions to verify the correctness of type inference.

  For now, this is broken. Also, I hardcoded all the expected results. We should fix that later.
  Uncomment the print statement to output the inference result for each function.


  it "infers id function" $ do
    let minimal = parseProgram $ unlines
          [ "id(x) ="
          , "  x"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "TBuiltin (Arrow (TVar t0) (TVar t0))"

  it "infers addOne function v1" $ do
    let minimal = parseProgram $ unlines
          [ "addOne(x) ="
          , "  x + 1"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "TBuiltin (Arrow (TBuiltin (Integral 32)) (TBuiltin (Integral 32)))"

  it "infers addOne function v2" $ do
    let minimal = parseProgram $ unlines
          [ "id(x) ="
          , "  x"
          , "addOne(x) ="
          , "  id(x) + 1"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "TBuiltin (Arrow (TBuiltin (Integral 32)) (TBuiltin (Integral 32)))"

  it "infers tricky program #1:`f` should has type `a -> a`" $ do
    let minimal = parseProgram $ unlines
          [ "f x ="
          , "  let y = x"
          , "  y"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "TBuiltin (Arrow (TVar t0) (TVar t0))"

  it "infers tricky program #2:`f` should has type `a -> (b -> a)`" $ do
    let minimal = parseProgram $ unlines
          [ "f x ="
          , "  let g z = x"
          , "  g"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "Var x (TVar t0)"
    typedPString `shouldContain` "Var g (TBuiltin (Arrow (TVar t2) (TVar t0)))"
    typedPString `shouldContain` "TBuiltin (Arrow (TVar t0) (TBuiltin (Arrow (TVar t2) (TVar t0))))"

  it "infers tricky program #3:`f` should has type `a -> (b -> b)` not `a -> (a -> a)`" $ do
    let minimal = parseProgram $ unlines
          [ "f x ="
          , "  let g x y = x"
          , "  g x"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "Var g (TBuiltin (Arrow (TVar t4) (TVar t4)))"
    typedPString `shouldContain` "TBuiltin (Arrow (TVar t0) (TBuiltin (Arrow (TVar t4) (TVar t4))))"

  it "infers tricky program #4: `f` should has type `a -> a`" $ do
    let minimal = parseProgram $ unlines
          [ "f x ="
          , "  let g y = x"
          , "  g g"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "Var g (TBuiltin (Arrow (TVar t4) (TVar t0)))"
    typedPString `shouldContain` "TBuiltin (Arrow (TVar t0) (TVar t0))"

  it "infers tricky program #5: `f` should has type `a -> a`" $ do
    let minimal = parseProgram $ unlines
          [ "f ="
          , "  let g x = x"
          , "  g g"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "Var g (TBuiltin (Arrow (TVar t3) (TVar t3)))"
    typedPString `shouldContain` "TBuiltin (Arrow (TVar t3) (TVar t3))"

  it "infers tricky program #6: `f` should has type `(a -> b) -> (a -> b)` not `(a -> b) -> (c -> d)`" $ do
    let minimal = parseProgram $ unlines
          [ "f x ="
          , "  let g z = x z"
          , "  g"
          ]
        typedMinimal = lowerAndInfer minimal
        typedPString = show typedMinimal
    -- print typedMinimal
    typedPString `shouldContain` "Var g (TBuiltin (Arrow (TVar t1) (TVar t2)))"
    typedPString `shouldContain` "TBuiltin (Arrow (TBuiltin (Arrow (TVar t1) (TVar t2))) (TBuiltin (Arrow (TVar t1) (TVar t2))))"
  -}
