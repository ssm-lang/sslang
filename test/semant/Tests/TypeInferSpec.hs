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
    let int2int = parseInfer [here|
          id(x : Int) =
            x : Int
          |]
        int2unknow = parseInfer [here|
          id(x : Int) =
            x
          |]
        unknow2int = parseInfer [here|
          id(x) =
            x : Int
          |]
        none = parseInfer [here|
          id(x) =
            x
          |]
    int2int `shouldPassAs` int2unknow
    int2int `shouldPassAs` unknow2int
    int2int `shouldPassButNotAs` none

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

  it "type annotations can contains type variable" $ do
    let a2a = parseInfer [here|
          id(x : a) =
            x
          |]
        b2b = parseInfer [here|
          id(x : b) =
            x
          |]
        none = parseInfer [here|
          id(x) =
            x
          |]
        int2a = parseInfer [here|
          id(x : Int) =
            x : a
          |]
        int2int = parseInfer [here|
          id(x : Int) =
            x
          |]
    a2a `shouldPassAs` b2b
    a2a `shouldPassAs` none

  it "throws error when annotated type is more general than infered type" $ do
    pendingWith "check type generality"
    let int2a = parseInfer [here|
          addOne(x : a) =
            x + 1
          |]
    shouldFail int2a

  it "type inference can correctly handle some tricky cases" $ do
    let tricky1a = parseInfer [here|
          f x =
            let y = x
            y
          |]
        tricky1b = parseInfer [here|
          f(x : a) -> a =
            let y = x
            y
          |]
    tricky1a `shouldPassAs` tricky1b
    let tricky2a = parseInfer [here|
          f x =
            let g y = x
            g
          |]
        tricky2b = parseInfer [here|
          f(x : a) -> (b -> a) =
            let g y = x
            g
          |]
    tricky2a `shouldPassAs` tricky2b
    let tricky3a = parseInfer [here|
          f x =
            let g x = x
            g
          |]
        tricky3b = parseInfer [here|
          f(x : a) -> (b -> b) =
            let g x = x
            g
          |]
    tricky3a `shouldPassAs` tricky3b
    let tricky4a = parseInfer [here|
          f x =
            let g x y = x
            g x
          |]
        tricky4b = parseInfer [here|
          f (x : a) -> (b -> a) =
            let g x y = x
            g x
          |]
    tricky4a `shouldPassAs` tricky4b
    let tricky5a = parseInfer [here|
          f x =
            let g y x = x
            g x
          |]
        tricky5b = parseInfer [here|
          f (x : a) -> (b -> b) =
            let g y x = x
            g x
          |]
    tricky5a `shouldPassAs` tricky5b
    let tricky6a = parseInfer [here|
          f x =
            let g y = x
            g g
          |]
        tricky6b = parseInfer [here|
          f (x : a) -> a =
            let g y = x
            g g
          |]
    tricky6a `shouldPassAs` tricky6b
    let tricky7a = parseInfer [here|
          f x =
            let g x = x
            g g
          |]
        tricky7b = parseInfer [here|
          f (x : a) -> (b -> b) =
            let g x = x
            g g
          |]
    tricky7a `shouldPassAs` tricky7b
    let tricky8a = parseInfer [here|
          f x =
            let g y = x y
            g
          |]
        tricky8b = parseInfer [here|
          f (x : (a -> b)) -> (a -> b) =
            let g y = x y
            g
          |]
    tricky8a `shouldPassAs` tricky8b
