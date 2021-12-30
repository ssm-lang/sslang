{-# LANGUAGE QuasiQuotes #-}
module Tests.TypeInferSpec where

import           Sslang.Test

import qualified Front
import qualified IR
import qualified IR.IR                         as I
import           IR.TypeInference               ( inferProgram )
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
          main(led : &Int) -> () =
            par ((slow: &Int -> ()) (led: &Int): ())
                ((fast: &Int -> ()) (led: &Int): ())
          |]
        minimal = parseInfer [here|
          toggle(led : &Int) -> () =
            led <- 1 - deref led
          slow(led : &Int) -> () =
            let e1 = new ()
            loop
              toggle led
              after 30 , e1 <- ()
              wait e1
          fast(led : &Int) -> () =
            let e2 = new ()
            loop
              toggle led
              after 20 , e2 <- ()
              wait e2
          main(led : &Int) -> () =
            par slow led
                fast led
          |]
    fully `shouldPassAs` minimal
