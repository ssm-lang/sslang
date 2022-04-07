{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Sanity check test suite for equivalent and unequivalent programs.
module Tests.DropInferenceSpec where
import           Sslang.Test
import           Data.Data                      ( Data
                                                , Proxy(..)
                                                )
import qualified Front
import           Front.Ast
import qualified IR
import           IR.DropInference               ( insertDropsProgram )
import qualified IR.IR                         as I
import           IR.IR
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.SubstMagic                  ( substMagic )
import           IR.Types.Annotated            as I
import           IR.Types.Poly                 as Poly
import Control.Concurrent (yield)


parseDrop :: String -> Pass (I.Program I.Type)
parseDrop s = substMagic (Proxy :: Proxy I.Type) <$> do
  Front.run def s >>= IR.lower def
  -- >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram


spec :: Spec
spec = do
  it "drop inference in simple let binding" $ do
    let undropped =
          parseDrop [here|
        top = 
            let a: Int = 5
                b: Int = 10
            a + b
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
            >>= insertDropsProgram
    let dropped =
          parseDrop [here|
        top =
            let a: Int = 5
                b: Int = 10
            let _ = dup a
            let _ = dup b
            let anon0_let = a + b
            let _ = drop a
            let _ = drop b
            anon0_let
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
    undropped `shouldPassAs` dropped

  it "drop inference in nested let binding" $ do
    let undropped =
          parseDrop [here|
        top = 
            let a = 5
                b = let c = a + 3
                    c + 5
            a + b
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
            >>= insertDropsProgram
    let dropped =
          parseDrop [here|
        top =
            let a = 5
                b = let c = a + 3
                    let _ = dup c
                    let anon0_let = c + 5
                    let _ = drop c
                    anon0_let
            let _ = dup a
            let _ = dup b
            let anon1_let = a + b
            let _ = drop a
            let _ = drop b
            anon1_let
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
    undropped `shouldPassAs` dropped

  it "drop inference in lambda" $ do
    let undropped =
          parseDrop [here|
        add_fun a : Int -> Int = a + 2  
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
            >>= insertDropsProgram
    let dropped =
          parseDrop [here|
        add_fun a : Int -> Int = 
          let _ = dup a
          let anon0_lambda = a + 2
          let _ = drop a
          anon0_lambda
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
    undropped `shouldPassAs` dropped

  it "drop inference in function application" $ do
    let undropped =
          parseDrop [here|
        x : Int = 5
        id_fun a : Int -> Int = a
        top = id_fun x
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
            >>= insertDropsProgram
    let dropped =
          parseDrop [here|
        x : Int = 5
        id_fun a : Int -> Int = 
          let _ = dup a
          let anon0_lambda = a
          let _ = drop a
          anon0_lambda
        top = let anon1_app = let _ = dup x
                              id_fun x
              let _ = drop x
              anon1_app
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
    undropped `shouldPassAs` dropped

  it "drop inference in pattern matching" $ do
    let undropped = parseDrop [here|
        type MyBool 
          MyFalse Int Int
          MyTrue Int

        x = MyTrue 30
        top = match x
                MyTrue fst = fst
                MyFalse fst snd = snd
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
            >>= insertDropsProgram
    let dropped =
          parseDrop [here|
        type MyBool 
          MyFalse Int Int
          MyTrue Int

        x = MyTrue 30
        top = match x
                MyTrue fst = 
                  let _ = dup fst
                  let anon0_alt_data = fst
                  let _ = drop fst
                  anon0_alt_data
                MyFalse fst snd = 
                  let _ = dup fst
                  let _ = dup snd
                  let anon1_alt_data = snd
                  let _ = drop fst
                  let _ = drop snd
                  anon1_alt_data
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
    undropped `shouldPassAs` dropped
    -- print $ runPass undropped