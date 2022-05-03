{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Sanity check test suite for equivalent and unequivalent programs.
module Tests.DropInferenceSpec where
import           Control.Concurrent             ( yield )
import           Data.Data                      ( Data
                                                , Proxy(..)
                                                )
import qualified Front
import qualified Front.Ast                     as A
import           Front.ParseOperators           ( parseOperators )
import           Front.Parser                   ( parseProgram )
import qualified IR
import           IR.DropInference               ( insertDropsProgram )
import qualified IR.IR                         as I
import           IR.IR
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.SubstMagic                  ( substMagic )
import           IR.Types.Annotated            as I
import           IR.Types.Poly                 as Poly
import           Sslang.Test

parseWithoutPatDesugar :: String -> Pass A.Program
parseWithoutPatDesugar src = do
  ast  <- parseProgram src
  astP <- parseOperators ast
  Front.checkAst def astP
  return astP

parseFront :: String -> Pass (I.Program I.Type)
parseFront s = substMagic (Proxy :: Proxy I.Type) <$> do
  parseWithoutPatDesugar s >>= IR.lower def

parseDrop :: String -> Pass (IR.IR.Program Poly.Type)
parseDrop s =
  parseFront s >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram

parseNoDrop :: String -> Pass (IR.IR.Program Poly.Type)
parseNoDrop s = parseFront s >>= IR.ann2Class def >>= IR.class2Poly def

spec :: Spec
spec = do
  it "drop inference in simple let binding" $ do
    let undropped = parseDrop [here|
        top = 
            let a: Int = 5
                b: Int = 10
                _: Int = 15
            a + b
        |]
    let dropped = parseNoDrop [here|
        top =
            let a: Int = 5
                b: Int = 10
                anon3_let_underscore: Int = 15
            let _ = dup a
            let _ = dup b
            let _ = dup anon3_let_underscore
            let anon0_let = a + b
            let _ = drop a
            let _ = drop b
            let _ = drop anon3_let_underscore
            anon0_let
        |]
    undropped `shouldPassAs` dropped

  it "drop inference in nested let binding" $ do
    let undropped = parseDrop [here|
        top = 
            let a = 5
                b = let c = a + 3
                    c + 5
            a + b
        |]
    let dropped = parseNoDrop [here|
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
    undropped `shouldPassAs` dropped

  it "drop inference in lambda" $ do
    let undropped = parseDrop [here|
        add_fun a : Int -> Int = a + 2  
        |]
    let dropped = parseNoDrop [here|
        add_fun a : Int -> Int = 
          let _ = dup a
          let anon0_lambda = a + 2
          let _ = drop a
          anon0_lambda
        |]
    undropped `shouldPassAs` dropped

  it "drop inference in function application" $ do
    let undropped = parseDrop [here|
        x : Int = 5
        id_fun a : Int -> Int = a
        top = id_fun x + 1
        |]
    let dropped = parseNoDrop [here|
        x : Int = 5
        id_fun a : Int -> Int = 
          let _ = dup a
          let anon0_lambda = a
          let _ = drop a
          anon0_lambda
        top = id_fun x + 1
        |]
    undropped `shouldPassAs` dropped

  it "drop inference in pattern matching" $ do
    let undropped = parseDrop [here|
        type MyADT
          MyDCon1 Int Int
          MyDCon2 Int
          MyDCon3 MyADT

        x = MyDCon2 30
        top = match x
                MyDCon2 fst = fst
                MyDCon1 fst snd = snd
                _ = 69
        |]
    let dropped = parseNoDrop [here|
        type MyADT
          MyDCon1 Int Int
          MyDCon2 Int
          MyDCon3 MyADT

        x = MyDCon2 30
        top = match x
                MyDCon2 fst =
                  let _ = dup fst
                  let anon0_alt_data = fst
                  let _ = drop fst
                  anon0_alt_data
                MyDCon1 fst snd =
                  let _ = dup fst
                  let _ = dup snd
                  let anon2_alt_data = snd
                  let _ = drop fst
                  let _ = drop snd
                  anon2_alt_data
                _ =
                  let _ = dup x
                  let anon4_alt_default = 69
                  let _ = drop x
                  anon4_alt_default
        |]
    undropped `shouldPassAs` dropped
    -- print $ runPass undropped
