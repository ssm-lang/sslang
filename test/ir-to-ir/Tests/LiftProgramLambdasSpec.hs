module Tests.LiftProgramLambdasSpec where

import qualified IR.IR                         as I
import qualified IR.Types.Classes              as C

import IR.LambdaLift
import           Common.Pretty                  ( spaghetti )
import           Common.Compiler                ( runPass )
import           Front.Ast
import           Front.Parser                   ( parseProgram )
import           Front.ParseOperators           ( parseOperators )
import           IR.LowerAst                    ( lowerProgram )
import IR.ClassInstantiation (instProgram)
import IR.Types.Poly as Poly
import           IR.TypeInference               ( inferExpr
                                                , inferProgram
                                                )

import           Control.Comonad                ( Comonad(..) )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , pending
                                                , shouldBe
                                                , describe
                                                )

lowerAndLift :: Either String Program -> Either String (I.Program Poly.Type)
lowerAndLift (Left e) = Left "Failed to parse program"
lowerAndLift (Right p) =
  case runPass $ lowerProgram (parseOperators p) >>= inferProgram >>= instProgram >>= liftProgramLambdas of
    Left e' -> Left $ show e'
    Right p' -> Right p'

spec :: Spec
spec = do

  it "lifts a lambda without free variables" $ do
    let nonLiftedProgram = parseProgram $ unlines
          [ "bar: Int = 5"
          , "baz x: Int -> Int ="
          , "  x + 1"
          , "foo y: Int -> Int ="
          , "  let adder (z: Int) -> Int = z + 1"
          , "  adder y"
          ]
        liftedProgram = parseProgram $ unlines
          [ "bar: Int = 5"
          , "baz x: Int -> Int ="
          , "  x + 1"
          , "anon0 z: Int -> Int ="
          , "  z + 1"
          , "foo y: Int -> Int ="
          , "  let adder = anon0"
          , "  adder y"
          ]
        nestedToLifted = lowerAndLift nonLiftedProgram
        liftedToLifted = lowerAndLift liftedProgram
    nestedToLifted `shouldBe` liftedToLifted

  it "lifts a lambda with free variables" $ do
    let nonLiftedProgram = parseProgram $ unlines
          [ "bar: Int = 5"
          , "baz x: Int -> Int ="
          , "  x + 1"
          , "foo y: Int -> Int ="
          , "  let w = 1"
          , "      adder (z: Int) -> Int = z + bar + w"
          , "  adder y"
          ]
        liftedProgram = parseProgram $ unlines
          [ "bar: Int = 5"
          , "baz x: Int -> Int ="
          , "  x + 1"
          , "anon0 (w: Int) (z: Int) -> Int ="
          , "  z + bar + w"
          , "foo y: Int -> Int ="
          , "  let w = 1"
          , "      adder = anon0 w"
          , "  adder y"
          ]
        nestedToLifted = lowerAndLift nonLiftedProgram
        liftedToLifted = lowerAndLift liftedProgram
    nestedToLifted `shouldBe` liftedToLifted

  it "lifts nested lambdas with free variables" $ do
    let nonLiftedProgram = parseProgram $ unlines
          [ "foo (x: Int) (y: Int) -> Int ="
          , "  let z = 5"
          , "      g (a: Int) -> Int ="
          , "        let h (b: Int) -> Int = a + b + x"
          , "        h z"
          , "  g y"
          ]
        liftedProgram = parseProgram $ unlines
          [ "anon0 (a: Int) (x: Int) (b: Int) -> Int ="
          , "  a + b + x"
          , "anon1 (x: Int) (z: Int) (a: Int) -> Int ="
          , "  let h = anon0 a x"
          , "  h z"
          , "foo (x: Int) (y: Int) -> Int ="
          , "  let z = 5"
          , "      g = anon1 x z"
          , "  g y"
          ]
        nestedToLifted = lowerAndLift nonLiftedProgram
        liftedToLifted = lowerAndLift liftedProgram
    nestedToLifted `shouldBe` liftedToLifted
