module Tests.InferProgramSpec where

import qualified IR.IR                         as I

import           Common.Pretty                  ( spaghetti )
import           Common.Compiler                ( runPass )
import           Front.Ast
import           Front.Parser                   ( parseProgram )
import           Front.ParseOperators           ( parseOperators )
import           IR.LowerAst                    ( lowerProgram )
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

renderAndParse :: Program -> Either String Program
renderAndParse = parseProgram . spaghetti

spec :: Spec
spec = do

  it "infers doubleblink.ssl" $ do
    let input = parseProgram $ unlines
          [ "toggle(led : &Int) -> () ="
          , "  (led: &Int) <- (deref (led: &Int): Int)"
          , "slow(led : &Int) -> () ="
          , "  let e1 = (new () : &())"
          , "  loop"
          , "    ((toggle: &Int -> ()) (led: &Int): ())"
          , "    after 30 , (e1: &()) <- ()"
          , "    wait (e1: &())"
          , "fast(led : &Int) -> () ="
          , "  let e2 = (new () : &())"
          , "  loop"
          , "    ((toggle: &Int -> ()) (led: &Int): ())"
          , "    after 20 , (e2: &()) <- ()"
          , "    wait (e2: &())"
          , "main(led : &Int) -> () ="
          , "  par ((slow: &Int -> ()) (led: &Int): ())"
          , "      ((fast: &Int -> ()) (led: &Int): ())"
          ]
        typedInput = case input of
          Left e -> Left "Failed to parse program"
          Right p ->
            case runPass $ lowerProgram (parseOperators p) >>= inferProgram of
              Left e' -> Left "Failed to lower program"
              Right p' -> Right p'
    print typedInput

  it "infers doubleblink2.ssl" $ do
    let input = parseProgram $ unlines
          [ "toggle(led : &Int) -> () ="
          , "  led <- (1 - deref led)"
          , "slow(led : &Int) -> () ="
          , "  let e1 = new ()"
          , "  loop"
          , "    toggle led"
          , "    after 30 , e1 <- ()"
          , "    wait e1"
          , "fast(led : &Int) -> () ="
          , "  let e2 = new ()"
          , "  loop"
          , "    toggle led"
          , "    after 20 , e2 <- ()"
          , "    wait e2"
          , "main(led : &Int) -> () ="
          , "  par slow led"
          , "      fast led"
          ]
        typedInput = case input of
          Left e -> Left "Failed to parse program"
          Right p ->
            case runPass $ lowerProgram (parseOperators p) >>= inferProgram of
              Left e' -> Left "Failed to lower program"
              Right p' -> Right p'
    print typedInput
