module Tests.InferProgramSpec where

import qualified IR.IR                         as I

import           Common.Pretty                  ( spaghetti )
import           Common.Compiler                ( runPass )
import           Front.Ast
import           Front.Parser                   ( parseProgram )
import           Front.ParseOperators           ( parseOperators )
import           IR.LowerAst                    ( lowerProgram )
import           IR.TypeInference               ( emptyCtx
                                                , inferExpr
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

  it "inferExpr doubleblink.ssl part 1" $ do
    let input = parseProgram $ unlines
          [ "toggle(led : &Int) -> () ="
          , "  (led: &Int) <- (deref (led: &Int): Int)"
          ]
        typedInput = case input of
          Left e -> Left "Failed to parse program"
          Right p ->
            case runPass $ lowerProgram (parseOperators p) of
              Left e' -> Left "Failed to lower program"
              Right p' -> Right $ second (inferExpr emptyCtx) <$> (I.programDefs p')
    putStrLn $ show typedInput

  it "inferExpr doubleblink.ssl part 2" $ do
    let input = parseProgram $ unlines
          [ "slow(led : &Int) -> () ="
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
            case runPass $ lowerProgram (parseOperators p) of
              Left e' -> Left "Failed to lower program"
              Right p' -> Right $ second (inferExpr emptyCtx) <$> (I.programDefs p')
    putStrLn $ show typedInput
