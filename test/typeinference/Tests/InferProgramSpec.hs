module Tests.InferProgramSpec where

import qualified Data.Map                      as M
import qualified IR.IR                         as I
import qualified IR.Types.Classes              as C
import qualified IR.Types.Annotated            as A

import           Common.Pretty                  ( spaghetti )
import           Common.Compiler                ( runPass )
import           Front.Ast
import           Front.Parser                   ( parseProgram )
import           Front.ParseOperators           ( parseOperators )
import           IR.LowerAst                    ( lowerProgram )
import           IR.HM                          ( inferProgram
                                                )

import           Control.Comonad                ( Comonad(..) )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , pending
                                                , shouldBe
                                                , shouldContain
                                                , describe
                                                )

renderAndParse :: Program -> Either String Program
renderAndParse = parseProgram . spaghetti

lowerAndInfer :: Either String Program -> Either String (I.Program C.Type)
lowerAndInfer (Left e) = Left "Failed to parse program"
lowerAndInfer (Right p) =
  case runPass $ lowerProgram (parseOperators p) >>= inferProgram of
    Left e' -> Left $ show e'
    Right p' -> Right p'

-- | Helper function used to inspect the IR before type inference.
lower :: Either String Program -> Either String (I.Program A.Type)
lower (Left e) = Left "Failed to parse program"
lower (Right p) =
  case runPass $ lowerProgram (parseOperators p) of
    Left e' -> Left $ show e'
    Right p' -> Right p'

spec :: Spec
spec = do

  it "infers programs with minimal type annotations" $ do
    let fully = parseProgram $ unlines
          [ "toggle(led : &Int) -> () ="
          , "  (led: &Int) <- (1 - deref (led: &Int): Int)"
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
        minimal = parseProgram $ unlines
          [ "toggle(led : &Int) -> () ="
          , "  led <- 1 - deref led"
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
        typedFully = lowerAndInfer fully
        typedMinimal = lowerAndInfer minimal
    typedFully `shouldBe` typedMinimal

  {- A list of tricky functions to verify the correctness of type inference.

  For now, I hardcode all the expected results. We should fix that later.
  Uncomment the print statement to output the inference result for each function.
  -}
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
          , "  let g x = x"
          , "  g g"
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
