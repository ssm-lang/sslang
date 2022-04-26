module Main where

import           Common.Identifiers
import           Data.List
import           IR.IR
import Prelude (putStrLn)

{-
How to run:
in src, run `stack ghci`
then do ``:l unit-tests/IRTest.hs`
then do ``:main`

-}
tests =
  [ (show New                                , "new")
  , (show (Var (VarId (Identifier "foo")) ()), "(var foo () )")
  ]

runTest :: (String, String) -> Bool
runTest (a, b) = b == a

runTests :: [(String, String)] -> [Bool]
runTests = map runTest

getOutputMessage :: [Int] -> String
getOutputMessage [] = "IR Test - all passed"
getOutputMessage x  = "IR Test - failed tests: " ++ show x

main :: IO ()
main = do
  putStrLn $ getOutputMessage (elemIndices False (runTests tests))
