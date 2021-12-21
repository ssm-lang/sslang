{- | Defines some common utilities for writing Hspec tests.

TODO: display the pretty-printed instance of items, for better test output.
-}
module Sslang.Test
  ( module Common.Compiler
  , module Common.Default
  , module Data.String.SourceCode
  , module Test.Hspec
  , shouldPass
  , shouldPassAs
  , shouldProduce
  , shouldFail
  , shouldFailWith
  ) where

import           Common.Compiler
import           Common.Default
import           Data.String.SourceCode
import           Test.Hspec

import           Control.DeepSeq                ( deepseq )
import           Control.Exception             as E
                                                ( throwIO )
import           Data.CallStack                 ( SrcLoc
                                                , callStack
                                                )
import           Test.HUnit.Lang                ( FailureReason(..)
                                                , HUnitFailure(..)
                                                , assertFailure
                                                )

-- | Produce Haskell code location from callstack.
location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  []           -> Nothing

-- | Signals that two items are different.
--
-- Based on Test.Hspec.assertEqual, without the 'Eq' and 'Show' instances.
assertDifference :: HasCallStack => String -> String -> String -> Expectation
assertDifference preface expected actual =
  preface `deepseq` expected `deepseq` actual `deepseq` E.throwIO
    (HUnitFailure location $ ExpectedButGot prefaceMsg expected actual)
 where
  prefaceMsg | null preface = Nothing
             | otherwise    = Just preface

-- | Expect that some 'Pass' must succeed without failure.
shouldPass :: (HasCallStack, Show a, Eq a) => Pass a -> Expectation
shouldPass a = case runPass a of
  Right _ -> return ()
  Left  e -> assertFailure $ "Encountered compiler error: " ++ show e

-- | Expect that some 'Pass' must produce a particular value.
shouldProduce :: (HasCallStack, Show a, Eq a) => Pass a -> a -> Expectation
shouldProduce actual expected = case runPass actual of
  Right a -> a `shouldBe` expected
  Left  a -> assertDifference "Expected success but encountered error"
                              (show a)
                              (show expected)

-- | Expect that some 'Pass' must produce some expected value.
shouldPassAs :: (HasCallStack, Show a, Eq a) => Pass a -> Pass a -> Expectation
shouldPassAs actual expected = case (runPass actual, runPass expected) of
  (_, Left e) ->
    assertFailure $ "Encountered error when evaluating expectation: " ++ show e
  (Left a, Right e) ->
    assertDifference "Expected success but encountered error" (show a) (show e)
  (Right a, Right e) -> a `shouldBe` e

-- | Expect that some 'Pass' should fail with the given 'Error'.
shouldFail :: (HasCallStack, Show a) => Pass a -> Expectation
shouldFail actual = case runPass actual of
  Right a ->
    assertDifference "Did not encounter expected error" (show a) "Some error"
  Left _ -> return ()

-- | Expect that some 'Pass' should fail with the given 'Error'.
shouldFailWith :: (HasCallStack, Show a) => Pass a -> Error -> Expectation
shouldFailWith actual expected = case runPass actual of
  Right a ->
    assertDifference "Did not encounter expected error" (show a) (show expected)
  Left a -> a `shouldBe` expected
