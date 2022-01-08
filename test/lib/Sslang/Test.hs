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
  , shouldPassExactlyAs
  , shouldNotPassAs
  , shouldProduce
  , shouldFail
  , shouldFailWith
  ) where

import           Common.Compiler
import           Common.Default
import           Common.Identifiers             ( mangleVars )
import           Data.String.SourceCode
import           Test.Hspec

import           Control.DeepSeq                ( deepseq )
import           Control.Exception             as E
                                                ( throwIO )
import           Control.Monad                  ( unless, when )
import           Data.CallStack                 ( SrcLoc
                                                , callStack
                                                )
import           Data.Generics                  ( Data )
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
assertDifference preface actual expected =
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

produce :: (HasCallStack, Show a) => String -> Pass a -> IO a
produce desc actual = case runPass actual of
  Right (a, _) -> return a
  Left a ->
    assertFailure
      $  "Expected success but encountered error when evaluating "
      ++ desc
      ++ ": "
      ++ show a

-- | Expect that some 'Pass' must produce a particular value.
shouldProduce :: (HasCallStack, Show a, Eq a) => Pass a -> a -> Expectation
shouldProduce actual expected = do
  a <- produce "actual case" actual
  a `shouldBe` expected

-- | Expect that some 'Pass' successfully produces the same normalized value
-- as another.
shouldPassAs
  :: (HasCallStack, Show a, Eq a, Data a) => Pass a -> Pass a -> Expectation
shouldPassAs actual expected = do
  e <- produce "expected case" expected
  a <- produce "actual case" actual
  unless (mangleVars a == mangleVars e) $ assertDifference "" (show a) (show e)

-- | Expect that some 'Pass' successfully produces the same value as another.
shouldPassExactlyAs
  :: (HasCallStack, Show a, Eq a) => Pass a -> Pass a -> Expectation
shouldPassExactlyAs actual expected = do
  e <- produce "expected case" expected
  a <- produce "actual case" actual
  a `shouldBe` e

-- | Expect that some 'Pass' successfully produces a different value from
-- another.
shouldNotPassAs
  :: (HasCallStack, Show a, Eq a, Data a) => Pass a -> Pass a -> Expectation
shouldNotPassAs actual unexpected = do
  e <- produce "unexpected case" unexpected
  a <- produce "actual case" actual
  when (e == a) $ assertFailure $ unlines
    ["Expected inequality but found equivalent:", show a, "----", show e]

-- | Expect that some 'Pass' should fail with the given 'Error'.
shouldFail :: (HasCallStack, Show a) => Pass a -> Expectation
shouldFail actual = case runPass actual of
  Right (a, _) ->
    assertFailure $ "Did not encounter expected error, instead got: " ++ show a
  Left _ -> return ()

-- | Expect that some 'Pass' should fail with the given 'Error'.
shouldFailWith :: (HasCallStack, Show a) => Pass a -> Error -> Expectation
shouldFailWith actual expected = case runPass actual of
  Right (a, _) ->
    assertDifference "Did not encounter expected error" (show a) (show expected)
  Left a -> a `shouldBe` expected
