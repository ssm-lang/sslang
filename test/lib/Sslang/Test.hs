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
import           Common.Identifiers             ( mangleVars )
import           Data.String.SourceCode
import           Test.Hspec

import           Control.DeepSeq                ( deepseq )
import           Control.Exception             as E
                                                ( throwIO )
import           Control.Monad                  ( unless )
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

-- | Expect that some 'Pass' must produce a particular value.
shouldProduce :: (HasCallStack, Show a, Eq a) => Pass a -> a -> Expectation
shouldProduce actual expected = case runPass actual of
  Right (a, _) -> a `shouldBe` expected
  Left  a      -> assertDifference "Expected success but encountered error"
                                   (show a)
                                   (show expected)

-- | Expect that some 'Pass' successfully produces the same value as another
-- modulo alpha renaming.
shouldPassAs
  :: (HasCallStack, Show a, Eq a, Data a) => Pass a -> Pass a -> Expectation
shouldPassAs actual expected = case (runPass actual, runPass expected) of
  (_, Left e) ->
    assertFailure $ "Encountered error when evaluating expectation: " ++ show e
  (Left a, Right e) ->
    assertDifference "Expected success but encountered error" (show a) (show e)
  (Right (a, _), Right (e, _)) ->
    unless (mangleVars a == mangleVars e)
      $ assertDifference "" (show a) (show e)

-- | Expect that some 'Pass' successfully produces the same value as another.
shouldPassAsExactly
  :: (HasCallStack, Show a, Eq a) => Pass a -> Pass a -> Expectation
shouldPassAsExactly actual expected =
  case (runPass actual, runPass expected) of
    (_, Left e) ->
      assertFailure
        $  "Encountered error when evaluating expectation: "
        ++ show e
    (Left a, Right e) -> assertDifference
      "Expected success but encountered error"
      (show a)
      (show e)
    (Right (a, _), Right (e, _)) -> a `shouldBe` e

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
