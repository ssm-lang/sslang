{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ParseInlineCSpec where

import           Sslang.Test

import           Front.Ast
import           Front.Parser                   ( parseProgram )

shouldParse :: HasCallStack => String -> Expectation
shouldParse = shouldPass . parseProgram

shouldntParse :: HasCallStack => String -> Expectation
shouldntParse = (`shouldFailWith` ParseError "parse error") . parseProgram

spec :: Spec
spec = do
  it "parses inline C blocks" $ do
    shouldParse [here|
      $$
      #include <stdio.h>

      void hello() { printf("Hello world!\n"); }
      $$

      main _ _ =
        $hello ()
    |]
    shouldParse [here|
      $$

      #include <stdio.h>

      void printInt(ssm_value_t v) { printf("%d\n", ssm_unmarshal(v)); }

      $$

      main _ _ =
        $printInt(42)
    |]

  it "parses multiple inline C blocks" $ do
    shouldParse [here|
      $$

      #include <stdio.h>

      $$

      $$

      void printInt(ssm_value_t v) {
        printf("%d\n", ssm_unmarshal(v));
      }
      $$

      main _ _ =
        $printInt(42)
    |]
    shouldParse [here|
      $$
      #include <stdio.h>
      $$

      main _ _ =
        $printInt(42)

      $$

      void printInt(ssm_value_t v) {
        printf("%d\n", ssm_unmarshal(v));
      }
      $$

    |]
  it "is robust against C syntax errors in C blocks" $ do
    shouldParse [here|
      $$
      #include <stdio.h
      void printInt(ssm_value_t v) { printf("%d\n", ssm_unmarshal(v)); }
      $$

      main _ _ =
        $printInt(42)
    |]

    shouldParse [here|
      $$
      #include <stdio.h>
      void printInt(ssm_value_t v) { printf("%d\n", ssm_unmarshal(v)); }}}
      $$

      main _ _ =
        $printInt(42)
    |]

    shouldParse [here|
      $$
      ### hello
      $$$$$ hello
      123 123
      " this is some argi
      aribtrrary text test }} ))(()
      $$

      main _ _ =
        $printInt(42)
    |]
