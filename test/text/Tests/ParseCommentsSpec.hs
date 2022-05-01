{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ParseCommentsSpec where

import           Sslang.Test

import           Front.Ast
import           Front.Parser                   ( parseProgram )

p :: HasCallStack => String -> Expectation
p = shouldPass . parseProgram

spec :: Spec
spec = do
  it "parses a program with leading comments or spaces" $ do
    p [here|
        // a comment here
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
      |]
    p [here|

        // a comment here
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
      |]
    p [here|
        // a comment here

        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
      |]
    p [here|

        // a comment here

        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
      |]
    p [here|


        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
      |]
    p [here|

        /* hello */

        /* this is /* a nested */ comment */

        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
      |]

  it "parses a program with trailing comments or spaces" $ do
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
        // a comment here
      |]
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)

        // a comment here
      |]
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)
        // a comment here

      |]
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)

        // a comment here

      |]
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)


      |]
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          wait (cout : & Int)

        /* hello */

        /* this is /* a nested */ comment */

      |]
    p "main cout = \n wait cout"

  it "parses a program with confusing comment indentation" $ do
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
        // a comment here
          wait (cout : & Int)
      |]
    p [here|
        main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
          // a comment here
          wait (cout : & Int)
      |]
    p [here|
        main ( cout : &Int ) -> () =
          after 10 , (cout : & Int) <- 96
            // a comment here
          wait (cout : & Int)
      |]
    p [here|
        /* this block comment bleeds
        into the program */main ( cout : & Int ) -> () =
          after 10 , (cout : & Int) <- 96
            // a comment here
          wait (cout : & Int)
        f x = x
      |]
    p [here|
         /*{*/main/*s*/(/*p*/cout/*l*/:/*i*/& /*c*/Int/*e*/)/*d*/-> /* */()/*!*/= /*}*/
           after 10 , (cout : & Int) <- 96
           wait (cout : & Int)
       |]
    p [here|
        main ( cout : & Int ) -> () =
        /* using comments to */  after 10 , (cout : & Int) <- 96
                /* is ok */
                                 wait cout /* even */
          /* leading */          wait (cout : & Int)  /* comments */
        f x = x
      |]
    p [here|
        main ( cout : & Int ) -> () = /* similar */  after 10 , (cout : & Int) <- 96
           /* idea */
                                                     wait cout /* with blocks */
              /* started on */                       wait (cout : & Int) /* the same line */
        f x = x
      |]
