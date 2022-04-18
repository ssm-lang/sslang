{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.ParseCommentsSpec where

import           Sslang.Test

import           Front.Ast
import           Front.Parser                   ( parseProgram )

spec :: Spec
spec = do
  it "parses a program with leading comments or spaces" $ do
    let inputs =
          [ [here|
              // a comment here
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
            |]
          , [here|

              // a comment here
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
            |]
          , [here|
              // a comment here

              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
            |]
          , [here|

              // a comment here

              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
            |]
          , [here|


              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
            |]
          , [here|

              /* hello */

              /* this is /* a nested */ comment */

              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
            |]
          ]
    mapM_ (shouldPass . parseProgram) inputs

  it "parses a program with trailing comments or spaces" $ do
    let inputs =
          [ [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
              // a comment here
            |]
          , [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)

              // a comment here
            |]
          , [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)
              // a comment here

            |]
          , [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)

              // a comment here

            |]
          , [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)


            |]
          , [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                wait (cout : & Int)

              /* hello */

              /* this is /* a nested */ comment */

            |]
          , "main cout = \n wait cout"
          ]
    mapM_ (shouldPass . parseProgram) inputs

  it "parses a program with confusing comment indentation" $ do
    let inputs =
          [ [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
              // a comment here
                wait (cout : & Int)
            |]
          , [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                // a comment here
                wait (cout : & Int)
            |]
          , [here|
              main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                  // a comment here
                wait (cout : & Int)
            |]
          , [here|
              /* this block comment bleeds
              into the program */main ( cout : & Int ) -> () =
                after 10 , (cout : & Int) <- 96
                  // a comment here
                wait (cout : & Int)
              f x = x
            |]
          -- , [here|
          --     /*{*/main/*s*/(/*p*/cout/*l*/:/*i*/&/*c*/Int/*e*/)/*d*/->/* */()/*!*/=/*}*/
          --       after 10 , (cout : & Int) <- 96
          --       wait (cout : & Int)
          --   |]
          , [here|
              main ( cout : & Int ) -> () =
              /* using comments to */  after 10 , (cout : & Int) <- 96
                      /* is ok */
                                       wait cout /* even */
                /* leading */          wait (cout : & Int)  /* comments */
              f x = x
            |]
          , [here|
              main ( cout : & Int ) -> () = /* similar */  after 10 , (cout : & Int) <- 96
                 /* idea */
                                                           wait cout /* with blocks */
                    /* started on */                       wait (cout : & Int) /* the same line */
              f x = x
            |]
          ]
    mapM_ (shouldPass . parseProgram) inputs
