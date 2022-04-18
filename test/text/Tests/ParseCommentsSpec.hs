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
          ]
    mapM_ (shouldPass . parseProgram) inputs
