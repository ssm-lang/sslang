{-# LANGUAGE QuasiQuotes #-}
module Tests.ScopingSpec where

import           Sslang.Test

import qualified Front
import           Front.Scope                    ( scopeProgram )

-- import           Common.Compiler                ( Error(..)
--                                                 , runPass
--                                                 )
-- import           Common.Default                 ( Default(..) )
-- import           Test.Hspec                     ( Spec(..)
--                                                 , describe
--                                                 , it
--                                                 , pending
--                                                 , shouldBe
--                                                 )

doScope :: String -> Pass ()
doScope s = Front.parseAst def s >>= scopeProgram

spec :: Spec
spec = do
  it "supports global scoping" $ do
    shouldPass $ doScope [here|
      x = 3
      y = x
    |]
  it "supports let-bound variables" $ do
    shouldPass $ doScope [here|
      x =
        let y = 3
        x + y
    |]
  it "supports arguments" $ do
    shouldPass $ doScope [here|
      f x = x
    |]
  it "supports nested arguments" $ do
    shouldPass $ doScope [here|
      f x =
        let g y = x
        g x
    |]
  it "supports pattern-matching" $ do
    shouldPass $ doScope [here|
      foo x =
        match x
          0 = 1
          a = x + a
    |]
  it "supports wild-cards" $ do
    shouldPass $ doScope [here|
      foo _ x =
        match x
          0 = 2
          _ = x
    |]
  it "supports local and global scoping" $ do
    shouldPass $ doScope [here|
      foo x =
        let foo' y = x
        foo'
      bar a b =
        foo b a
    |]
  it "supports enum matches" $ do
    pendingWith "syntax for defining data constructors"
    shouldPass $ doScope [here|
      foo D = 3
    |]
  it "supports data constructor matches" $ do
    pendingWith "syntax for defining data constructors"
    shouldPass $ doScope [here|
      foo (D x) = 3
    |]
  it "supports nested data constructors" $ do
    pendingWith "syntax for defining data constructors"
    shouldPass $ doScope [here|
      foo (D (D 3)) = 3
    |]
