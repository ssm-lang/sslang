{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.ScopingSpec where

import           Sslang.Test

import qualified Front
import           Front.Scope                    ( scopeProgram )

doScope :: String -> Pass ()
doScope s = Front.parseAst def s >>= scopeProgram

spec :: Spec
spec = do
  describe "scoping global definitions" $ do
    it "supports global scoping" $ do
      shouldPass $ doScope [here|
        x = 3
        y = x
      |]
    it "supports global function arguments" $ do
      shouldPass $ doScope [here|
        f x = x
      |]
    it "supports local and global scoping" $ do
      shouldPass $ doScope [here|
        foo x =
          let foo' y = x
          foo'
        bar a b =
          foo b a
      |]
    it "rejects undefined references" $ do
      doScope [here|
        f = x
      |] `shouldFailWith` ScopeError "x is undefined"
    it "rejects global references to hidden variables" $ do
      doScope [here|
        f =
          let x = 3
          x
        g = x
      |] `shouldFailWith` ScopeError "x is undefined"

  describe "scoping variable-binding expressions" $ do
    it "supports let-bound variables" $ do
      shouldPass $ doScope [here|
        x =
          let y = 3
          x + y
      |]
    it "supports nested function arguments" $ do
      shouldPass $ doScope [here|
        f x =
          let g y = x
          g x
      |]
    it "supports pattern-matches" $ do
      shouldPass $ doScope [here|
        foo x =
          match x
            a = x + a
      |]
    it "supports lambdas" $ do
      shouldPass $ doScope [here|
        foo =
          fun x y
            x + y
      |]
    it "rejects leaked lambda arguments" $ do
      doScope [here|
        f g = g (fun h {3}) h
      |] `shouldFailWith` ScopeError "h is not defined"
    it "rejects local references to hidden variables" $ do
      doScope [here|
        f =
          let x = let y = 3
                  y
          y
      |] `shouldFailWith` ScopeError "y is undefined"
    it "rejects references to hidden arguments" $ do
      doScope [here|
        x =
          let f y = y
          f y
      |] `shouldFailWith` ScopeError "y is undefined"

  describe "scoping patterns" $ do
    it "supports literal patterns" $ do
      shouldPass $ doScope [here|
        foo () x =
          match x
            0 = 2
            _ = x
      |]
    it "supports wildcard patterns" $ do
      shouldPass $ doScope [here|
        foo _ x =
          match x
            0 = 2
            _ = x
      |]
    it "supports top-level wildcards" $ do
      shouldPass $ doScope [here|
        _ = 3
        x =
          let _ = 3
          2
      |]
    it "supports tuples patterns" $ do
      shouldPass $ doScope [here|
        f (x, y, z) =
          x + y + z
        g t =
          match t
            (x, y, z) = x + y + z
      |]
    it "supports tuples let-bindings" $ do
      pendingWith "tuple literal syntax"
      shouldPass $ doScope [here|
        (x, y) =
          (1, 2)
        f =
          let (x, y, z) = (1, 2, 3)
          x + y + z
      |]
    it "rejects identifiers across match arms" $ do
      doScope [here|
       f_ =
          match 3
            a = a
            b = a
      |] `shouldFailWith` ScopeError "a is not defined in second match arm"
    it "rejects leaked pattern-matched identifiers" $ do
      doScope [here|
        _ =
          let b = match 3
                    a = a
          a
      |] `shouldFailWith` ScopeError "a is not defined outside of match"
    it "rejects wildcards as function names" $ do
      doScope [here|
        _ x = 3
      |] `shouldFailWith` ParseError "Invalid syntax"

  describe "scoping recursion and corecursion" $ do
    it "supports top-level recursion" $ do
      shouldPass $ doScope [here|
        f x = f x
      |]
    it "supports let-bound recursion" $ do
      shouldPass $ doScope [here|
        _ =
          let f x = f x
          f
      |]
    it "supports unordered top-level items" $ do
      shouldPass $ doScope [here|
        f x = g
        g = ()
      |]
    it "supports out-of-order local definitions" $ do
      shouldPass $ doScope [here|
        f =
          let i = g
              g x = x
          i
      |]
    it "supports top-level corecursion" $ do
      shouldPass $ doScope [here|
        f x = g x
        g y = f y
      |]
    it "supports let-bound corecursion" $ do
      shouldPass $ doScope [here|
        _ =
          let f x = g x
              g y = f y
          f g
      |]

  describe "scoping data constructors" $ do
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

  describe "scoping redefinitions and shadowing" $ do
    it "suppors local variable shadowing" $ do
      shouldPass $ doScope [here|
        f x =
          let x = 7
          x
      |]
    it "supports the same name across different branches" $ do
      shouldPass $ doScope [here|
        f x =
          match x
            a = a
            a = a
      |]
    it "supports match shadowing" $ do
      shouldPass $ doScope [here|
        f x y =
          match x
            y = y
      |]
    it "supports lambda shadowing" $ do
      shouldPass $ doScope [here|
        f x =
          fun x
            x
      |]
    it "rejects overlapping global identifiers" $ do
      doScope [here|
        f x = x
        f x = x
      |] `shouldFailWith` ScopeError "f is redefined"
    it "rejects overlapping identifiers in the same pattern" $ do
      doScope [here|
        (x, x) = x
      |] `shouldFailWith` ScopeError "x is redefined"
    it "rejects corecursive overlap with argument" $ do
      doScope [here|
        f x = x
        x = 3
      |] `shouldFailWith` ScopeError "x is redefined"
    it "rejects overlapping identifiers in the same argument pattern" $ do
      doScope [here|
        f (x, x) = x
      |] `shouldFailWith` ScopeError "x is redefined"
    it "rejects overlapping identifiers in different patterns" $ do
      doScope [here|
        f x x = x
      |] `shouldFailWith` ScopeError "x is redefined"
    it "rejects overlapping across co-recursive definitions" $ do
      doScope [here|
        f =
          let x = 3
              x = 4
          x
      |] `shouldFailWith` ScopeError "x is redefined"

  describe "scoping naming conventions" $ do
    it "rejects shadowing new" $ do
      doScope [here|
        new = 3
      |] `shouldFailWith` NameError "new is a builtin, cannot be redefined"
    it "rejects shadowing deref" $ do
      doScope [here|
        deref = 3
      |] `shouldFailWith` NameError "deref is a builtin, cannot be redefined"
