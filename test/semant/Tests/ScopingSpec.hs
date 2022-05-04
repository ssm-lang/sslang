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
      |]
        `shouldFailWith` ScopeError "x is undefined"
    it "rejects global references to hidden variables" $ do
      doScope [here|
        f =
          let x = 3
          x
        g = x
      |]
        `shouldFailWith` ScopeError "x is undefined"

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
      |]
        `shouldFailWith` ScopeError "h is not defined"
    it "rejects local references to hidden variables" $ do
      doScope [here|
        f =
          let x = let y = 3
                  y
          y
      |]
        `shouldFailWith` ScopeError "y is undefined"
    it "rejects references to hidden arguments" $ do
      doScope [here|
        x =
          let f y = y
          f y
      |]
        `shouldFailWith` ScopeError "y is undefined"

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
    it "supports nested tuples patterns" $ do
      shouldPass $ doScope [here|
        f (x, (y, z)) =
          x + y + z
        g (t, r) =
          match t r
            ((x, y), z) = x + y + z
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
       type Either
         Left Int
         Right Int
       f_ =
          match Left 3
            Left a = a
            Right b = a
      |]
        `shouldFailWith` ScopeError "a is not defined in second match arm"
    it "rejects leaked pattern-matched identifiers" $ do
      doScope [here|
        _ =
          let b = match 3
                    a = a
          a
      |]
        `shouldFailWith` ScopeError "a is not defined outside of match"
    it "rejects wildcards as function names" $ do
      doScope [here|
        _ x = 3
      |]
        `shouldFailWith` ParseError "Invalid syntax"
    it "rejects juxtaposed patterns whose head is a variable" $ do
      doScope [here|
        f (x y) = 3
      |]
        `shouldFailWith` NameError "Head of pattern must be a constructor"
    it "rejects juxtaposed patterns whose head is not an identifier" $ do
      doScope [here|
        f ((x y) z) = 3
      |]
        `shouldFailWith` PatternError "Head of pattern must be a constructor"

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

  describe "scoping type constructors" $ do
    it "supports basic type definitions" $ do
      shouldPass $ doScope [here|
        type Unit
          Unit
      |]
      shouldPass $ doScope [here|
        type Bool
          True
          False
      |]
    it "supports type definitions with parameters" $ do
      shouldPass $ doScope [here|
        type Option t
          Some t
          None
      |]
      shouldPass $ doScope [here|
        type Either a b
          Left a
          Right b
      |]
    it "supports type definitions that refer to other type definitions" $ do
      shouldPass $ doScope [here|
        type IntList
          Cons Int IntList
          Nil
      |]
      shouldPass $ doScope [here|
        type List a
          Cons a (List a)
          Nil
      |]
      shouldPass $ doScope [here|
        type Foo a
          Foo (Foo (Foo a))
      |]
      shouldPass $ doScope [here|
        type Foo a
          Foo a a
        type Bar a b
          Bar a (Foo b)
      |]
      shouldPass $ doScope [here|
        type Bar a b
          Bar a (Foo b)
        type Foo a
          Foo a
      |]
      shouldPass $ doScope [here|
        type Bar a b
          Bar a (Foo b)
        type Foo a
          Foo (Bar a (Foo a))
      |]
    it "rejects naming type constructor with variable convention" $ do
      doScope [here|
        type foo
          Foo
      |]
        `shouldFailWith` NameError "foo cannot being with lower case"
    it "rejects type redefinition" $ do
      doScope [here|
        type Foo
          Foo
        type Foo
          Foo
      |]
        `shouldFailWith` ScopeError "type constructor Foo is already defined"
    it "rejects undefined type constructors" $ do
      doScope [here|
        type Foo a
          Foo Bar
      |]
        `shouldFailWith` ScopeError "type constructor Bar is never defined"

  describe "scoping type variables" $ do
    it "supports implicitly quantified type variables in type annotations" $ do
      shouldPass $ doScope [here|
        id a : a -> a = a
      |]
      shouldPass $ doScope [here|
        id (a: a) -> a = a
      |]
    it "requires type parameters to be named like variables" $ do
      doScope [here|
        type Foo F
          Foo F
      |]
        `shouldFailWith` NameError "type parameter 'F' must not be capitalized"
    it "requires type parameters to be quantified in type definitions" $ do
      doScope [here|
        type Foo b
          Foo a
      |]
        `shouldFailWith` ScopeError "type parameter 'a' is not defined"
    it "rejects repeated type parameters" $ do
      doScope [here|
        type Foo a a
          Foo a
      |]
        `shouldFailWith` ScopeError "type parameter 'a' was specified twice"

  describe "scoping data constructors" $ do
    it "supports data constructor matches" $ do
      shouldPass $ doScope [here|
        type T
          D
        foo D = 3
      |]
      shouldPass $ doScope [here|
        type T
          D Int
        foo (D x) = 3
      |]
      shouldPass $ doScope [here|
        type T
          D
          E
        foo (D (D 3)) (E (D D)) = 3
      |]
      shouldPass $ doScope [here|
        type List a
          Cons a (List a)
          Nil
        map (f: a -> a) (l: List a) -> a =
          match l
            Cons a l_ = Cons (f a) (map f l_)
            Nil       = Nil
      |]
    it "rejects repeated data constructors" $ do
      doScope [here|
        type Foo a
          Foo a
          Foo a
      |]
        `shouldFailWith` ScopeError "data constructor 'Foo' was defined twice"
    it "rejects data constructors named like variables" $ do
      doScope [here|
        type Foo a
          foo a
      |]
        `shouldFailWith` NameError
                           "data constructor 'foo' must begin with upper case"

  describe "scoping redefinitions and shadowing" $ do
    it "suppors local variable shadowing" $ do
      shouldPass $ doScope [here|
        f x =
          let x = 7
          x
      |]
    it "supports the same name across different branches" $ do
      shouldPass $ doScope [here|
        type Either
          Left Int
          Right Int
        f x =
          match Left 3
            Left a = a
            Right a = a
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
      |]
        `shouldFailWith` ScopeError "f is redefined"
    it "rejects overlapping identifiers in the same pattern" $ do
      doScope [here|
        (x, x) = x
      |]
        `shouldFailWith` ScopeError "x is redefined"
    it "rejects corecursive overlap with argument" $ do
      doScope [here|
        f x = x
        x = 3
      |]
        `shouldFailWith` ScopeError "x is redefined"
    it "rejects overlapping identifiers in the same argument pattern" $ do
      doScope [here|
        f (x, x) = x
      |]
        `shouldFailWith` ScopeError "x is redefined"
    it "rejects overlapping identifiers in different patterns" $ do
      doScope [here|
        f x x = x
      |]
        `shouldFailWith` ScopeError "x is redefined"
    it "rejects overlapping across co-recursive definitions" $ do
      doScope [here|
        f =
          let x = 3
              x = 4
          x
      |]
        `shouldFailWith` ScopeError "x is redefined"

  describe "scoping naming conventions" $ do
    it "rejects shadowing new" $ do
      doScope [here|
        new = 3
      |]
        `shouldFailWith` NameError "new is a builtin, cannot be redefined"
    it "rejects shadowing deref" $ do
      doScope [here|
        deref = 3
      |]
        `shouldFailWith` NameError "deref is a builtin, cannot be redefined"
