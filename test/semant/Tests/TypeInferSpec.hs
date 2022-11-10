{-# LANGUAGE QuasiQuotes #-} {-# LANGUAGE OverloadedStrings #-}
module Tests.TypeInferSpec where

import           Sslang.Test

import qualified Common.Compiler               as Compiler
import qualified Front
import qualified IR
import qualified IR.IR                         as I

import           Control.Monad                  ( (>=>) )

infer :: HasCallStack => String -> Pass (I.Program I.Type)
infer = Front.run def >=> IR.lower def >=> IR.typecheck def

typeChecks :: HasCallStack => String -> Expectation
typeChecks = shouldPass . infer

typeErrors :: HasCallStack => String -> Expectation
typeErrors = (`shouldFailWith` TypeError "type error") . infer

typeChecksAs :: HasCallStack => String -> String -> Expectation
typeChecksAs x y = infer x `shouldPassAs` infer y

spec :: Spec
spec = do
  describe "basic type errors" $ do
    it "rejects incompatible type annotations" $ do
      typeErrors [here|
        f (x: Int) = (x: &Int)
      |]
    it "refuses arithmetic on references types" $ do
      typeErrors [here|
        f (x: &Int) = x + 1 - 1
      |]
    it "does not allow integers to be derefenced" $ do
      typeErrors [here|
        f (x: Int) = deref x
      |]
    it "respects the annotated return type of a function" $ do 
      typeErrors [here|
        eof (cout : &Int) -> () =
          wait cout
        main cin (cout : &Int) -> () =
          eof // Should cause a type error
      |]

  describe "polymorphism" $ do
    it "generalizes the identity function" $ do
      typeChecks [here|
        f _ =
            let id x = x
            id ()
            id 3
      |]
    it "generalizes the identity function, even when in a let block" $ do
      typeChecks [here|
        f _ =
            let id x = x
                y = id ()
                z = id 3
            z
      |]
    it "allows the identity function to be called on itself" $ do
      typeChecks [here|
        f _ =
            let id x = x
            (id id) ()
      |]
    it "generalizes the identity function at the top-level" $ do
      typeChecks [here|
        id x = x
        f = id id
        g x = id x
        h x = id 3
      |]

  describe "recursion" $ do
    it "supports non-terminating recursive functions" $ do
      typeChecks [here|
        f x = f (x - 1)
      |]
    it "supports non-terminating mutually recursive functions" $ do
      typeChecks [here|
        f x = g (x - 1)
        g x = f (x - 1)
      |]


  describe "sensible type annotations" $ do
    it "accepts a precise annotation of the identity function" $ do
      typeChecks [here|
        f _ =
            let id x : a -> a = x
            id ()
      |]
    it "accepts a restriction of the identity function to integers" $ do
      typeChecks [here|
        f _ =
            let id x : Int -> Int = x
            id 3
      |]
    it "accepts stacked type annotations that gradually specialize the identity function" $ do
      typeChecks [here|
        f _ =
            let id x = x
            (id : a -> a : () -> ()) ()
      |]
    it "accepts partially annotated identity functions" $ do
      typeChecks [here|
        f _ =
            let id x -> Int = x
            id 3
      |]
      typeChecks [here|
        f _ =
            let id (x: Int) = x
            id 3
      |]
      typeChecks [here|
        f _ =
            let id x = (x: Int)
            id 3
      |]

  describe "bad type annotations" $ do
    it "rejects a type annotation that over-generalizes a function argument" $ do
      typeErrors [here|
        addOne (x: a) = x + 1
      |]
    it "rejects a type annotation that over-generalizes a variable" $ do
      pendingWith "some kind of generalization bug"
      typeErrors [here|
        addOne x = (x: a) + 1
      |]
    it "rejects a stacked type annotations that generalize an already-specialized identity function" $ do
      typeErrors [here|
        f _ =
            let id x = x
            // Error: a -> a is not more specific than Int -> Int
            (id : Int -> Int : a -> a) 3
      |]
    it "rejects applying an incorrectly specialized identity function (annotated variable)" $ do
      typeErrors [here|
        f _ =
            let id x = x
            // Error: id is specialized to Int, cannot be applied to ()
            (id : Int -> Int) ()
      |]
    it "rejects applying an incorrectly specialized identity function (annotated type)" $ do
      typeErrors [here|
        f _ =
            let id x : Int -> Int = x
            // Error: id is specialized to Int -> Int, cannot be applied to ()
            id ()
      |]
    it "rejects applying an incorrectly specialized identity function (annotated return type)" $ do
      typeErrors [here|
        f _ =
            let id x -> Int = x
            // Error: id is specialized to Int -> Int, cannot be applied to ()
            id ()
      |]
    it "rejects applying an incorrectly specialized identity function (annotated argument type)" $ do
      typeErrors [here|
        f _ =
          let id (x: Int) = x
          // Error: id is specialized to Int -> Int, cannot be applied to ()
          id ()
      |]

  -------- Comparison-based test cases --------

  describe "polymorphism (comparison-based tests)" $ do
    it "does not overgeneralize non-free unification variables" $ do
      [here|
        f x =
          let y = x
          y
      |] `typeChecksAs` [here|
        f (x: a) -> a =
          let y = x
          y
      |]
    it "correctly captures non-free unification variable in nested functions" $ do
      pendingWith "Correct mangling of type variables"
      [here|
        f x =
          let g y = x
          g
      |] `typeChecksAs` [here|
        f (x: a) -> (b -> a) =
          let g y = x
          g
      |]
    it "correctly captures non-free unification variables in nested functions with name shadowing" $ do
      [here|
        f x =
          let g x y = x
          g x
      |] `typeChecksAs` [here|
        f (x: a) -> (b -> a) =
          let g x y = x
          g x
      |]

    it "correctly generalizes a nested function when possible" $ do
      [here|
        f x =
          let g x = x
          g
      |] `typeChecksAs` [here|
        f (x : a) -> (b -> b) =
          let g x = x
          g
      |]
    it "correctly generalizes a nested function (with name shadowing) when possible" $ do
      [here|
        f x =
          let g y x = x
          g x
      |] `typeChecksAs` [here|
        f (x : a) -> (b -> b) =
          let g y x = x
          g x
      |]
    it "accepts a wonky definition of the identity function" $ do
      [here|
        f x =
          let g y = x
          g g
      |] `typeChecksAs` [here|
        f (x : a) -> a =
          let g y = x
          g g
      |]
    it "accepts a wonky definition of the flipped const function" $ do
      [here|
        f x =
          let g x = x
          g g
      |] `typeChecksAs` [here|
        f (x : a) -> (b -> b) =
          let g x = x
          g g
      |]
    it "correctly generalizes a nested function (with name shadowing) when possible" $ do
      [here|
        f x =
          let g y = x y
          g
      |] `typeChecksAs` [here|
        f (x: (a -> b)) -> (a -> b) =
          let g y = x y
          g
      |]

  describe "annotating identity function (comparison-based)" $ do
    let int2int = infer [here|
          id (x: Int) =
            x: Int
          |]
        int2unknown = infer [here|
          id (x: Int) =
            x
          |]
        unknown2int = infer [here|
          id x =
            x: Int
          |]
        none = infer [here|
          id x =
            x
          |]
    it "unifies the argument with the return type" $ do
      int2int `shouldPassAs` int2unknown
    it "unifies the return type with the argument" $ do
      int2int `shouldPassAs` unknown2int

    it "meaningfully specializes the polymorphic identity function" $ do
      int2unknown `shouldPassButNotAs` none
      unknown2int `shouldPassButNotAs` none
      int2int `shouldPassButNotAs` none

  describe "identity function annotated with type variables (comparison-based)" $ do
    let a2a = infer [here|
          id (x: a) = x
        |]
        b2b = infer [here|
          id (x: b) = x
        |]
        none = infer [here|
          id x = x
        |]
    it "correctly interprets type variables in annotations" $ do
      a2a `shouldPassAs` none
    it "treats type variables the same modulo alpha-equivalence" $ do
      a2a `shouldPassAs` b2b

  describe "checks kindness of type constructors" $ do
    it "rejects partially-applied, user-defined type constructos" $ do
      typeErrors [here|
        type MyEither a b
          MyLeft a
          MyRight b

        l : MyEither () = MyLeft ()
      |]

      typeErrors [here|
        type MyEither a b
          MyLeft a
          MyRight b

        l : MyEither () = MyLeft
      |]

      typeErrors [here|
        type MyEither a b
          MyLeft a
          MyRight b

        l : MyEither = MyLeft
      |]

      typeErrors [here|
        type MyEither a b
          MyLeft a
          MyRight b

        l : MyEither () MyEither = MyLeft ()
      |]

  -------- Larger, full test cases --------

  describe "larger, full-program test cases" $ do
    it "typechecks an unannotated, monomorphic program" $ do
      [here|
            toggle(led : &Int) -> () =
              (led: &Int) <- (1 - deref (led: &Int): Int)
            slow(led : &Int) -> () =
              let e1 = (new () : &())
              loop
                ((toggle: &Int -> ()) (led: &Int): ())
                after 30 , (e1: &()) <- ()
                wait (e1: &())
            fast(led : &Int) -> () =
              let e2 = (new () : &())
              loop
                ((toggle: &Int -> ()) (led: &Int): ())
                after 20 , (e2: &()) <- ()
                wait (e2: &())
            main(led : &Int) -> ((), ()) =
              par ((slow: &Int -> ()) (led: &Int): ())
                  ((fast: &Int -> ()) (led: &Int): ())
        |] `typeChecksAs` [here|
            toggle(led) =
              led <- 1 - deref led
            slow(led) =
              let e1 = new ()
              loop
                toggle led
                after 30 , e1 <- ()
                wait e1
            fast(led) =
              let e2 = new ()
              loop
                toggle led
                after 20 , e2 <- ()
                wait e2
            main(led) =
              par slow led
                  fast led
            |]
    it "typechecks a program with terminating single recursion" $ do
      typeChecks [here|
            type MyBool
              MyFalse
              MyTrue

            main x =
              match x
                MyFalse = main MyTrue
                _       = 1
            |]
    it "typechecks a program with terminating mutual recursion" $ do
      typeChecks [here|
            type MyBool
              MyFalse
              MyTrue

            type Number
              Zero
              One
              Two
              Three

            isOdd x =
              match x
                Zero  = MyFalse
                One   = isEven Zero
                Two   = isEven One
                Three = isEven Two

            isEven x =
              match x
                Zero  = MyTrue
                One   = isOdd Zero
                Two   = isOdd One
                Three = isOdd Two
            |]

    it "typechecks a program with complicated patterns" $ do
      typeChecks [here|
            type MyEither a b
              MyLeft a
              MyRight b

            flattenEither x: MyEither (MyEither a b) (MyEither b a) -> MyEither a b =
              match x
                MyLeft  (MyLeft a) = MyLeft a
                MyLeft  (MyRight b)  = MyRight b
                MyRight (MyRight a)  = MyLeft a
                MyRight (MyLeft b) = MyRight b
            |]

    it "typechecks a program with a specialized complicated pattern match" $ do
      pendingWith "some kind of generalization bug"
      typeChecks [here|
            type MyEither a b
              MyLeft a
              MyRight b

            flattenEither x: MyEither (MyEither a ()) (MyEither () a) -> MyEither a () =
              match x
                MyLeft  (MyLeft a) = MyLeft a
                MyLeft  (MyRight b)  = MyRight b
                MyRight (MyRight a)  = MyLeft a
                MyRight (MyLeft b) = MyRight b
            |]

    it "typechecks a program with complicated patterns and annotations" $ do
      typeChecks [here|
            type MyEither a b
              MyLeft a
              MyRight b

            flattenEither x: MyEither (MyEither Int ()) (MyEither () Int) -> MyEither () Int =
              match x
                MyLeft  (MyRight a: MyEither Int ()) = MyLeft a
                MyLeft  (MyLeft b:  MyEither Int ())  = MyRight b
                MyRight (MyLeft a:  MyEither () Int)  = MyLeft a
                MyRight (MyRight b: MyEither () Int) = MyRight b
            |]

