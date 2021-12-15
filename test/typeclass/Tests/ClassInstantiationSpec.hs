module Tests.ClassInstantiationSpec where

import qualified IR.IR                         as I
import qualified IR.Types.Classes              as C
import qualified IR.Types.Poly                 as P

import           Common.Compiler                          ( Pass
                                                          , runPass
                                                          )
import           Common.Identifiers                       ( fromString )
import           IR.ClassInstantiation                    ( instProgram )
import           IR.IR                                    ( ClassDef(..) )
import           IR.Types.TypeSystem                      ( Builtin(..)
                                                          , TypeDef(..)
                                                          , TypeVariant
                                                            ( VariantNamed
                                                            )
                                                          , projectBuiltin
                                                          )

import           Test.Hspec                               ( Expectation
                                                          , Spec(..)
                                                          , describe
                                                          , expectationFailure
                                                          , it
                                                          , shouldBe
                                                          )

emptyProgram :: I.Program C.Type
emptyProgram = I.Program { I.programEntry   = fromString "main"
                         , I.programDefs    = []
                         , I.programTypes   = []
                         , I.programClasses = []
                         , I.programInsts   = []
                         }

isSameProgramAs
  :: Pass (I.Program P.Type) -> Pass (I.Program P.Type) -> Expectation
isSameProgramAs p1 p2 = case (runPass p1, runPass p2) of
  (Right p1', Right p2') -> p1' `shouldBe` p2'
  _                      -> expectationFailure "ClassInstantiation pass failed"


spec :: Spec
spec = do
  it "instantiates a typeclass with single parameter and single method" $ do
    let
      clsName       = "ExampleClass"
      clsTVar       = "a"
      clsMethodName = "exampleMethod"
      clsMethodType =
        C.TBuiltin (Arrow (C.TVar (fromString clsTVar)) (projectBuiltin Void))
      cls = I.ClassDef
        { I.className    = fromString clsName
        , I.classTVar    = C.TVar (fromString clsTVar)
        , I.classMethods =
          [ ( fromString clsMethodName
            , C.TBuiltin
-- instClassMethod :: (VarId, Classes.Type) -> TypeVariant Poly.Type
              (Arrow (C.TVar (fromString clsTVar)) (projectBuiltin Void))
            )
          ]
        }
      withClass    = emptyProgram { I.programClasses = [cls] }
      withoutClass = emptyProgram
        { I.programTypes =
          [ ( fromString clsName
            , TypeDef
              { arity       = 1
              , variants    =
                [ ( fromString clsName
                  , VariantNamed [(fromString clsMethodName, clsMethodType)]
                  )
                ]
              , typeDefVars = [C.TVar $ fromString clsTVar]
              }
            )
          ]
        }
      instantiatedWithClass    = instProgram withClass
      instantiatedWithoutClass = instProgram withoutClass
    instantiatedWithClass `isSameProgramAs` instantiatedWithoutClass
