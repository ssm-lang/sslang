module Tests.ClassInstantiationSpec where

import qualified IR.IR                         as I
import qualified IR.Types.Classes              as C
import qualified IR.Types.Poly                 as P

import           Common.Compiler                          ( Pass
                                                          , runPass
                                                          )
import           Common.Identifiers                       ( fromString )
import           IR.ClassInstantiation                    ( instNameMangle
                                                          , instProgram
                                                          )
import           IR.IR                                    ( Alt(..)
                                                          , ClassDef(..)
                                                          , Expr(..)
                                                          , Literal(..)
                                                          )
import           IR.Types.TypeSystem                      ( Builtin(..)
                                                          , InstConstraint(..)
                                                          , TypeDef(..)
                                                          , TypeVariant
                                                              ( VariantNamed
                                                              )
                                                          , arrow
                                                          , int
                                                          , projectBuiltin
                                                          , void
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
    _ -> expectationFailure "ClassInstantiation pass failed"

_clsName = "Collapsible"
_clsTVar = "a"
_clsMethodName = "collapse"
_clsMethodType = C.TBuiltin (Arrow (C.TVar (fromString _clsTVar)) (int 32))
_cls = I.ClassDef
    { I.className    = fromString _clsName
    , I.classTVar    = C.TVar (fromString _clsTVar)
    , I.classMethods = [(fromString _clsMethodName, _clsMethodType)]
    }
_clsADT =
    ( fromString _clsName
    , TypeDef
        { arity       = 1
        , variants    = [ ( fromString _clsName
                          , VariantNamed
                              [(fromString _clsMethodName, _clsMethodType)]
                          )
                        ]
        , typeDefVars = [C.TVar (fromString _clsTVar)]
        }
    )

_instType = int 32
_instMethod = Lambda
    (Just $ fromString "x")
    (Match
        (Var (fromString "x") (int 32))
        [ (AltLit (LitIntegral 0), Lit (LitIntegral 0) (int 32))
        , (AltDefault Nothing    , Lit (LitIntegral 1) (int 32))
        ]
        (int 1)
    )
    _clsMethodType

_inst = I.InstDef { I.instConstraint = _instType `IsIn` fromString _clsName
                  , I.instMethods = [(fromString _clsMethodName, _instMethod)]
                  }

_instData =
    ( fromString $ instNameMangle (fromString _clsName) _instType
    , App
        (Data
            (fromString _clsName)
            (_clsMethodType `arrow` C.TCon (fromString _clsName) [_instType])
        )
        _instMethod
        (C.TCon (fromString _clsName) [_instType])
    )

spec :: Spec
spec = do
    it "instantiates a typeclass with single parameter and single method" $ do
        let withClass = emptyProgram { I.programClasses = [_cls] }
            withoutClass = emptyProgram { I.programTypes = [_clsADT] }
            instantiatedWithClass = instProgram withClass
            instantiatedWithoutClass = instProgram withoutClass
        instantiatedWithClass `isSameProgramAs` instantiatedWithoutClass

    it
            "instantiates a typeclass and instance with single parameter and single method"
        $ do
              let withClass = emptyProgram { I.programClasses = [_cls]
                                           , I.programInsts   = [_inst]
                                           }
                  withoutClass = emptyProgram { I.programTypes = [_clsADT]
                                              , I.programDefs  = [_instData]
                                              }
                  instantiatedWithClass    = instProgram withClass
                  instantiatedWithoutClass = instProgram withoutClass
              instantiatedWithClass `isSameProgramAs` instantiatedWithoutClass
