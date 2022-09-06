module IR.Types.Constraint.TypingEnvironment where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( DConId(..)
                                                , TVarId(..)
                                                , fromString
                                                )
import           IR.Types.Constraint.Env        ( Env
                                                , emptyEnv
                                                )
import           IR.Types.Constraint.Inference  ( InferM )
import           IR.Types.Constraint.MultiEquation
                                                ( CRTerm
                                                , Variable
                                                , isConstant
                                                )

type AlgebraicDatatype s = [(DConId, Variable s)]

data TypeInfo s = TypeInfo (Variable s) (AlgebraicDatatype s)

asTypeConstructor :: TypeInfo s -> InferM s (TypeInfo s)
asTypeConstructor x@(TypeInfo v _) = do
  isC <- isConstant v
  if isC
    then return x
    else Compiler.throwError $ Compiler.UnexpectedError $ fromString
      "Type needs to be Constant rigidity."

asTypeVariable :: TypeInfo s -> Variable s
asTypeVariable (TypeInfo v _) = v

data DataConstructor s = DataConstructor Int [Variable s] (CRTerm s)

data Environment s = Environment
  { typeInfo        :: Env TVarId (TypeInfo s)
  , dataConstructor :: Env DConId (DataConstructor s)
  }

emptyEnvironment :: Environment s
emptyEnvironment =
  Environment { typeInfo = emptyEnv, dataConstructor = emptyEnv }
