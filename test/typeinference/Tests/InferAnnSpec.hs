module Tests.InferAnnSpec where

import           Test.Hspec                     ( Spec(..)
                                                , it
                                                , shouldBe
                                                )

import qualified Data.Map                      as M

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes

import           IR.Types.TypeSystem            ( Builtin(..) )
import           IR.TypeInference               ( emptyCtx
                                                , inferExpr
                                                )
import           Common.Identifiers             ( VarId(..)
                                                , Identifier(..)
                                                )

spec :: Spec
spec = do
  it "inferExpr copies the annotated types" $ do
    let input = I.Var (VarId (Identifier "x")) (Ann.Type [Ann.TBuiltin Unit])
    inferExpr emptyCtx input `shouldBe` input

  it "inferExpr checks typing context and append it to the annotated types" $ do
    let ctx = M.singleton (VarId (Identifier "x")) (Ann.Type [Ann.TBuiltin Unit])
        input = I.Var (VarId (Identifier "x")) (Ann.Type [])
        output = I.Var (VarId (Identifier "x")) (Ann.Type [Ann.TBuiltin Unit])
    inferExpr ctx input `shouldBe` output

  it "inferExpr infers from bindings in let expression" $ do
    let vx = VarId (Identifier "x")
        vy = VarId (Identifier "y")
        ex = I.Var vx (Ann.Type [Ann.TBuiltin Unit])
        ey = I.Var vy (Ann.Type [])
        ey' = I.Var vy (Ann.Type [Ann.TBuiltin Unit])
        input = I.Let [((Just vy), ex)] ey (Ann.Type [])                    -- let y = (x :: unit) in y
        output = I.Let [((Just vy), ex)] ey' (Ann.Type [Ann.TBuiltin Unit]) -- (let y = (x :: unit) in (y :: unit)) :: unit
    inferExpr emptyCtx input `shouldBe` output
