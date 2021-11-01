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
        input = I.Let [((Just vy), ex)] ey (Ann.Type [])                     -- let y = (x :: Unit) in y
        output = I.Let [((Just vy), ex)] ey' (Ann.Type [Ann.TBuiltin Unit])  -- (let y = (x::Unit) in (y::Unit)) :: Unit
    inferExpr emptyCtx input `shouldBe` output

  it "inferExpr infers after expression" $ do
    let t = I.Lit (I.LitIntegral 1) (Ann.Type [])
        x = I.Var (VarId (Identifier "Led")) (Ann.Type [])
        v = I.Lit (I.LitIntegral 100) (Ann.Type [])
        input = I.Prim I.After [t, x, v] (Ann.Type [])                       -- after 1, Led <- 100

        t' = I.Lit (I.LitIntegral 1) (Ann.Type [Ann.TBuiltin (Integral 32), Ann.TBuiltin (Integral 32)])
        x' = I.Var (VarId (Identifier "Led")) (Ann.Type [Ann.TBuiltin (Ref (Ann.Type [Ann.TBuiltin (Integral 32)]))])
        v' = I.Lit (I.LitIntegral 100) (Ann.Type [Ann.TBuiltin (Integral 32)])
        output = I.Prim I.After [t', x', v'] (Ann.Type [Ann.TBuiltin Unit])  -- (after 1::[Int, Int], Led::[Int] <- 100::[Int]) :: Unit

    inferExpr emptyCtx input `shouldBe` output

  it "inferExpr infers loop expression" $ do
    let t = I.Lit (I.LitIntegral 1) (Ann.Type [])
        x = I.Var (VarId (Identifier "Led")) (Ann.Type [])
        v = I.Lit (I.LitIntegral 100) (Ann.Type [])
        e1 = I.Prim I.After [t, x, v] (Ann.Type [])                          -- after 1, Led <- 100
        e2 = I.Prim I.Wait [x] (Ann.Type [])                                 -- wait Led
        input = I.Prim I.Loop [e1, e2] (Ann.Type [])                         -- loop

        t' = I.Lit (I.LitIntegral 1) (Ann.Type [Ann.TBuiltin (Integral 32), Ann.TBuiltin (Integral 32)])
        x' = I.Var (VarId (Identifier "Led")) (Ann.Type [Ann.TBuiltin (Ref (Ann.Type [Ann.TBuiltin (Integral 32)]))])
        v' = I.Lit (I.LitIntegral 100) (Ann.Type [Ann.TBuiltin (Integral 32)])
        e1' = I.Prim I.After [t', x', v'] (Ann.Type [Ann.TBuiltin Unit])     -- (after 1::[Int, Int], Led::[Int] <- 100::[Int]) :: Unit
        e2' = I.Prim I.Wait [x] (Ann.Type [Ann.TBuiltin Unit])               -- (wait Led::[]) :: Unit
        output = I.Prim I.Loop [e1', e2'] (Ann.Type [Ann.TBuiltin Unit])     -- loop :: Unit

    inferExpr emptyCtx input `shouldBe` output
