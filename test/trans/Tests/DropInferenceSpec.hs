{-# LANGUAGE QuasiQuotes #-}
-- | Sanity check test suite for equivalent and unequivalent programs.
module Tests.DropInferenceSpec where
import           Sslang.Test

import qualified Front
import           Front.Ast
import qualified IR
import           IR.DropInference               ( insertDropsProgram )
import qualified IR.IR                         as I
import           IR.IR
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.Types.Poly                 as Poly
parseLift :: String -> Pass (I.Program Poly.Type)
parseLift s =
  Front.run def s
    >>= IR.lower def
    >>= IR.ann2Class def
    >>= IR.class2Poly def
    >>= insertDropsProgram

spec :: Spec
spec = do
  it "drop inference in named let binding" $ do
    let undropped = parseLift [here|
        top = 
            let a: Int = 5
                b: Int = 10
            a + b
        |]  
    print $ runPass undropped
    --undropped `shouldPassAs` dropped


    {-
    Right
      ( Program
        { programEntry = VarId main
        , programDefs  = [ ( VarId top
                           , Let
                             [ ( Just (VarId a)
                               , Lit (LitIntegral 5) (TBuiltin (Integral 32))
                               )
                             , ( Just (VarId b)
                               , Lit (LitIntegral 10) (TBuiltin (Integral 32))
                               )
                             ]
                             (Let
                               [ ( Just (VarId anon0)
                                 , Prim
                                   (PrimOp PrimAdd)
                                   [ Var (VarId a) (TBuiltin (Integral 32))
                                   , Var (VarId b) (TBuiltin (Integral 32))
                                   ]
                                   (TBuiltin (Integral 32))
                                 )
                               ]
                               (Let
                                 [ ( Nothing
                                   , Prim
                                     Drop
                                     [Var (VarId a) (TBuiltin (Integral 32))]
                                     (TBuiltin (Integral 32))
                                   )
                                 ]
                                 (Let
                                   [ ( Nothing
                                     , Prim
                                       Drop
                                       [Var (VarId b) (TBuiltin (Integral 32))]
                                       (TBuiltin (Integral 32))
                                     )
                                   ]
                                   (Var (VarId anon0) (TBuiltin (Integral 32)))
                                   (TBuiltin (Integral 32))
                                 )
                                 (TBuiltin (Integral 32))
                               )
                               (TBuiltin (Integral 32))
                             )
                             (TBuiltin (Integral 32))
                           )
                         ]
        , typeDefs     = []
        }
      , []
      )
      -}
    return ()
