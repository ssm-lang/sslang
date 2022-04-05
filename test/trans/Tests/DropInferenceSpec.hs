{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Sanity check test suite for equivalent and unequivalent programs.
module Tests.DropInferenceSpec where
import           Sslang.Test

import           Data.Data                      ( Data
                                                , Proxy(..)
                                                )
import qualified Front
import           Front.Ast
import qualified IR
import           IR.DropInference               ( insertDropsProgram )
import qualified IR.IR                         as I
import           IR.IR
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.SubstMagic                  ( substMagic )
import           IR.Types.Annotated            as I
import           IR.Types.Poly                 as Poly

p :: Proxy (I.Program I.Type)
p = Proxy

parseDrop :: String -> Pass (I.Program I.Type)
parseDrop s = substMagic p <$> do
  Front.run def s >>= IR.lower def
  -- >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram


spec :: Spec
spec = do
  it "drop inference in named let binding" $ do
    let undropped = parseDrop [here|
        top = 
            let a: Int = 5
                b: Int = 10
                _: Int = 2
            a + b
        |] -- >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram

    let dropped = parseDrop [here|
        top =
            let a: Int = 5
                b: Int = 10
            let anon0_let = a + b
            drop b
            drop a
            anon0_let
        |] -- >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram
        
    -- print $ runPass undropped
    undropped `shouldPassAs` dropped
    -- return ()

    {- 
    Program
      { programEntry = VarId main
      , programDefs  =
        [ ( VarId top
          , Let
            [ ( Just (VarId a)
              , Lit (LitIntegral 5) (Type [TBuiltin (Integral 32)])
              )
            , ( Just (VarId b)
              , Lit (LitIntegral 10) (Type [TBuiltin (Integral 32)])
              )
            ]
            (Let
              [ ( Just (VarId anon0_let)
                , Prim (PrimOp PrimAdd)
                       [Var (VarId a) (Type []), Var (VarId b) (Type [])]
                       (Type [])
                )
              ]
              (Let
                [ ( Nothing
                  , App (Var (VarId drop) (Type []))
                        (Var (VarId b) (Type []))
                        (Type [])
                  )
                ]
                (Let
                  [ ( Nothing
                    , App (Var (VarId drop) (Type []))
                          (Var (VarId a) (Type []))
                          (Type [])
                    )
                  ]
                  (Var (VarId anon0_let) (Type []))
                  (Type [])
                )
                (Type [])
              )
              (Type [])
            )
            (Type [])
          )
        ]
      , typeDefs     = []
      }
    -}
