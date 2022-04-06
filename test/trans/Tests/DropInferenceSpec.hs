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


parseDrop :: String -> Pass (I.Program I.Type)
parseDrop s = substMagic (Proxy :: Proxy I.Type) <$> do
  Front.run def s >>= IR.lower def
  -- >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram


spec :: Spec
spec = do
  it "drop inference in named let binding" $ do
    let undropped =
          parseDrop [here|
        top = 
            let a: Int = 5
                b: Int = 10
            a + b
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def
            >>= insertDropsProgram

    let dropped =
          parseDrop [here|
        top =
            let a: Int = 5
                b: Int = 10
            let anon0_let = a + b
            let _ = drop a
            let _ = drop b
            anon0_let
        |]
            >>= IR.ann2Class def
            >>= IR.class2Poly def

    undropped `shouldPassAs` dropped
