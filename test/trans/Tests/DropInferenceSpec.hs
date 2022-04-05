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
parseDrop s = Front.run def s >>= IR.lower def
  -- >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram


spec :: Spec
spec = do
  it "drop inference in named let binding" $ do
    let undropped = do
          parseDrop [here|
        top = 
            let a: Int = 5
                b: Int = 10
                _: Int = 2
            a + b
        |] -- >>= IR.ann2Class def >>= IR.class2Poly def >>= insertDropsProgram

    let dropped = substMagic p <$> do
          parseDrop [here|
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
