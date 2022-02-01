{- | Code generation (codegen) stage of the compiler pipeline.

This module exposes some options for configuring what C code is generated and
how it is represented.
-}
module Codegen where

import qualified Common.Compiler               as Compiler
import           Common.Default                 ( Default(..) )

import qualified IR.IR                         as I
import qualified IR.Types.Poly                 as I

import qualified Text.PrettyPrint.Mainland     as C
import qualified Text.PrettyPrint.Mainland.Class
                                               as C

import           Codegen.Codegen                ( genProgram )
import           System.Console.GetOpt          ( ArgDescr(..)
                                                , OptDescr(..)
                                                )

-- | Operation modes for the codegen compiler stage.
data Mode = Continue
  deriving (Eq, Show)

-- | Compiler options for the codegen compiler stage.
data Options = Options
  { mode      :: Mode
  , textWidth :: Int
  }
  deriving (Eq, Show)

instance Default Options where
  def = Options { mode = Continue, textWidth = 120 }

-- | CLI options for the codegen compiler stage.
options :: [OptDescr (Options -> Options)]
options =
  [ Option ""
           ["codegen-textwidth"]
           (ReqArg (\tw o -> o { textWidth = read tw }) "<textwidth>")
           "Line width for pretty-printing the generated C code."
  ]


-- | Codegen compiler stage.
run :: Options -> I.Program I.Type -> Compiler.Pass String
run opt ir = do
  cdefs <- genProgram ir
  return $ C.pretty (textWidth opt) $ C.pprList cdefs
