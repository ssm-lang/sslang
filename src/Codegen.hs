module Codegen where

import qualified Common.Compiler               as Compiler
import           Common.Default                 ( Default(..) )

import qualified IR.IR                         as I
import qualified IR.Types.Flat                 as Flat

import qualified Text.PrettyPrint.Mainland     as C
import qualified Text.PrettyPrint.Mainland.Class
                                               as C

import           Codegen.Codegen                ( genProgram )
import           System.Console.GetOpt          ( ArgDescr(..)
                                                , OptDescr(..)
                                                )

data Mode = Continue
  deriving (Eq, Show)

data Options = Options
  { mode      :: Mode
  , textWidth :: Int
  }
  deriving (Eq, Show)

instance Default Options where
  def = Options { mode = Continue, textWidth = 120 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ""
           ["codegen-textwidth"]
           (ReqArg (\tw o -> o { textWidth = read tw }) "<textwidth>")
           ""
  ]

run :: Options -> I.Program Flat.Type -> Compiler.Pass String
run opt ir = do
  cdefs <- genProgram ir
  return $ C.pretty (textWidth opt) $ C.pprList cdefs
