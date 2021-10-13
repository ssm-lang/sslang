module Codegen where

import qualified Common.Compiler               as Compiler

import qualified IR.IR                         as I
import qualified IR.Types.Flat                 as Flat

import qualified Language.C                    as C
import qualified Text.PrettyPrint.Mainland     as C
import qualified Text.PrettyPrint.Mainland.Class
                                               as C

import           Codegen.Codegen                ( genProgram )

genIR :: I.Program Flat.Type -> Compiler.Pass [C.Definition]
genIR = genProgram

prettyC :: [C.Definition] -> Compiler.Pass String
prettyC = return . C.pretty 120 . C.pprList
