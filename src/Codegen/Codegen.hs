{- | Translate SSM program to C compilation unit.

What is expected of the IR:

- Well-formed: All primitive functions are applied to the right number of
  arguments.

- Fully-applied: All applications are fully-applied; the only time a term of
  type a -> b appears anywhere is on the left-hand side of an application.

- Defunctionalized: No lambdas/closures; the only kinds of terms with an arrow
  type are variables.

- Name mangled: All variable identifiers are unique.

-}
{-# LANGUAGE QuasiQuotes #-}
module Codegen.Codegen
  ( genProgram
  ) where

import qualified IR.IR                         as L
import qualified Types.Flat                    as L

import           Codegen.Errors                 ( CodegenError(..) )
import           Codegen.Expr                   ( genTop )

import           Codegen.Identifiers
import qualified Language.C.Quote.GCC          as C
import qualified Language.C.Syntax             as C

import           Data.Bifunctor                 ( Bifunctor(bimap) )

{-------- Compilation --------}

-- | Generate a C compilation from an SSM program.
genProgram :: L.Program L.Type -> Either CodegenError [C.Definition]
genProgram p@L.Program { L.programDefs = defs } = do
  (cdecls, cdefs) <- bimap concat concat . unzip <$> mapM genTop defs
  return $ includes ++ cdecls ++ cdefs ++ genInitProgram p

-- | Include statements in the generated C file.
includes :: [C.Definition]
includes = [C.cunit|$esc:("#include \"ssm-platform.h\"")|]

-- | Setup the entry point of the program.
genInitProgram :: L.Program L.Type -> [C.Definition]
genInitProgram L.Program { L.programEntry = entry } = [C.cunit|
    int $id:initialize_program(void) {
       $id:activate($id:(enter_ entry)(&$id:top_parent, $id:root_priority, $id:root_depth));
      return 0;
    }
  |]
