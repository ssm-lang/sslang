module IR.Constraint.PrettyPrint where

import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Type            as Typ

import           Prettyprinter
import           Prettyprinter.Render.String

import           Common.Compiler

import           Data.List

-- Make Constraint an instance of Pretty!!!

printConstraint :: Constraint -> String
printConstraint CTrue = "True"
printConstraint CSaveTheEnvironment = "SaveTheEnvironment"
printConstraint (CEqual t1 t2) = "Equal " ++ printType t1 ++ printType t2
printConstraint (CPattern t1 t2) = "Pattern " ++ printType t1 ++ printType t2
printConstraint (CLocal i t) = "Local " ++ show i ++ printType t
printConstraint (CForeign (Can.Forall vars ct) t) = "Foreign " ++ ("Forall " ++ show vars ++ printCanType ct) ++ printType t
printConstraint (CAnd lst) = "And " ++ concatMap printConstraint lst
printConstraint (CLet _ _ _ _ _) = error "not implemented"
--   | CLet { _rigidVars :: [Variable]
--          , _flexVars :: [Variable]
--          , _header :: Map.Map Ident.Identifier Type
--          , _headerCon :: Constraint
--          , _bodyCon :: Constraint
--          }

printType:: Typ.Type -> String
printType (TConN i lst) = "TCon " ++ show i ++ concatMap printType lst
printType (TVarN var)  = error "not implemented"

printCanType:: Can.Type -> String
printCanType _ = error "not implemented"