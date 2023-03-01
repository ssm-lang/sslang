module IR.Constraint.PrettyPrint where

import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Type            as Typ

import           Prettyprinter
import           Prettyprinter.Render.String

import           Common.Compiler

import           Data.List

import           IR.Constraint.Monad            ( TC )

-- Make Constraint an instance of Pretty!!!

-- Has to operate inside TC monad to invoke toCanType!!!

printConstraint :: Constraint -> TC String
printConstraint CTrue = do 
    return "True"

printConstraint CSaveTheEnvironment = do 
    return "SaveTheEnvironment"

printConstraint (CEqual t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ "Equal " ++ p1 ++ p2

printConstraint (CPattern t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ "Pattern " ++ p1 ++ p2

printConstraint (CLocal i t) = do
    p <- printType t
    return $ "Local " ++ show i ++ p

printConstraint (CForeign (Can.Forall vars ct) t) = do
    pct <- printCanType ct
    let scheme = "Forall " ++ show vars ++ pct

    p <- printType t
    return $ "Foreign " ++ scheme ++ p

printConstraint (CAnd lst) = do
    printed <- mapM printConstraint lst
    return $ "And " ++ concat printed

printConstraint (CLet _ _ _ _ _) = error "not implemented"
--   | CLet { _rigidVars :: [Variable]
--          , _flexVars :: [Variable]
--          , _header :: Map.Map Ident.Identifier Type
--          , _headerCon :: Constraint
--          , _bodyCon :: Constraint
--          }

printType:: Typ.Type -> TC String
printType (TConN i lst) = do 
    printed <- mapM printType lst
    return $ "TConN " ++ show i ++ concat printed

printType (TVarN var)  = do 
    can_t <- toCanType var
    p <- printCanType can_t
    return $ "TVarN " ++ p

printCanType:: Can.Type -> TC String
printCanType t = do return $ show t