module IR.Constraint.PrettyPrint where

import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Type            as Typ

import           Prettyprinter
import           Prettyprinter.Render.String

import           Common.Compiler

import           Data.Map                       (foldWithKey)

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

printConstraint (CLet r f h hc bc) = do

    pr <- mapM printVar r
    pf <- mapM printVar f

    -- let pheader = foldWithKey (\k a lst -> (show k ++ printType a):lst) [] h

    phc <- printConstraint hc
    pbc <- printConstraint bc

    return $ "Let {" ++ concat pr ++ concat pf ++ "placeholder" ++ phc ++ pbc ++ "}"
--   CLet { _rigidVars :: [Variable]
--          , _flexVars :: [Variable]
--          , _header :: Map.Map Ident.Identifier Type
--          , _headerCon :: Constraint
--          , _bodyCon :: Constraint
--          }


printType:: Typ.Type -> TC String
printType (TConN i lst) = do 
    printed <- mapM printType lst
    return $ "TConN " ++ show i ++ concat printed

printType (TVarN var) = do 
    p <- printVar var
    return $ "TVarN " ++ p

printVar:: Variable -> TC String
printVar var = do
    can_t <- toCanType var
    printCanType can_t

printCanType:: Can.Type -> TC String
printCanType t = do return $ show t