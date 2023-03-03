module IR.Constraint.PrettyPrint (
    printConstraint,
    Doc
) where

import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Type            as Typ

import           Prettyprinter
import           Prettyprinter.Render.String

import           Common.Compiler

import           Data.Map                       (foldWithKey)

import           IR.Constraint.Monad            ( TC )

-- Make Constraint an instance of Pretty!!!

-- Has to operate inside TC monad to invoke toCanType!!!
    
-- data Constraint
--   = CTrue
--   | CSaveTheEnvironment
--   | CEqual Type Type
--   | CPattern Type Type
--   | CLocal Ident.Identifier Type
--   | CForeign Can.Scheme Type
--   | CAnd [Constraint]
--   | CLet { _rigidVars :: [Variable]
--          , _flexVars :: [Variable]
--          , _header :: Map.Map Ident.Identifier Type
--          , _headerCon :: Constraint
--          , _bodyCon :: Constraint
--          }

printConstraint :: Constraint -> TC (Doc ann)
printConstraint CTrue = do 
    return $ pretty "True"

printConstraint CSaveTheEnvironment = do 
    return $ pretty "SaveTheEnvironment"

printConstraint (CEqual t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ pretty "Equal" <+> p1 <+> p2

printConstraint (CPattern t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ pretty "Pattern" <+> p1 <+> p2

printConstraint (CLocal i t) = do
    p <- printType t
    return $ pretty "Local" <+> (pretty . show) i <+> p

printConstraint (CForeign (Can.Forall vars ct) t) = do
    pct <- printCanType ct
    let scheme = pretty "Forall" <+> (pretty . show) vars <+> pct

    p <- printType t
    return $ pretty "Foreign" <+> scheme <+> p

printConstraint (CAnd lst) = do
    printed <- mapM printConstraint lst
    return $ pretty "And" <+> vsep printed

printConstraint (CLet r f h hc bc) = do

    pr <- mapM printVar r
    pf <- mapM printVar f

    -- let pheader = foldWithKey (\k a lst -> (show k ++ printType a):lst) [] h

    phc <- printConstraint hc
    pbc <- printConstraint bc

    return $ pretty "Let" <+> pretty "rigidVars" <+> vsep pr 
                          <+> pretty "flexVars"  <+> vsep pf
                          <+> pretty "placeholder"
                          <+> pretty "headerCon" <+> phc 
                          <+> pretty "bodyCon"   <+> pbc

printType:: Typ.Type -> TC (Doc ann)
printType (TConN i lst) = do 
    printed <- mapM printType lst
    return $ pretty "TConN" <+> (pretty . show) i <+> vsep printed

printType (TVarN var) = do 
    p <- printVar var
    return $ pretty "TVarN" <+> p

printVar:: Variable -> TC (Doc ann)
printVar var = do
    can_t <- toCanType var
    printCanType can_t

printCanType:: Can.Type -> TC (Doc ann)
printCanType t = do return $ (pretty . show) t