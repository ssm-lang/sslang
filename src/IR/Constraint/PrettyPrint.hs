module IR.Constraint.PrettyPrint (
    printConstraint,
    Doc
) where

import qualified IR.Constraint.Canonical       as Can
import qualified Common.Identifiers            as Ident
import           IR.Constraint.Type            as Typ

import           Prettyprinter
import           Prettyprinter.Render.String

-- import           Common.Compiler

import           Data.Map                       as Map

import           IR.Constraint.Monad            ( TC )
import Control.Monad.ST (ST)

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
    return $ pretty "Equal" <+> (align . parens . vsep) [p1, p2]

printConstraint (CPattern t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ pretty "Pattern" <+> (align . parens . vsep) [p1, p2]

printConstraint (CLocal i t) = do
    p <- printType t
    return $ pretty "Local" <+> (align . parens . vsep) [(pretty . show) i, p]

printConstraint (CForeign (Can.Forall vars ct) t) = do
    pct <- printCanType ct
    let scheme = pretty "Forall" <+> (pretty . show) vars <+> pct

    p <- printType t
    return $ pretty "Foreign" <+> (align . parens . vsep) [scheme, p]

printConstraint (CAnd lst) = do
    printed <- mapM printConstraint lst
    return $ pretty "And" <+> (align . brackets . vsep . punctuate comma) printed 

printConstraint (CLet r f h hc bc) = do

    pr <- mapM printVar r
    pf <- mapM printVar f

    let pheader = printHeader h

    phc <- printConstraint hc
    pbc <- printConstraint bc

    return $ pretty "Let" <+> (align . braces . vsep . punctuate comma) [ 
                                pretty "rigidVars:" <+> (brackets . align . vsep . punctuate comma) pr,
                                pretty "flexVars:"  <+> (brackets . align . vsep . punctuate comma) pf,
                                pretty "header:"    <+> align pheader,
                                pretty "headerCon:" <+> align phc, 
                                pretty "bodyCon:"   <+> align pbc]
                            

printType:: Typ.Type -> TC (Doc ann)
printType (TConN i lst) = do 
    printed <- mapM printType lst
    return $ pretty "TConN" <+> parens ((pretty . show) i <+> (brackets . vsep) printed)

printType (TVarN var) = do 
    p <- printVar var
    return $ pretty "TVarN" <+> parens p

printVar:: Variable -> TC (Doc ann)
printVar var = do
    can_t <- toCanType var
    printCanType can_t

printCanType:: Can.Type -> TC (Doc ann)
printCanType t = do return $ (pretty . show) t

printHeader:: Map.Map Ident.Identifier Type -> Doc ann
printHeader headers = pretty "placeholder!!!"