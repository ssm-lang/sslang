module IR.Constraint.PrettyPrint (
    printConstraint,
    Doc
) where

import qualified IR.Constraint.Canonical       as Can
import qualified Common.Identifiers            as Ident
import           IR.Constraint.Type            as Typ

import           Prettyprinter

import           Data.Map                       as Map

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
    return $ pretty "true"

printConstraint CSaveTheEnvironment = do 
    return $ pretty "SaveTheEnvironment"

printConstraint (CEqual t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ (align . hsep) [p1, pretty "=", p2]

printConstraint (CPattern t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ (align . hsep) [p1, pretty "=p", p2]

printConstraint (CLocal i t) = do
    p <- printType t
    return $ pretty "local" <+> (align . parens . hsep) [(pretty . show) i, p]

printConstraint (CForeign (Can.Forall vars ct) t) = do
    pct <- printCanType ct
    let scheme = pretty "∀" <+> (pretty . show) vars <+> dot <+> pct

    p <- printType t
    return $ pretty "foreign" <+> (align . parens . hsep) [scheme, p]

printConstraint (CAnd lst) = do
    printed <- mapM printConstraint lst
    let joined = (vsep . punctuate (pretty " ∧")) printed
    return joined

printConstraint (CLet r f h hc bc) = do

    pr <- mapM printVar r
    pf <- mapM printVar f

    pheader <- printHeader h

    phc <- printConstraint hc
    pbc <- printConstraint bc

    return $ (align . vsep) [pretty "let" <+> pheader, 
                            indent 2 (vsep [ 
                                align $ pretty "rigid" <+> colon <+> (align . hsep . punctuate comma) pr ,
                                align $ pretty "flex " <+> colon <+> (align . hsep . punctuate comma) pf,
                                
                                align lbracket,
                                align $ indent 2 phc,
                                align rbracket
                            ]),
                            align (pretty "in"), 
                            indent 2 $ align pbc]
                            

printType:: Typ.Type -> TC (Doc ann)
printType c@(TConN i lst) = do 
    printed <- mapM printType lst
    if show i == "->"
        then let [a, b] = printed in return $ parens $ a <+> pretty "->" <+> b
        else return $ pretty i <+> hsep printed
    

printType (TVarN var) = do printVar var

printVar:: Variable -> TC (Doc ann)
printVar var = do
    can_t <- toCanType var
    printCanType can_t

printCanType:: Can.Type -> TC (Doc ann)
printCanType t = do return $ pretty t

printHeader:: Map.Map Ident.Identifier Type -> TC (Doc ann)
printHeader headers = do 
    lst <- mapM printPair $ toList headers
    return $ (align . vsep . punctuate semi) lst
    where 
        printPair (i, t) = do 
            pt <- printType t
            return $ (pretty . show) i <+> colon <+> pt
