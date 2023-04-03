module IR.Constraint.PrettyPrint (
    printConstraint,
    PrintingOptions(..),
    Doc
) where

import qualified IR.Constraint.Canonical       as Can
import qualified Common.Identifiers            as Ident
import IR.Constraint.Type as Typ
    ( toCanType,
      Constraint(CLet, CTrue, CSaveTheEnvironment, CEqual, CPattern,
                 CLocal, CForeign, CAnd),
      Type(..),
      Variable )

import           Prettyprinter

import           Data.Map                      as Map  ( toList, Map, null )
-- import           Data.List                     as List ( isPrefixOf )

import           IR.Constraint.Monad                   ( TC )  

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

data PrintingOptions = Pretty | NoAuxiliary deriving (Eq)

indentation :: Doc ann -> Doc ann
indentation = indent 2

printConstraint :: PrintingOptions -> Constraint -> TC (Doc ann)
printConstraint _ CTrue = do 
    return $ pretty "true"

printConstraint _ CSaveTheEnvironment = do 
    return $ pretty "SaveTheEnvironment"

printConstraint _ (CEqual t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ (align . hsep) [p1, pretty "=", p2]

printConstraint _ (CPattern t1 t2) = do
    p1 <- printType t1
    p2 <- printType t2
    return $ (align . hsep) [p1, pretty "=p", p2]

printConstraint _ (CLocal i t) = do
    p <- printType t
    return $ (align . hsep) [(pretty . show) i, pretty "<:", p]

printConstraint _ (CForeign (Can.Forall vars ct) t) = do
    pct <- printCanType ct
    let scheme = pretty "forall" <+> (pretty . show) vars <+> dot <+> pct

    p <- printType t
    return $ pretty "primitive" <+> (align . hsep) [scheme, pretty "<:", p]

printConstraint opt (CAnd lst) = do
    printed <- mapM (printConstraint opt) lst
    let joined = vsep printed
    return joined

printConstraint opt c@(CLet r f h hc bc) = do
    pr <- mapM printVar r
    pf <- mapM printVar f

    pheader <- printHeader h

    phc <- printConstraint opt hc
    pbc <- printConstraint opt bc

    blocks <- case c of 
            (CLet _ [v] _ (CAnd ((CEqual (TVarN v') _):xs)) CTrue) | opt == NoAuxiliary && Map.null h && v == v'-> do
                block <- printConstraint opt (CAnd xs)
                return [block]

            (CLet _ _ _ _ CTrue) | Map.null h -> 
                return [ 
                    pretty "exists" <+> list (pf ++ pr),
                    indentation (
                        align phc
                    )
                ]
            (CLet [] [] _ CTrue _) -> 
                return [ 
                    pretty "def",
                    indentation (
                        align pheader
                    ),
                    pretty "in", 
                    indentation pbc
                ]
            (CLet _ _ o (CLet [] [] i CTrue innerbc) _) | o == i -> do
                pinnerbc <- printConstraint opt innerbc
                return [
                    pretty "let rec" <+> list (pf ++ pr),
                    indentation (
                        align pheader
                    ),
                    pretty "given",
                    indentation pinnerbc,
                    pretty "in", 
                    indentation pbc
                    ]
            _ -> 
                return [ 
                    pretty "let" <+> list (pf ++ pr),
                    indentation (
                        align pheader
                    ),
                    pretty "given",
                    indentation (
                        align phc
                    ),
                    pretty "in", 
                    indentation pbc
                ]

    return $ (align . vsep) blocks



printType :: Typ.Type -> TC (Doc ann)
printType (TConN i lst) = do 
    printed <- mapM printType lst
    if show i == "->"
        then let [a, b] = printed in return $ parens $ a <+> pretty "->" <+> b
        else return $ pretty i <+> hsep printed
    

printType (TVarN var) = do printVar var

printVar :: Variable -> TC (Doc ann)
printVar var = do
    can_t <- toCanType var
    printCanType can_t

printCanType :: Can.Type -> TC (Doc ann)
printCanType t = do return $ pretty t

printHeader :: Map.Map Ident.Identifier Type -> TC (Doc ann)
printHeader headers = do 
    lst <- mapM printPair $ toList headers
    return $ (align . vsep) lst
    where 
        printPair (i, t) = do 
            pt <- printType t
            return $ (pretty . show) i <+> colon <+> pt
