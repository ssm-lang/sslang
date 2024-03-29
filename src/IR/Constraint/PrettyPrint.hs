module IR.Constraint.PrettyPrint (
  printConstraint,
  Doc,
) where

import qualified Common.Identifiers as Ident
import qualified IR.Constraint.Canonical as Can
import IR.Constraint.Type as Typ (
  Constraint (
    CAnd,
    CEqual,
    CForeign,
    CLet,
    CLocal,
    CPattern,
    CSaveTheEnvironment,
    CTrue
  ),
  Type (..),
  Variable,
  toCanType,
 )

import Prettyprinter

import Data.Map as Map (Map, null, toList)

import IR.Constraint.Monad (TC)


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

indentation :: Doc ann -> Doc ann
indentation = indent 2


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
  return $ pretty "local" <+> (align . hsep) [(pretty . show) i, pretty "<:", p]
printConstraint (CForeign (Can.Forall vars ct) t) = do
  pct <- printCanType ct
  let scheme = pretty "forall" <+> (pretty . show . toList) vars <+> dot <+> pct

  p <- printType t
  return $ pretty "foreign" <+> (align . hsep) [scheme, pretty "<:", p]
printConstraint (CAnd lst) = do
  printed <- mapM printConstraint lst
  let joined = vsep printed
  return joined
printConstraint (CLet _ [v] h (CAnd ((CEqual (TVarN v') _) : xs)) CTrue) | Map.null h && v == v' = do
  printConstraint (CAnd xs)
printConstraint (CLet r f h hc CTrue) | Map.null h = do
  pr <- mapM printVar r
  pf <- mapM printVar f

  phc <- printConstraint hc

  return $
    (align . vsep)
      [ pretty "exists" <+> list (pf ++ pr)
      , indentation
          ( align phc
          )
      ]
printConstraint (CLet [] [] h CTrue bc) = do
  pheader <- printHeader h
  pbc <- printConstraint bc

  return $
    (align . vsep)
      [ pretty "def"
      , indentation
          ( align pheader
          )
      , pretty "in"
      , indentation pbc
      ]
printConstraint (CLet r f o (CLet [] [] i CTrue innerbc) bc) | o == i = do
  pr <- mapM printVar r
  pf <- mapM printVar f

  pheader <- printHeader o

  pbc <- printConstraint bc

  pinnerbc <- printConstraint innerbc

  return $
    (align . vsep)
      [ pretty "let rec" <+> list (pf ++ pr)
      , indentation
          ( align pheader
          )
      , pretty "given"
      , indentation pinnerbc
      , pretty "in"
      , indentation pbc
      ]
printConstraint (CLet r f h hc bc) = do
  pr <- mapM printVar r
  pf <- mapM printVar f

  pheader <- printHeader h

  phc <- printConstraint hc
  pbc <- printConstraint bc

  return $
    (align . vsep)
      [ pretty "let" <+> list (pf ++ pr)
      , indentation
          ( align pheader
          )
      , pretty "given"
      , indentation
          ( align phc
          )
      , pretty "in"
      , indentation pbc
      ]


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
