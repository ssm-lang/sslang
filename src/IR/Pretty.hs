{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Pretty () where

import Common.Pretty
import IR.IR
import IR.Types.Type (unfoldArrow)


{- | Pretty Typeclass: pretty print the IR

Adds
* indentation and line breaks
* some parens (not minimal parens, but fewer than around every node)
Omits
* let _ =
* type annotations
Reverts
* curried funcs of one arg back to multiple arg funcs
-}
instance Pretty (Program Type) where
  pretty Program{programDefs = ds, typeDefs = tys, externDecls = xds} =
    vsep $ punctuate line tops
   where
    tops =
      map prettyTypDef tys ++ map prettyExternDecl xds ++ map prettyFuncDef ds

    -- Generates readable Doc representation of an IR Top Level Function
    prettyFuncDef :: (VarId, Expr Type) -> Doc ann
    prettyFuncDef (v, l@(Lambda _ _ ty)) =
      pretty v <+> typSig <+> "=" <+> line <> indent 2 (pretty body)
     where
      typSig = hsep args <+> rarrow <+> pretty retTy
      args = zipWith (\arg t -> parens $ pretty arg <+> ":" <+> pretty t) argIds argTys
      (argIds, body) = unfoldLambda l
      (argTys, retTy) = unfoldArrow ty -- FIXME
    prettyFuncDef (v, e) = pretty v <+> "=" <+> pretty e

    prettyExternDecl :: (Pretty t) => (VarId, t) -> Doc ann
    prettyExternDecl (v, t) = "extern" <+> pretty v <+> colon <+> pretty t

    -- Generates readable Doc representation of an IR Type Definition
    prettyTypDef :: (TConId, TypeDef) -> Doc ann
    prettyTypDef (tcon, TypeDef{variants = vars}) =
      "type" <+> pretty tcon <+> line <> indent indentNo (vsep $ map prettyDCon vars) <> line
    prettyDCon :: (DConId, TypeVariant) -> Doc ann
    prettyDCon (dcon, VariantNamed argz) =
      pretty dcon <+> hsep (pretty . snd <$> argz)
    prettyDCon (dcon, VariantUnnamed argz) =
      pretty dcon <+> hsep (pretty <$> argz)


indents :: Doc ann -> Doc ann
indents = indent 2


lineSep :: Doc ann -> Doc ann
lineSep = flatAlt line


altBlock :: Doc ann -> Doc ann
altBlock d = indents $ flatAlt "" "{ " <> align d <> flatAlt "" " }"


groupLine :: Doc ann -> Doc ann
groupLine d = group $ align d


-- | Pretty prints IR Expr nodes without type annotations
instance Pretty (Expr Type) where
  pretty = prettySeq


prettyDef :: (Binder Type, Expr Type) -> Doc ann
prettyDef (v, e) = pretty v <+> "=" <+> groupLine (pretty e)


prettySeq :: Expr Type -> Doc ann
prettySeq (Let [(BindAnon _, e)] b _) = prettyStm e <> lineSep "; " <> prettySeq b
prettySeq (Let [d] b _) = "let" <+> prettyDef d <> lineSep "; " <> prettySeq b
prettySeq (Let ds b _) = "let" <+> align (vsep $ map prettyDef ds) <> lineSep "; " <> prettySeq b
prettySeq e = prettyStm e


prettyStm :: Expr Type -> Doc ann
prettyStm (Prim After [d, l, r] _) = "after" <+> prettyOp d <> "," <+> prettyOp l <+> "<-" <+> groupLine (prettyOp r)
prettyStm (Prim Assign [l, r] _) = prettyOp l <+> "<-" <+> groupLine (prettyOp r)
prettyStm e = prettyOp e


prettyOp :: Expr Type -> Doc ann
-- TODO: fix precedence and associativity here
prettyOp (Prim (PrimOp po) [o] _) = pretty po <+> prettyOp o
prettyOp (Prim (PrimOp po) [l, r] _) = prettyOp l <+> pretty po <+> prettyOp r
prettyOp e = prettyBlk e


prettyBlk :: Expr Type -> Doc ann
prettyBlk (Match s as _) =
  groupLine $
    "match" <> prettyOp s <> lineSep " "
      <> altBlock (vsep $ punctuate (lineSep " | ") $ map prettyArm as)
prettyBlk (Lambda a b _) =
  -- TODO: uncurry?
  "fun" <+> pretty a <> lineSep " " <> altBlock (pretty b)
prettyBlk (Prim Loop [b] _) = "loop" <> lineSep " " <> altBlock (pretty b)
prettyBlk (Prim Wait es _) =
  groupLine $
    "wait" <> lineSep " "
      <> altBlock (vsep $ punctuate (lineSep " || ") $ map pretty es)
prettyBlk (Prim Par es _) =
  groupLine $
    "par" <> lineSep " "
      <> altBlock (vsep $ punctuate (lineSep " || ") $ map pretty es)
prettyBlk (Prim (CCall s) es _) = "$" <> pretty s <> parens (hsep $ punctuate ", " $ map pretty es)
prettyBlk e = prettyApp e


prettyArm :: (Alt Type, Expr Type) -> Doc ann
prettyArm (a, e) = pretty a <+> "=" <+> groupLine (pretty e)


prettyApp :: Expr Type -> Doc ann
prettyApp (Prim (FfiCall s) es _) = pretty s <+> hsep (map prettyAtom es)
prettyApp (Prim New [e] _) = "new" <+> prettyAtom e
prettyApp (Prim Deref [e] _) = "deref" <+> prettyAtom e
prettyApp (Prim Dup [r] _) = "%dup" <+> prettyAtom r
prettyApp (Prim Drop [e, r] _) = group $ "%do" <+> flatAlt "{ " "{" <> line <> indents (prettyAtom e) <> line <> flatAlt " }" "}" <+> "%dropping" <+> prettyAtom r
prettyApp (Exception et _) = "%exception" <+> pretty et
prettyApp e@App{} =
  let (f, map fst -> as) = unfoldApp e
   in prettyAtom f <+> hsep (map prettyAtom as)
prettyApp e = prettyAtom e


prettyAtom :: Expr Type -> Doc ann
prettyAtom (Var v _) = pretty v
prettyAtom (Lit l _) = pretty l
prettyAtom (Data d _) = pretty d
prettyAtom (Prim Break _ _) = "break"
prettyAtom e = parens $ pretty e


instance Pretty ExceptType where
  pretty (ExceptDefault l) = pretty l


instance Pretty (Alt Type) where
  pretty (AltData d [] _) = pretty d
  pretty (AltData d as _) = parens $ pretty d <+> hsep (map pretty as)
  pretty (AltLit a _) = pretty a
  pretty (AltBinder b) = pretty b


instance Pretty Literal where
  pretty (LitIntegral i) = pretty $ show i
  pretty LitEvent = "()"


instance Pretty PrimOp where
  pretty PrimNeg = "-"
  pretty PrimNot = "!"
  pretty PrimBitNot = "~"
  pretty PrimAdd = "+"
  pretty PrimSub = "-"
  pretty PrimMul = "*"
  pretty PrimDiv = "/"
  pretty PrimMod = "%"
  pretty PrimBitAnd = "&"
  pretty PrimBitOr = "|"
  pretty PrimEq = "=="
  pretty PrimNeq = "!="
  pretty PrimGt = ">"
  pretty PrimGe = ">="
  pretty PrimLt = "<"
  pretty PrimLe = "<="


instance Pretty (Binder Type) where
  pretty (BindVar v t) = parens $ pretty v <> ":" <+> pretty t
  pretty (BindAnon t) = parens $ "_:" <+> pretty t
  pretty _ = error "Non-exhaustive Pretty (Binder Type)"
