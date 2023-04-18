{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Pretty () where

import Common.Pretty
import Data.Char (GeneralCategory (..), generalCategory, isSpace)
import Data.List (dropWhileEnd)
import IR.IR
import IR.Types.Type (pattern Hole)


indents :: Doc ann -> Doc ann
indents = indent 2 . align


lineSep :: Doc ann -> Doc ann
lineSep = flatAlt line


hardlineSep :: Doc ann -> Doc ann
hardlineSep = const hardline


layoutBlock' :: Doc ann -> Doc ann
layoutBlock' d = group $ flatAlt indented oneLiner
 where
  oneLiner = " " <> d
  indented = line <> indents d


layoutBlock :: Doc ann -> Doc ann
layoutBlock d = flatAlt indented oneLiner
 where
  oneLiner = " { " <> d <> " }"
  indented = line <> indents d


hardBlock :: Doc ann -> Doc ann
hardBlock d = group $ flatAlt indented oneLiner
 where
  oneLiner = " " <> d
  indented = hardline <> indents d


hardvsep :: [Doc ann] -> Doc ann
hardvsep = align . concatWith (\x y -> x <> hardline <> y)


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
  pretty Program{programDefs, typeDefs, externDecls, cDefs} =
    vsep $
      punctuate line $
        concat
          [ cDefsChunk
          , map prettyTypDef typeDefs
          , map prettyExternDecl externDecls
          , map prettyDef programDefs
          ]
   where
    cDefsChunk
      | null $ dropWhile isSpace $ dropWhileEnd isSpace cDefs' = []
      | otherwise = ["$$$" <> hardline <> pretty cDefs' <> hardline <> "$$$"]
    cDefs' = dropWhile isLineBreak $ dropWhileEnd isLineBreak cDefs
    isLineBreak (generalCategory -> LineSeparator) = True
    isLineBreak (generalCategory -> ParagraphSeparator) = True
    isLineBreak '\n' = True
    isLineBreak '\r' = True
    isLineBreak _ = False

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


prettyDef :: (Binder Type, Expr Type) -> Doc ann
prettyDef (v, unfoldLambda -> ([], b)) = prettyBinderTyped False v <+> "=" <> hardBlock (pretty b)
prettyDef (v, unfoldLambda -> (as, b)) =
  let args = sep $ map (prettyBinderTyped True) as
      t = extract v
      typeInfo
        | hasTypeInfo t = ":" <+> pretty t
        | otherwise = ""
   in prettyBinderUntyped v <+> args <> typeInfo <+> "=" <> hardBlock (pretty b)


instance Pretty (Expr Type) where
  pretty = prettySeq


prettySeq :: Expr Type -> Doc ann
prettySeq (Let [(BindAnon _, e)] b _) = prettyStm e <> lineSep "; " <> prettySeq b
prettySeq (Let [d] b _) = "let" <+> prettyDef d <> hardlineSep "; " <> prettySeq b
prettySeq (Let ds b _) = "let" <+> hardvsep (map prettyDef ds) <> hardlineSep "; " <> prettySeq b
prettySeq e = prettyStm e


prettyStm :: Expr Type -> Doc ann
prettyStm (Prim After [d, l, r] _) = "after" <+> prettyOp d <> "," <+> prettyOp l <+> "<-" <+> hardBlock (prettyOp r)
prettyStm (Prim Assign [l, r] _) = prettyOp l <+> "<-" <+> hardBlock (prettyOp r)
prettyStm e = prettyOp e


prettyOp :: Expr Type -> Doc ann
-- TODO: fix precedence and associativity here
prettyOp (Prim (PrimOp po) [o] _) = pretty po <+> prettyOp o
prettyOp (Prim (PrimOp po) [l, r] _) = prettyOp l <+> pretty po <+> prettyOp r
prettyOp e = prettyBlk e


prettyBlk :: Expr Type -> Doc ann
prettyBlk (Match s as _) =
  "match" <+> prettyOp s <> layoutBlock (vsep $ punctuate (lineSep " | ") $ map prettyArm as)
prettyBlk e@Lambda{} =
  let (as, b) = unfoldLambda e
      args = hsep $ map (prettyBinderTyped True) as
   in "fun" <+> args <> layoutBlock (pretty b)
prettyBlk (Prim Loop [b] _) = "loop" <> layoutBlock (pretty b)
prettyBlk (Prim Wait [e] _) =
  "wait" <+> prettyApp e
prettyBlk (Prim Wait es _) =
  "wait" <> layoutBlock (vsep $ punctuate (lineSep " || ") $ map pretty es)
prettyBlk (Prim Par [e] _) =
  "par" <+> prettyApp e
prettyBlk (Prim Par es _) =
  "par" <> layoutBlock (vsep $ punctuate (lineSep " || ") $ map pretty es)
prettyBlk (Prim Drop [e, r] _) =
  "%do" <> layoutBlock (pretty e) <> line <> "%dropping" <+> prettyAtom r
prettyBlk (Prim (CCall s) es _) = "$" <> pretty s <> parens (hsep $ punctuate ", " $ map pretty es)
prettyBlk e = prettyApp e


prettyArm :: (Alt Type, Expr Type) -> Doc ann
prettyArm (a, e) = pretty a <+> "=" <> group (layoutBlock (pretty e))


prettyApp :: Expr Type -> Doc ann
prettyApp (Prim (FfiCall s) es _) = pretty s <+> hsep (map prettyAtom es)
prettyApp (Prim New [e] _) = "new" <+> prettyAtom e
prettyApp (Prim Deref [e] _) = "deref" <+> prettyAtom e
prettyApp (Prim Dup [r] _) = "%dup" <+> prettyAtom r
prettyApp (Exception et _) = "%exception" <+> pretty et
prettyApp e@App{} =
  let (f, map fst -> as) = unfoldApp e
   in prettyAtom f <> layoutBlock' (vsep $ map prettyAtom as)
prettyApp e = prettyAtom e


prettyAtom :: Expr Type -> Doc ann
prettyAtom (Var v _) = pretty v
prettyAtom (Lit l _) = pretty l
prettyAtom (Data d _) = pretty d
prettyAtom (Prim Break _ _) = "break"
prettyAtom (Prim (CQuote c) _ _) = "$$" <> pretty c <> "$$"
prettyAtom e = parens $ pretty e


instance Pretty ExceptType where
  pretty (ExceptDefault l) = pretty l


instance Pretty (Alt Type) where
  pretty (AltData d [] _) = pretty d
  pretty (AltData d as _) = parens $ pretty d <+> hsep (map pretty as)
  pretty (AltLit a _) = pretty a
  pretty (AltBinder b) = prettyBinderUntyped b


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


-- | Pretty-print a binder, never with type information.
prettyBinderUntyped :: Show a => Binder a -> Doc ann
prettyBinderUntyped (BindVar v _) = pretty v
prettyBinderUntyped (BindAnon _) = "_"


{- | Pretty-print a binder.

 Only print type info if it's useful (not just a hole).
-}
prettyBinderTyped :: Bool -> Binder Type -> Doc ann
prettyBinderTyped needsParen (BindVar v t)
  | hasTypeInfo t = (if needsParen then parens else id) $ pretty v <> ":" <+> pretty t
  | otherwise = pretty v
prettyBinderTyped needsParen (BindAnon t)
  | hasTypeInfo t = (if needsParen then parens else id) $ "_:" <+> pretty t
  | otherwise = "_"


-- | Whether a 'Type' has any information worth printing out.
hasTypeInfo :: Type -> Bool
hasTypeInfo Hole = False
hasTypeInfo _ = True
