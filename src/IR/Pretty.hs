{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Pretty () where

import Common.Pretty
import Control.Monad (void)
import IR.IR
import IR.Types.Type (
  Annotation,
  Annotations,
  Type,
  unfoldArrow,
  pattern Arrow,
 )


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
      pretty v <+> typSig <+> pretty "=" <+> line
        <> indent
          2
          (pretty (void body))
     where
      typSig = hsep args <+> rarrow <+> pretty retTy
      args =
        zipWith
          (\arg t -> parens $ pretty arg <+> pretty ":" <+> pretty t)
          argIds
          argTys
      (argIds, body) = unfoldLambda l
      (argTys, retTy) = unfoldArrow ty -- FIXME
    prettyFuncDef (v, e) = pretty v <+> pretty "=" <+> pretty (void e)

    prettyExternDecl :: (Pretty t) => (VarId, t) -> Doc ann
    prettyExternDecl (v, t) =
      pretty "extern" <+> pretty v <+> colon <+> pretty t

    -- Generates readable Doc representation of an IR Type Definition
    prettyTypDef :: (TConId, TypeDef) -> Doc ann
    prettyTypDef (tcon, TypeDef{variants = vars}) =
      pretty "type"
        <+> pretty tcon
        <+> line
        <> indent indentNo (vsep $ map prettyDCon vars)
        <> line
    prettyDCon :: (DConId, TypeVariant) -> Doc ann
    prettyDCon (dcon, VariantNamed argz) =
      pretty dcon <+> hsep (pretty . snd <$> argz)
    prettyDCon (dcon, VariantUnnamed argz) =
      pretty dcon <+> hsep (pretty <$> argz)


-- | Pretty prints IR Expr nodes without type annotations
instance Pretty (Expr ()) where
  pretty = undefined


{-
pretty a@App{} = pretty nm <+> hsep (parenz . fst <$> args)
 where
  (nm, args) = unfoldApp a
  -- insert (usually) necessary parens
  parenz :: Expr () -> Doc ann
  parenz v@(Var _ _) = pretty v  -- variables
  parenz l@(Lit _ _) = pretty l  -- literals
  parenz e           = parens (pretty e)
  -- TODO: minimum parens algo
pretty (Prim Wait es _           ) = pretty "wait" <+> vsep (map pretty es)
pretty (Var v _                  ) = pretty v
pretty (Lambda a              b _) = pretty "fun" <+> pretty a <+> pretty b
-- pretty (Let    [(Nothing, e)] b _) = pretty e <> line <> pretty b
pretty (Let    as             b _) = letexpr
 where
  letexpr = pretty "let" <+> vsep (map def as) <> line <> pretty b
  def (_binderId -> Just v, e) = pretty v <+> equals <+> align (pretty e)
  def (_, e) = pretty '_' <+> equals <+> align (pretty e)
pretty (Prim After [d, l, r] _) = ae
 where
  ae =
    pretty "after" <+> pretty d <> comma <+> pretty l <+> larrow <+> pretty r
pretty (Prim  Assign [l, r] _) = parens $ pretty l <+> larrow <+> pretty r
pretty (Match s      as     _) = pretty "match" <+> pretty s <> line <> arms
 where
  arms = vsep (map (indent indentNo . arm) as)
  arm :: (Alt t, Expr ()) -> Doc ann
  arm (a, e) = pretty a <+> equals <+> align (pretty e)
pretty (Prim Loop [b] _) =
  pretty "loop" <> line <> indent indentNo (pretty b)
pretty (Prim (PrimOp po) [l, r] _) = pretty l <+> pretty po <+> pretty r
pretty (Data d _                 ) = pretty d
pretty (Lit  l _                 ) = pretty l
pretty (Prim New [r] _           ) = pretty "new" <+> pretty r
pretty (Prim Dup [r] _           ) = pretty "__dup" <+> parens (pretty r)
pretty (Prim Drop [e, r] _) =
  pretty "__drop"
    <+> parens (line <> indent 2 (pretty e) <> line)
    <+> pretty r
pretty (Prim Deref [r] _) = pretty "deref" <+> parens (pretty r)
pretty (Prim Par   es  _) = pretty "par" <+> block dbar (map pretty es)
pretty (Prim Break []  _) = pretty "break"
pretty (Prim (CCall s) es _) =
  pretty "$" <> pretty s <> parens (hsep $ punctuate comma $ map pretty es)
pretty (Prim (FfiCall s) es _) = pretty s <+> hsep (map (parens . pretty) es)
pretty (Prim (CQuote  s) [] _) = pretty "$$" <> pretty s <> pretty "$$"

-- pretty (Prim Return [e] _        ) = pretty "return" <+> braces (pretty e)
pretty (Prim p _ _) = error "Primitive expression not well-formed: " $ show p
pretty (Exception et _       ) = pretty "__exception" <+> prettyet et
  where prettyet (ExceptDefault l) = pretty l
  -}

instance Pretty (Alt Type) where
  pretty = undefined


-- pretty (AltData a b       _ ) = pretty a <+> hsep (map pretty b)
-- pretty (AltLit    a       _ ) = pretty a
-- pretty (AltBinder (_binderId -> Just v)) = pretty v
-- pretty (AltBinder (_binderId -> Nothing)) = pretty '_'

instance Pretty Literal where
  pretty = undefined


-- pretty (LitIntegral i) = pretty $ show i
-- pretty LitEvent        = pretty "()"

instance Pretty PrimOp where
  pretty PrimNeg = pretty "-"
  pretty PrimNot = pretty "!"
  pretty PrimBitNot = pretty "~"
  pretty PrimAdd = pretty "+"
  pretty PrimSub = pretty "-"
  pretty PrimMul = pretty "*"
  pretty PrimDiv = pretty "/"
  pretty PrimMod = pretty "%"
  pretty PrimBitAnd = pretty "&"
  pretty PrimBitOr = pretty "|"
  pretty PrimEq = pretty "=="
  pretty PrimNeq = pretty "!="
  pretty PrimGt = pretty ">"
  pretty PrimGe = pretty ">="
  pretty PrimLt = pretty "<"
  pretty PrimLe = pretty "<="


instance Pretty (Binder Type) where
  pretty Binder{_binderId = Just v} = pretty v
  pretty Binder{_binderId = Nothing} = pretty '_'
