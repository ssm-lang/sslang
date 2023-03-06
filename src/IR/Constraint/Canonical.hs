{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module IR.Constraint.Canonical
  ( Type(..)
  , Scheme(..)
  , FreeVars
  , Annotation(..)
  , Annotations(..)
  , Kind
  , schemeOf
  , freeVars
  , foldArrow
  , unfoldArrow
  , (-->)
  , pattern Unit
  , pattern Ref
  , pattern List
  , pattern Time
  , pattern I64
  , pattern U64
  , pattern I32
  , pattern U32
  , pattern I16
  , pattern U16
  , pattern I8
  , pattern U8
  , tuple
  , unAnnotations
  , annToType
  , builtinKinds
  ) where

import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified IR.IR                         as I
import           IR.Types.Type                  ( Annotation(..)
                                                , Annotations(..)
                                                , Type(..)
                                                , builtinKinds
                                                , foldArrow
                                                , tupleId
                                                , unAnnotations
                                                , unfoldArrow
                                                )


-- | SCHEMES

data Scheme = Forall FreeVars I.Type

type FreeVars = Map.Map Ident.TVarId ()

-- | Construct a scheme from all free type variables and a trivial constraint.
schemeOf :: I.Type -> Scheme
schemeOf t = Forall (freeVars t) t

freeVars :: I.Type -> FreeVars
freeVars t = Map.fromList . map (, ()) . Set.toList $ Ident.freeVars t

-- | KINDS

-- no support for higher-kinded stuff yet, so Int suffices
type Kind = Int


-- | HELPERS

infixr 0 -->
(-->) :: I.Type -> I.Type -> I.Type
(-->) t1 t2 = TCon "->" [t1, t2]

-- | The builtin singleton 'Type', whose only data constructor is just @()@.
pattern Unit :: Type
pattern Unit = TCon "()" []

-- | The builtin reference 'Type', created using @new@.
pattern Ref :: Type -> Type
pattern Ref a = TCon "&" [a]

-- | The builtin list 'Type', created using list syntax, e.g., @[a, b]@.
pattern List :: Type -> Type
pattern List a = TCon "[]" [a]

-- | The builtin 64-bit timestamp 'Type'.
pattern Time :: Type
pattern Time = TCon "Time" []

-- | Builtin 'Type' for signed 64-bit integers.
pattern I64 :: Type
pattern I64 = TCon "Int64" []

-- | Builtin 'Type' for unsigned 64-bit integers.
pattern U64 :: Type
pattern U64 = TCon "UInt64" []

-- | Builtin 'Type' for signed 32-bit integers.
pattern I32 :: Type
pattern I32 = TCon "Int32" []

-- | Builtin 'Type' for unsigned 32-bit integers.
pattern U32 :: Type
pattern U32 = TCon "UInt32" []

-- | Builtin 'Type' for signed 16-bit integers.
pattern I16 :: Type
pattern I16 = TCon "Int16" []

-- | Builtin 'Type' for unsigned 16-bit integers.
pattern U16 :: Type
pattern U16 = TCon "UInt16" []

-- | Builtin 'Type' for signed 8-bit integers.
pattern I8 :: Type
pattern I8 = TCon "Int8" []

-- | Builtin 'Type' for unsigned 8-bit integers.
pattern U8 :: Type
pattern U8 = TCon "UInt8" []

-- | Construct a builtin tuple type out of a list of at least 2 types.
tuple :: [Type] -> Type
tuple ts = TCon (tupleId $ length ts) ts


-- | ANNOTATION

annToType :: Annotation -> Type
annToType ann = case ann of
  AnnDCon _ _     -> error "No need for AnnDCon anymore."
  AnnType canType -> canType
  AnnArrows paramAnns retAnn ->
    let paramCanTypes = map annToType paramAnns
        retCanType    = annToType retAnn
    in  foldArrow (paramCanTypes, retCanType)
