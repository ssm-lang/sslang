{-# LANGUAGE QuasiQuotes #-}

module Codegen.TypeDef where

import Codegen.Identifiers
import Common.Identifiers
  ( DConId,
    TConId,
    ident,
  )
import Data.Function (on)
import Data.List
  ( maximumBy,
    partition,
  )
import qualified Data.Map as M
import qualified IR.Types.Flat as L
import qualified IR.Types.TypeSystem as L
import Language.C.Quote.GCC
import qualified Language.C.Syntax as C

data TypeDefInfo = TypeDefInfo
  { dconType :: M.Map DConId TConId,
    typeSize :: M.Map TConId Int,
    isPointer :: M.Map DConId Bool,
    tag :: M.Map TConId (C.Exp -> C.Exp),
    intInit :: M.Map DConId C.Exp,
    ptrFields :: M.Map DConId (C.Exp -> Int -> C.Exp)
  }

instance Semigroup TypeDefInfo where
  (<>) = combineTypeDefInfo

instance Monoid TypeDefInfo where
  mempty = TypeDefInfo M.empty M.empty M.empty M.empty M.empty M.empty

combineTypeDefInfo :: TypeDefInfo -> TypeDefInfo -> TypeDefInfo
combineTypeDefInfo a b = TypeDefInfo typ sz isPtr tags intInits fieldz
  where
    typ = dconType a `M.union` dconType b
    sz = typeSize a `M.union` typeSize b
    isPtr = isPointer a `M.union` isPointer b
    tags = tag a `M.union` tag b
    intInits = intInit a `M.union` intInit b
    fieldz = ptrFields a `M.union` ptrFields b

-- | Generate definitions for SSM type definitions.
genTypeDef :: (TConId, L.TypeDef L.Type) -> ([C.Definition], TypeDefInfo)
genTypeDef (tconid, L.TypeDef dCons _) = ([tagEnum], info)
  where
    tagEnum =
      [cedecl| enum $id:tconid {
                          $enums:(enumElts)
                       };  
             |]

    enumElts =
      [cenum|$id:(head dConTags) = 0|] :
      ((\n -> [cenum|$id:n|]) <$> tail dConTags)

    (heapObjs, intObjs, dConTags) = analyze dCons
      where
        analyze ::
          [(DConId, L.TypeVariant L.Type)] ->
          ( [(DConId, L.TypeVariant L.Type)],
            [(DConId, L.TypeVariant L.Type)],
            [String]
          )
        analyze dataCons = (big, small, tagList)
          where
            big = greater ++ notSmallEnough
            small = smallEnough
            tagList = ident . fst <$> (small ++ big)

            (greater, lessThan) = partition ((>= 32) . dConSize) dataCons

            (smallEnough, notSmallEnough) = partition isSmallEnough lessThan
              where
                isSmallEnough = (< 32) . (tagBits +) . dConSize
                tagBits = q + r where (q, r) = length lessThan `quotRem` 2

    info = saveInfo tconid heapObjs intObjs
      where
        saveInfo ::
          TConId ->
          [(DConId, L.TypeVariant L.Type)] ->
          [(DConId, L.TypeVariant L.Type)] ->
          TypeDefInfo
        saveInfo typ ptrs intgrs =
          TypeDefInfo
            { dconType = typs,
              typeSize = typsz,
              isPointer = isPtr,
              tag = readTagFuncs,
              intInit = initVals,
              ptrFields = pFields
            }
          where
            typs = M.fromList $ zip (fst <$> intgrs ++ ptrs) (repeat typ)

            typsz = M.fromList [(typ, sz)]
            sz = dConSize $ maximumBy (compare `on` dConSize) heapObjs

            isPtr = M.fromList $ intDCons ++ heapDCons
            intDCons = zip (fst <$> intgrs) (repeat False)
            heapDCons = zip (fst <$> ptrs) (repeat True)

            readTagFuncs = M.fromList [(tconid, readTagFunc ptrs intgrs)]
            readTagFunc ::
              [(DConId, L.TypeVariant L.Type)] ->
              [(DConId, L.TypeVariant L.Type)] ->
              C.Exp ->
              C.Exp
            readTagFunc ps ints val
              | null ps = intTagFunc val -- always integer
              | null ints = ptrTagFunc val -- always heap object
              | otherwise = tagFunc (isInt val) (intTagFunc val) (ptrTagFunc val)
              where
                intTagFunc :: C.Exp -> C.Exp
                intTagFunc v = [cexp| (($exp:v >> $uint:(tagBits+1)) & (tagBits+1))|]
                  where
                    tagBits = length intDCons
                ptrTagFunc :: C.Exp -> C.Exp
                ptrTagFunc v = [cexp|($exp:v.$id:mm.tag)|]
                isInt v = [cexp| (($exp:v) & 0x1 == 0x1) |]
                tagFunc cond a b = [cexp| (($exp:cond) ? ($exp:a) : ($exp:b)) |]
            initVals = M.fromList intInitVals
            intInitVals = intInitVal . fst <$> intgrs
              where
                intInitVal :: DConId -> (DConId, C.Exp)
                intInitVal tg = (tg, [cexp|(($id:tg << 1)&0x1)|])

            pFields = M.fromList $ zip (fst <$> intgrs) (repeat accessField)
              where
                accessField :: (C.Exp -> Int -> C.Exp)
                accessField nm index =
                  [cexp|$exp:(ssm_to_obj nm)->$id:payload[$uint:index] |]

    dConSize :: (DConId, L.TypeVariant L.Type) -> Int
    dConSize (_, L.VariantNamed fields) = sum $ fieldSize . snd <$> fields
    dConSize (_, L.VariantUnnamed fields) = sum $ fieldSize <$> fields
    fieldSize :: L.Type -> Int
    fieldSize (L.TBuiltin _) = 32
    fieldSize (L.TCon _) = 32
