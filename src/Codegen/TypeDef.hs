{-# LANGUAGE QuasiQuotes #-}
module Codegen.TypeDef where

import qualified IR.Types.Flat                 as L
import qualified IR.Types.TypeSystem           as L

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           Common.Identifiers             ( DConId
                                                , TConId
                                                , VarId
                                                , fromString
                                                , ident
                                                )

import           Codegen.Identifiers
import           Data.Function                  ( on )
import           Data.List                      ( maximumBy
                                                , partition
                                                )
import qualified Data.Map                      as M

data TypeDefInfo = TypeDefInfo
  { dconType  :: M.Map DConId TConId
  , typeSize  :: M.Map TConId Int
  , isPointer :: M.Map DConId Bool
  , tag       :: M.Map DConId (VarId -> C.Exp)
  , intInit   :: M.Map DConId C.Exp
  }

instance Semigroup TypeDefInfo where
  (<>) = combineTypeDefInfo

instance Monoid TypeDefInfo where
  mempty = TypeDefInfo empty empty empty empty empty

combineTypeDefInfo :: TypeDefInfo -> TypeDefInfo -> TypeDefInfo
combineTypeDefInfo a b = TypeDefInfo typ sz isPtr tags intInits
 where
  typ      = dconType a `M.union` dconType b
  sz       = typeSize a `M.union` typeSize b
  isPtr    = isPointer a `M.union` isPointer b
  tags     = tag a `M.union` tag b
  intInits = intInit a `M.union` intInit b

-- | Generate definitions for SSM type definitions.
genTypeDef :: TConId -> L.TypeDef L.Type -> ([C.Definition], TypeDefInfo)
genTypeDef tconid (L.TypeDef dCons _) = ([tagEnum, structDef], info)
 where
  -- | Create C Definition for enum of ADT's tag values
  tagEnum = [cedecl| enum $id:enumName {
                          $enums:(enumElts)
                       };  
             |]
  -- | Create C Defintion for C struct representing the ADT
  structDef = [cedecl| typedef struct {
                       $ty:ssm_mm_md $id:header;
                       $ty:ssm_value_t $id:payload[($ty:word_t ) $uint:maxNumFields]; 
                     } $id:tconid;     
                   |]
  -- | Find the size of the largest 'DCon
  maxNumFields = dConSize $ maximumBy (compare `on` dConSize) heapObjs
  -- | Define enum name as 'TConId ++ Tag 
  enumName     = CIdent $ fromString $ ident tconid ++ "Tag"
  -- | Define contents of enum as a list of 'DConId s, starting with 0
  enumElts =
    [cenum|$id:(head dConTags) = 0|] : ((\n -> [cenum|$id:n|]) <$> tail dConTags)

  -- | Distinguish heap objects from int objects, and return a list of tags
  (heapObjs, intObjs, dConTags) = analyze dCons
   where
    analyze
      :: [(DConId, L.TypeVariant L.Type)]
      -> ( [(DConId, L.TypeVariant L.Type)]
         , [(DConId, L.TypeVariant L.Type)]
         , [String]
         )
    analyze dataCons = (big, small, tagList)
     where
      small                         = smallEnough
      big                           = greater ++ notSmallEnough
      -- | Arrange data constructors from smallest to largest tag value
      tagList                       = ident . fst <$> (small ++ big)
      -- | Compare size of each data constructor to a word
      (greater       , lessThan   ) = A.partition ((>= 32) . dConSize) dataCons
      -- | Will each data constructor still be smaller than a word after including tag bits?
      (notSmallEnough, smallEnough) = A.partition isSmallEnough lessThan
       where
        isSmallEnough = (< 32) . (tagBits +) . dConSize
        tagBits       = q + r where (q, r) = length lessThan `quotRem` 2

  -- | Return the size in bits of a given data constructor
  dConSize :: (DConId, L.TypeVariant L.Type) -> Int
  dConSize (_, L.VariantNamed fields  ) = sum $ fieldSize . snd <$> fields
  dConSize (_, L.VariantUnnamed fields) = sum $ fieldSize <$> fields

  {- | Return the size in bits of a given field 
  (assume all built-in types are one word for now)
  -}
  fieldSize :: L.Type -> Int
  fieldSize (L.TBuiltin _) = 32
  fieldSize (L.TCon     _) = 32

  -- | Save information about the ADT
  info = saveInfo (tconid, maxNumFields) heapObjs intObjs
   where
    saveInfo
      :: (TConId, Int)
      -> [(DConId, L.TypeVariant L.Type)]
      -> [(DConId, L.TypeVariant L.Type)]
      -> TypeDefInfo
    saveInfo (typ, sz) ptrs intgrs = TypeDefInfo
      { dconType  = typs
      , typeSize  = M.fromList [(typ, sz)]
      , isPointer = isPtr
      , tag       = readTagFuncs
      , intInit   = initVals
      }
     where
      -- | Save ('DCon,'TCon) as key-value pairs
      typs         = M.fromList $ zip (fst <$> intgrs) (repeat typ)
      -- | Save whether each 'DCon is a pointer or integer
      isPtr        = M.fromList $ intDCons ++ heapDCons
      intDCons     = zip (fst <$> intgrs) (repeat False)
      heapDCons    = zip (fst <$> ptrs) (repeat True)
      -- | Save read tag func for each 'DCon
      readTagFuncs = M.fromList $ intTagFuncs ++ heapTagFuncs
      intTagFuncs  = zip (fst <$> intgrs) (repeat intTagFunc)
       where
        intTagFunc :: VarId -> C.Exp
        intTagFunc v = [cexp| (($id:v >> $uint:(tagBits+1)) & (tagBits+1))|]
        tagBits = length intDCons
      heapTagFuncs = zip (fst <$> ptrs) (repeat ptrTagFunc)
       where
        ptrTagFunc :: VarId -> C.Exp
        ptrTagFunc v = [cexp|$id:v.$id:header.tag|]
      -- | Save initialization for each integer 'DCon (assume 0 fields for now)
      initVals    = M.fromList intInitVals
      intInitVals = intInitVal . fst <$> intgrs
       where
        intInitVal :: DConId -> (DConId, C.Exp)
        intInitVal tg = (tg, [cexp|(($id:tg << 1)&0x1)|])
