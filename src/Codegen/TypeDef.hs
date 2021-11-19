{-# LANGUAGE QuasiQuotes #-}
module Codegen.TypeDef where

import qualified IR.Types.Flat                 as L
import qualified IR.Types.TypeSystem           as L

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           Common.Identifiers             ( DConId
                                                , TConId
                                                , fromString
                                                , ident
                                                )

import           Codegen.Identifiers
import           Data.Function                  ( on )
import           Data.List as A                 ( maximumBy
                                                , partition
                                                )
import Data.Map as M

data TypeDefInfo = TypeDefInfo
  { dconType :: M.Map String String
  , typeSize :: M.Map String Int
  , isPointer :: M.Map String Bool
  , tag :: M.Map String Int
  }

instance Semigroup TypeDefInfo where
  a <> b = combineTypeDefInfo a b

instance Monoid TypeDefInfo where
  mempty = TypeDefInfo empty empty empty empty

combineTypeDefInfo :: TypeDefInfo -> TypeDefInfo -> TypeDefInfo
combineTypeDefInfo a b = TypeDefInfo typ sz isPtr tags 
 where
   typ = dconType a `M.union` dconType b
   sz = typeSize a `M.union` typeSize b
   isPtr = isPointer a `M.union` isPointer b
   tags = tag a `M.union` tag b

-- | Generate definitions for SSM type definitions.
genTypeDef :: TConId -> L.TypeDef L.Type -> ([C.Definition], TypeDefInfo)
genTypeDef tconid (L.TypeDef dCons _) = ([tags,structDef],info)
 where
  {- | Return the size in bits of a given field 
  (assume all built-in types are one word for now)
  -}
  fieldSize :: L.Type -> Int
  fieldSize (L.TBuiltin _) = 32
  fieldSize (L.TCon     _) = 32

  -- | Return the size in bits of a given data constructor
  dConSize :: (DConId, L.TypeVariant L.Type) -> Int
  dConSize (_, L.VariantNamed fields  ) = sum $ fieldSize . snd <$> fields
  dConSize (_, L.VariantUnnamed fields) = sum $ fieldSize <$> fields

  -- | Distinguish heap objects from possible int objects
  (heapObjs, possibleIntObjs) = A.partition ((>= 32) . dConSize) dCons

  -- | Incorporate tag, then partition list based on whether each 'DCon is still < 32 bits 
  smallEnough
    :: [(DConId, L.TypeVariant L.Type)]
    -> ([(DConId, L.TypeVariant L.Type)], [(DConId, L.TypeVariant L.Type)])
  smallEnough candidates = A.partition ((< 32) . dataBitsPlus tagBits) candidates
   where
    dataBitsPlus :: Int -> (DConId, L.TypeVariant L.Type) -> Int
    dataBitsPlus bits dcon = bits + dConSize dcon
    tagBits = q + r where (q, r) = length candidates `quotRem` 2

  -- | Idenfity int objects by considering the space needed for tag bits
  (moreHeapObs, intObjs) = smallEnough possibleIntObjs

  -- | Arrange data constructors from smallest to largest tag value
  tagged                 = intObjs ++ moreHeapObs ++ heapObjs

  -- | Define enum name as 'TConId ++ Tag 
  enumName               = CIdent $ fromString $ ident tconid ++ "Tag"

  -- | Turn 'DConId into an elt of a c enumerated type
  toCEnum :: DConId -> C.CEnum
  toCEnum n = [cenum|$id:n|]

  -- | Define contents of enum as a list of 'DConId s, starting with 0
  enumElts =
    [cenum|$id:(fst $ head tagged) = 0|] : (toCEnum . fst <$> tail tagged)

  -- | Create c Definition for c enum of ADT's tag values
  tags = [cedecl| enum $id:enumName {
                          $enums:(enumElts)
                       };  
             |]

  -- | Find the size of the largest 'DCon
  maxNumFields =
    dConSize $ maximumBy (compare `on` dConSize) (heapObjs ++ moreHeapObs)

  -- | Create c defintion for c struct representing the ADT
  structDef = [cedecl| typedef struct {
                       $ty:ssm_mm_md $id:header;
                       $ty:ssm_object_t $id:payload[($ty:word_t ) $uint:maxNumFields]; 
                     } $id:tconid;     
                   |]
  -- | save information about each data constructor in a lookup table
  theInts = zip (ident.fst<$> intObjs) (repeat False)
  thePtrs = zip (ident.fst<$> moreHeapObs ++ heapObjs) (repeat True)
  -- | save corresponding 'TCon for each 'DCon
  dConTyp =  M.fromList $ zip (ident.fst <$> dCons) (repeat (ident tconid))
  -- | save the size of type 'TCon
  typSize = M.fromList [(ident tconid,maxNumFields)]
  -- | save whether each 'DCon is a pointer or integer
  isPtr = M.fromList $ theInts ++ thePtrs
  -- } save tag of each 'DCon
  tagMap =  M.fromList $ zip (ident.fst <$> tagged) [0::Int,1..]
  -- | save compelete lookup table
  info = TypeDefInfo dConTyp typSize isPtr tagMap


