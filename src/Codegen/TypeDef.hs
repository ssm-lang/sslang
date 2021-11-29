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
                                                , ident, fromId
                                                )

import           Codegen.Identifiers
import           Data.Function                  ( on )
import           Data.List as A                 ( maximumBy
                                                , partition
                                                )
import Data.Map as M

data TypeDefInfo = TypeDefInfo
  { dconType :: M.Map DConId TConId 
  , typeSize :: M.Map TConId  Int
  , isPointer :: M.Map DConId  Bool
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
genTypeDef tconid (L.TypeDef dCons _) = ([tagEnum,structDef],info)
 where
  -- | Create C Definition for enum of ADT's tag values
  tagEnum = [cedecl| enum $id:enumName {
                          $enums:(enumElts)
                       };  
             |]
  -- | Create C defintion for C struct representing the ADT
  structDef = [cedecl| typedef struct {
                       $ty:ssm_mm_md $id:header;
                       $ty:ssm_value_t $id:payload[($ty:word_t ) $uint:maxNumFields]; 
                     } $id:tconid;     
                   |]
  -- | Find the size of the largest 'DCon
  maxNumFields =
    dConSize $ maximumBy (compare `on` dConSize) heapObjs
  -- | Define enum name as 'TConId ++ Tag 
  enumName = CIdent $ fromString $ ident tconid ++ "Tag"
  -- | Define contents of enum as a list of 'DConId s, starting with 0
  enumElts =
    [cenum|$id:(head dConTags) = 0|] : ((\n->[cenum|$id:n|]) <$> tail dConTags)

  -- | Distinguish heap objects from int objects, and return a list of tags
  (heapObjs,intObjs,dConTags) = analyze dCons
   where analyze :: [(DConId, L.TypeVariant L.Type)]
                 -> ([(DConId, L.TypeVariant L.Type)],[(DConId, L.TypeVariant L.Type)],[String])
         analyze dataCons = (big,small,tagList)
           where
            small = smallEnough 
            big = greater ++ notSmallEnough
            -- | Arrange data constructors from smallest to largest tag value
            tagList = ident.fst <$> (small ++ big)
            -- | Compare size of each data constructor to a word
            (greater, lessThan) = A.partition ((>= 32) . dConSize) dataCons
            -- | Will each data constructor still be smaller than a word after including tag bits?
            (notSmallEnough, smallEnough) = A.partition isSmallEnough lessThan
             where
                isSmallEnough = (< 32) . (tagBits + ) . dConSize
                tagBits = q + r where (q, r) = length lessThan `quotRem` 2       

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



  

  saveInfo :: [(DConId, L.TypeVariant L.Type)] -> [(DConId, L.TypeVariant L.Type)] -> TypeDefInfo
  saveInfo ptrs intgrs = TypeDefInfo mempty  mempty mempty mempty 
    where
       -- | Save whether each 'DCon is a pointer or integer
       isPtr = M.fromList $ intDCons ++ heapDCons
       intDCons = zip (fst<$> intgrs) (repeat False)
       heapDCons = zip (fst<$> ptrs) (repeat True)
       -- | Save extract tag func for each DCon
       readTagFuncs = M.fromList $ intTags ++ heapTags
       intTags = zip (fst<$> intgrs) (repeat intTagFunc)
       intTagFunc :: VarId -> C.Exp 
       intTagFunc v = [cexp| (($id:v >> $uint:(tagBits+1)) & (tagBits+1))|]
       tagBits = length intDCons
       heapTags = zip (fst<$> ptrs) (repeat ptrTagFunc)
       ptrTagFunc = \v -> [cexp|$id:v.$id:header.tag|]
       -- | save init func for each DCon
      --  initFuncs = M.fromList $ intInits ++ heapInits
      --  intInits = [] --fix later!
      --  heapInits = ptrInitFunc maxSize.fst <$> ptrs
      --  ptrInitFunc = \sz tagName v -> [cdecl|$ty:word_t $id:v = $id:ssm_new($id:tagName,$uint:sz);|]
      --  maxSize = dConSize $ maximumBy (compare `on` dConSize) ptrs




  -- -- | Save the tag bits of type 'TCon, if any
  -- tagBits = length intDCons
  -- tagVals =  M.fromList $ zip dConTags [0::Int,1..]
  -- tagExprs = initTag tagBits <$> (fst<$> intObjs) 
  -- initTag :: Int -> DConId -> C.Exp
  -- initTag bits nm = [cexp|$id:nm << $uint:bits|]

  -- -- | Save information about each data constructor in a lookup table
  
 
  -- -- | Save 'TCon for each 'DCon
  -- dConTyp =  M.fromList $ zip (fst <$> dCons) (repeat tconid)
 
  -- -- | Save tag of each 'DCon
  -- tagMap =  M.fromList $ tagVals
  -- | Save compelete lookup table
 -- info = TypeDefInfo dConTyp typSize isPtr tagMap
  info = TypeDefInfo mempty  mempty mempty mempty 


