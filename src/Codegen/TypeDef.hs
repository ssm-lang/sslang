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
import           Data.Char                      ( toLower )
import           Data.List                      ( maximumBy
                                                , partition
                                                )

-- | Generate definitions for SSM type definitions.
genTypeDef :: TConId -> L.TypeDef L.Type -> (C.Definition, C.Definition)
genTypeDef tconid (L.TypeDef dCons _) = (tags, structDef)
 where
  structDef = [cedecl| typedef struct {
                       $ty:ssm_mm_md* $id:header;
                       $ty:ssm_object_t* $id:payload[($ty:word_t ) $uint:maxNumFields]; 
                     } $id:tconid;     
                   |]
  tags = [cedecl| enum $id:enumName {
                          $enums:(enumElts)
                       };  
             |]

  -- | find any data constructors that hold less than 32 bits of data
  (big, small ) = partition (\(_, sz) -> sz >= 32) $ augmentWSize <$> dCons
  -- | find any data constructors that are < 32 bits when including their tag bits
  (_  , medium) = smallEnough small
  -- | arrange data constructors from smallest to largest tag value
  tagged        = small ++ medium ++ big

  -- | define parts of the c definitions
  enumName      = CIdent $ fromString $ lower $ ident tconid
  enumElts =
    [cenum|$id:(fst $ fst $ head tagged)|]
      : (toCEnum . fst . fst <$> tail tagged)
  maxNumFields =
    snd $ maximumBy (\(_, sz1) (_, sz2) -> sz1 `compare` sz2) (big ++ medium)

  -- | helper functions

  -- | turn DConId into an elt of a c enumerated type
  toCEnum :: DConId -> C.CEnum
  toCEnum n = [cenum|$id:n|]

  -- | categorize data constructor as either small enough or too big to be represented in 31 bits
  -- | when taking into account the tag bits
  smallEnough
    :: [((DConId, L.TypeVariant L.Type), Int)]
    -> ( [((DConId, L.TypeVariant L.Type), Int)]
       , [((DConId, L.TypeVariant L.Type), Int)]
       )
  smallEnough candidates = partition (\(_, sz) -> (sz + tagBits) < 32)
                                     candidates
    where tagBits = q + r where (q, r) = quotRem (length candidates) 2

  -- | augment data constructor with size info in bits
  augmentWSize
    :: (DConId, L.TypeVariant L.Type) -> ((DConId, L.TypeVariant L.Type), Int)
  augmentWSize (dconid, fields) = ((dconid, fields), dConSize fields)

  -- | return the size in bits of a given data constructor
  dConSize :: L.TypeVariant L.Type -> Int
  dConSize (L.VariantNamed   fields) = sum $ fieldSize . snd <$> fields
  dConSize (L.VariantUnnamed fields) = sum $ fieldSize <$> fields

  -- | return the size in bits of a given field
  -- | assume all built-in types are one word for now
  fieldSize :: L.Type -> Int
  fieldSize (L.TBuiltin _) = 32
  fieldSize (L.TCon     _) = 32

  -- | Helper that makes the first letter lowercase 
  lower :: String -> String
  lower ""      = ""
  lower (h : t) = toLower h : t
