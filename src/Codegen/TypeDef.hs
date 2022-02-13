{-# LANGUAGE QuasiQuotes #-}
module Codegen.TypeDef where

import           Codegen.LibSSM
import           Common.Identifiers             ( DConId
                                                , TConId
                                                , fromId
                                                , ident
                                                )
import           Data.List                      ( partition )
import qualified Data.Map                      as M
import qualified IR.Types.Poly                 as L
import qualified IR.Types.TypeSystem           as L
import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

data TypeDefInfo = TypeDefInfo
  { dconType   :: M.Map DConId TConId                   -- ^ ('DConId', 'TConId') key value pairs
  , dconSize   :: M.Map DConId Int                      -- ^ size of ADT instance by its number of fields
  , isPointer  :: M.Map DConId Bool                     -- ^ whether the ADT is represented as a heap object
  , dtags      :: M.Map DConId C.Exp                    -- ^ ('DConId', c expression for tag) key-value pairs 
  , extractTag :: M.Map TConId (C.Exp -> C.Exp)         -- ^ ('TConId', extract-tag c expression) key value pairs
  , intInit    :: M.Map DConId C.Exp                    -- ^ ('DConId', initialization c expression) key value pairs
  , ptrFields  :: M.Map DConId (C.Exp -> Int -> C.Exp)  -- ^ ('DConId', access field c expression) key value pairs
  }

instance Semigroup TypeDefInfo where
  (<>) = combineTypeDefInfo

instance Monoid TypeDefInfo where
  mempty = TypeDefInfo M.empty M.empty M.empty M.empty M.empty M.empty M.empty

combineTypeDefInfo :: TypeDefInfo -> TypeDefInfo -> TypeDefInfo
combineTypeDefInfo a b = TypeDefInfo typ sz isPtr tags exTags intInits fieldz
 where
  typ      = dconType a `M.union` dconType b
  sz       = dconSize a `M.union` dconSize b
  tags     = dtags a `M.union` dtags b
  isPtr    = isPointer a `M.union` isPointer b
  exTags   = extractTag a `M.union` extractTag b
  intInits = intInit a `M.union` intInit b
  fieldz   = ptrFields a `M.union` ptrFields b

-- | Generate definitions for SSM type definitions.
genTypeDef :: (TConId, L.TypeDef L.Type) -> ([C.Definition], TypeDefInfo)
genTypeDef (tconid, L.TypeDef dCons _) = ([tagEnum], info)
 where
  tagEnum = [cedecl| enum $id:tconid {
                          $enums:(enumElts)
                       };  
             |]

  enumElts =
    [cenum|$id:(head dConTags) = 0|]
      : ((\n -> [cenum|$id:n|]) <$> tail dConTags)
  {- 
  Determine which data constructors are small enough to be represented as an integer,
  and which data constructors are large enough that they should be represented on the heap.
  The tags will be ordered in the enum from smallest to largest data constructor.
  -}
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
      tagList                       = ident . fst <$> (small ++ big)
      big                           = greater ++ notSmallEnough
      small                         = smallEnough

      -- is the data constructor greater or less than a word?
      (greater    , lessThan      ) = partition ((>= 32) . dConSize) dataCons

      {-
      Is this "small" data constructor small enough to be a packed value?
      Data constructors that are "small enough" have taken into account
      the edge case of many nullary data constructors.
      -}
      (smallEnough, notSmallEnough) = partition isSmallEnough lessThan
       where
        isSmallEnough = (< 32) . (tagBits +) . dConSize
        tagBits       = q + r where (q, r) = length lessThan `quotRem` 2

  -- |  Save information about the analyzed ADT. 
  info = saveInfo tconid heapObjs intObjs
   where
    saveInfo
      :: TConId
      -> [(DConId, L.TypeVariant L.Type)]
      -> [(DConId, L.TypeVariant L.Type)]
      -> TypeDefInfo
    saveInfo typ ptrs intgrs = TypeDefInfo { dconType   = typs
                                           , dconSize   = typsz
                                           , dtags      = tags
                                           , isPointer  = isPtr
                                           , extractTag = readTagFuncs
                                           , intInit    = initVals
                                           , ptrFields  = pFields
                                           }
     where
      typs         = M.fromList $ zip (fst <$> intgrs ++ ptrs) (repeat typ)

      typsz        = M.fromList $ zip (fst <$> dcons) $ dConNumFields <$> dcons
      tags = M.fromList $ zip (fst <$> dcons) (cexpr . fromId . fst <$> dcons)
      dcons        = intgrs ++ ptrs

      isPtr        = M.fromList $ intDCons ++ heapDCons
      intDCons     = zip (fst <$> intgrs) (repeat False)
      heapDCons    = zip (fst <$> ptrs) (repeat True)

      readTagFuncs = M.fromList [(tconid, readTagFunc ptrs intgrs)]
      readTagFunc
        :: [(DConId, L.TypeVariant L.Type)]
        -> [(DConId, L.TypeVariant L.Type)]
        -> C.Exp
        -> C.Exp
      readTagFunc ps ints val
        | null ps   = intTagFunc val
        | -- always integer
          null ints = ptrTagFunc val
        | -- always heap object
          otherwise = tagFunc (isInt val) (intTagFunc val) (ptrTagFunc val)
       where
        intTagFunc :: C.Exp -> C.Exp
        intTagFunc v = [cexp| (($exp:v >> $uint:(tagBits+1)) & (tagBits+1))|]
          where tagBits = length intDCons
        ptrTagFunc :: C.Exp -> C.Exp
        ptrTagFunc = adt_tag -- [cexp|($exp:v.$id:mm.tag)|]
        isInt v = [cexp| (($exp:v) & 0x1 == 0x1) |]
        tagFunc cond a b = [cexp| (($exp:cond) ? ($exp:a) : ($exp:b)) |]
      initVals    = M.fromList intInitVals
      intInitVals = intInitVal . fst <$> intgrs
       where
        intInitVal :: DConId -> (DConId, C.Exp)
        intInitVal tg = (tg, marshal [cexp|$id:tg|])

      pFields = M.fromList $ zip (fst <$> intgrs) (repeat accessField)
       where
        accessField :: C.Exp -> Int -> C.Exp
        accessField = adt_field
                -- [cexp|$exp:(ssm_to_obj nm)->$id:payload[$uint:index] |]

  -- | Return size of data constructor in bits
  dConSize :: (DConId, L.TypeVariant L.Type) -> Int
  dConSize (_, L.VariantNamed fields  ) = sum $ fieldSize . snd <$> fields
  dConSize (_, L.VariantUnnamed fields) = sum $ fieldSize <$> fields

  -- | Return size of field in bits
  fieldSize :: L.Type -> Int
  fieldSize = const 32

  -- | Return the number of fields in a data constructor
  dConNumFields :: (DConId, L.TypeVariant L.Type) -> Int
  dConNumFields (_, L.VariantNamed fields  ) = length fields
  dConNumFields (_, L.VariantUnnamed fields) = length fields
