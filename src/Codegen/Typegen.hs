{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Extract and encapsulate the type information needed during codegen.
module Codegen.Typegen (
  DConInfo (..),
  TConInfo (..),
  TypegenInfo (..),
  genTypes,
) where

import Codegen.LibSSM
import qualified IR.IR as I

import qualified Common.Compiler as Compiler
import Common.Identifiers (
  DConId (..),
  TConId (..),
 )

import qualified Language.C.Quote.GCC as C
import qualified Language.C.Syntax as C

import Control.Monad (forM)
import qualified Data.Map as M


-- | Type-related information, abstracted behind partial lookup functions.
data TypegenInfo = TypegenInfo
  { dconInfo :: DConId -> Maybe DConInfo
  -- ^ for each data constructor.
  , tconInfo :: TConId -> Maybe TConInfo
  -- ^ for each type constructor.
  }


-- | Information and codegen handlers associated with each data constructor.
data DConInfo = DConInfo
  { dconType :: TConId
  -- ^ the type that the data constructor inhabits
  , dconSize :: Int
  -- ^ number of fields
  , dconOnHeap :: Bool
  -- ^ whether the data constructor is heap-allocated
  , dconCase :: C.Exp
  -- ^ the dcon tag to match on, i.e., in @case tag@
  , dconConstruct :: C.Exp
  -- ^ constructs a dcon instance
  , dconDestruct :: Int -> C.Exp -> C.Exp
  -- ^ retrieve the ith field
  }


-- | Information and codegen handlers associated with each type constructor.
data TConInfo = TConInfo
  { typeEncoding :: TypeEncoding
  -- ^ how the data type is encoded
  , typeScrut :: C.Exp -> C.Exp
  -- ^ how to retrieve the tag of an instance
  }


-- | How a data type may be encoded, i.e., heap-allocated, by value, or both.
data TypeEncoding = TypePacked | TypeMixed -- TODO: | TypeHeap


-- | Create codegen definitions and helpers for sslang type definitions.
genTypes
  :: [(TConId, I.TypeDef)] -> Compiler.Pass ([C.Definition], TypegenInfo)
genTypes tdefs = do
  cdefs <- mapM genTypeDef tdefs
  typeInfo <- genTypeInfo tdefs
  return (cdefs, typeInfo)


-- | Generate C enums for sslang type definitions, enumerating tags.
genTypeDef :: (TConId, I.TypeDef) -> Compiler.Pass C.Definition
genTypeDef (tcon, tdef) = return [C.cedecl|enum $id:tcon { $enums:tags };|]
 where
  tags = case tdef of
    I.TypeDef [] _ -> []
    I.TypeDef ((dcon, _) : dcons) _ ->
      [C.cenum|$id:dcon = 0|] : map mkEnum dcons
  mkEnum (dcon, _) = [C.cenum|$id:dcon|]


-- | Compute codgen helpers for each sslang type definition.
genTypeInfo :: [(TConId, I.TypeDef)] -> Compiler.Pass TypegenInfo
genTypeInfo tdefs = do
  dInfos <- forM tdefs $ \(tcon, I.TypeDef tvars _) -> do
    forM tvars $ \(dcon, dvari) -> do
      let fields = I.variantFields dvari
          onHeap = fields > 0
          caseExp = [C.cexp|$id:dcon|]
          construct
            | onHeap = new_adt fields dcon
            | otherwise = marshal [C.cexp|$id:dcon|]
          destruct = flip adt_field
      -- TODO: does not handle packed ADTs
      return
        ( dcon
        , DConInfo
            { dconType = tcon
            , dconSize = fields
            , dconOnHeap = onHeap
            , dconCase = caseExp
            , dconConstruct = construct
            , dconDestruct = destruct
            }
        )

  let dInfoLookup = flip M.lookup $ M.fromList $ concat dInfos

  tInfos <- forM tdefs $ \(tcon, I.TypeDef tvars _) -> do
    -- Extract info for each dcon associated with this tcon
    tvarsInfo <- forM tvars $ \(dcon, _) -> do
      let failMsg = "Missing info for data constructor: " ++ show dcon
      maybe (Compiler.unexpected failMsg) return $ dInfoLookup dcon

    -- Determine the encoding of inhabitants of this type
    let (encoding, tagFn)
          | not (any dconOnHeap tvarsInfo) = (TypePacked, unmarshal)
          | otherwise = (TypeMixed, adt_tag)
    return (tcon, TConInfo{typeEncoding = encoding, typeScrut = tagFn})

  let tInfoLookup = flip M.lookup $ M.fromList tInfos

  return TypegenInfo{dconInfo = dInfoLookup, tconInfo = tInfoLookup}
