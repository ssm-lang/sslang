{- | Decode UVar and UType to TVar and Type -}

module IR.Types.Constraint.Decoder where

import           Common.Identifiers             ( TVarId(..)
                                                , fromString
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( throwError )
import qualified Data.Map                      as M
import           IR.Types.Constraint.Type       ( Infer
                                                , UType(..)
                                                , UVar
                                                , getDecoderMap
                                                , getStructure
                                                , getTag
                                                , internalError
                                                , isLeaf
                                                , projectNonLeaf
                                                , putDecoderMap
                                                , getDescriptor
                                                )
import           IR.Types.Type                  ( Type(..) )



decode :: UVar s -> Infer s Type
decode uv = do
  m <- getDecoderMap
  d <- getDescriptor uv

  -- check if decoded already
  let tag = getTag d
  case M.lookup tag m of
    Just itype -> return itype

    -- need to decode
    Nothing    -> do
      struc <- getStructure d
      a     <- if isLeaf struc
        then return $ decodeId tag
        else
          (do
            istruc <- mapM decode (projectNonLeaf struc)
            return $ unstructure istruc
          )

      -- remember decoded result in decoderMap
      putDecoderMap $ M.insert tag a m
      return a



inject :: Int -> TVarId
inject i = fromString $ "_a" ++ show i



decodeVariable :: UVar s -> Infer s TVarId
decodeVariable uv = do
  d     <- getDescriptor uv
  struc <- getStructure d

  unless (isLeaf struc) $ throwError $ internalError
    "can only decode leaf structure"

  return . inject . getTag $ d



decodeId :: Int -> Type
decodeId = TVar . inject



unstructure :: UType Type -> Type
unstructure (UTCon tcid ts) = TCon tcid ts
