{-# LANGUAGE QuasiQuotes #-}
module Codegen.Types
  ( svBaseTypename
  , genType
  ) where

import qualified Types.Flat                    as L
import qualified Types.TypeSystem              as L

import           Common.Identifiers             ( fromId )

import           Codegen.Identifiers
import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

-- | Possible, but temporarily punted for the sake of expediency.
todo :: a
todo = error "Not yet implemented"

-- | Impossible without a discussion about implementation strategy.
nope :: a
nope = error "Not yet supported"

-- | Obtain the type name of the base type of an SV.
svBaseTypename :: L.Type -> Maybe CIdent
svBaseTypename = fmap typeId . L.deref

-- | Translate an SSM 'Type' to a 'C.Type'.
genType :: L.Type -> C.Type
genType typ = ptrs_ typ $ ctype $ typeId typ

-- | Wrap a 'C.Type' with pointers, according to some 'L.Type'.
ptrs_ :: L.Type -> C.Type -> C.Type
ptrs_ (L.TBuiltin (L.Ref _)) t = [cty|$ty:t *|]
ptrs_ _                      t = t
-- ^ TODO: this does not handle stacked pointers

-- | Obtains the C type name corresponding to an SSM type.
typeId :: L.Type -> CIdent
typeId (L.TCon     t ) = fromId t
typeId (L.TBuiltin bt) = builtinId bt

-- | Obtains the C type name corresponding to an SSM built-in type.
builtinId :: L.Builtin L.Type -> CIdent
builtinId L.Unit        = todo
builtinId L.Void        = todo
builtinId (L.Arrow _ _) = todo
builtinId (L.Tuple _  ) = todo
builtinId (L.Ref   t  ) = sv_ $ typeId t -- NOTE: this does not add pointers
