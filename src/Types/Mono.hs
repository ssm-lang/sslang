module Types.Mono where

import           Types.Common                   ( DConId
                                                , TConId
                                                )


-- | The language of type expressions, e.g., what appears in a type signature.
data Type = TArrow Type Type    -- ^ Function type constructor, e.g., Int -> Int
          | TCon TConId         -- ^ Type constructor, e.g., Option_Int

data TConDef = TConBuiltin                    -- ^ Builtin type constructor
             | TConUserDef [(DConId, [Type])] -- ^ User-defined type constructor
