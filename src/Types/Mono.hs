module Types.Mono where

import           Types.Common                   ( DConId
                                                , TConId
                                                )

type Arity = Int

-- | The language of type expressions, e.g., what appears in a type signature.
data Type = TArrow Type Type    -- ^ Function type constructor, e.g., Int -> Int
          | TSV Type            -- ^ Scheduled variable of another type
          | TCon TConId        -- ^ Fully-applied monotype, e.g., Option_Int

data TConDef = TConBuiltin -- ^ Builtin type constructor
             | TConUserDef [(DConId, [Type])] -- ^ User-defined type constructor
