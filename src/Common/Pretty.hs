-- | Helpers for pretty-printing sslang, including common ASCII tokens.
module Common.Pretty
  ( module Prettyprinter
  , typeAnn
  , drarrow
  , larrow
  , rarrow
  , dbar
  , bar
  , amp
  , block
  , indentNo
  , spaghetti
  , Lengthy(lengthy)
  ) where

import           Prettyprinter
import           Prettyprinter.Render.String

{- | @typeAnn t d@ annotates document @d@ with type annotation @t@.

Only used by spaghetti so constrained by Lengthy instead of Pretty for now.
-}
typeAnn :: Lengthy t => t -> Doc ann -> Doc ann
typeAnn t d = parens $ d <> colon <+> lengthy t

-- | @=>@
drarrow :: Doc ann
drarrow = pretty "=>"

-- | @<-@
larrow :: Doc ann
larrow = pretty "<-"

-- | @->@
rarrow :: Doc ann
rarrow = pretty "->"

-- | @||@
dbar :: Doc ann
dbar = pretty "||"

-- | @|@
bar :: Doc ann
bar = pretty "|"

-- | @&@
amp :: Doc ann
amp = pretty "&"

-- | Number of spaces to indent
indentNo :: Int
indentNo = 2

-- | Constructs a separator-delimited block 'Doc' out of a list of 'Doc's.
block :: Doc ann -> [Doc ann] -> Doc ann
block separator = braces . sep . punctuate separator

-- | Format with a document of infinite width, preventing wraparound.
spaghetti :: Lengthy t => t -> String
spaghetti = renderString . layoutPretty opts . lengthy
  where opts = LayoutOptions { layoutPageWidth = Unbounded }

{- | Lengthy Typeclass: "pretty" print the IR; used in conjunction with spaghetti

* Translates from IR to Doc representation in one-to-one fashion
* No simplifications 
* No whitespace formatting
* Type annotates everything
-}
class (Pretty a) => Lengthy a where
  lengthy :: a -> Doc ann
