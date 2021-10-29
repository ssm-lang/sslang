-- | Helpers for pretty-printing sslang, including common ASCII tokens.
module Common.Pretty
  ( module Prettyprinter
  , typeAnn
  , drarrow
  , larrow
  , rarrow
  , dbar
  , bar
  , block
  , spaghetti
  ) where

import           Prettyprinter
import           Prettyprinter.Render.String

-- | @typeAnn t d@ annotates document @d@ with type annotation @t@.
typeAnn :: Pretty t => t -> Doc ann -> Doc ann
typeAnn t d = parens $ d <> colon <+> pretty t

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

-- | Constructs a separator-delimited block 'Doc' out of a list of 'Doc's.
block :: Doc ann -> [Doc ann] -> Doc ann
block separator = braces . sep . punctuate separator

-- | Format with a document of infinite width, preventing wraparound.
spaghetti :: Pretty t => t -> String
spaghetti = renderString . layoutPretty opts . pretty
  where opts = LayoutOptions { layoutPageWidth = Unbounded }
