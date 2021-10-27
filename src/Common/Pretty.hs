module Common.Pretty
  ( module Prettyprinter
  , typeAnn
  , drarrow
  , larrow
  , rarrow
  , dbar
  , bar
  , block
  ) where

import           Prettyprinter

typeAnn :: Pretty t => t -> Doc ann -> Doc ann
typeAnn t d = parens $ d <> colon <+> pretty t

drarrow :: Doc ann
drarrow = pretty "=>"

larrow :: Doc ann
larrow = pretty "<-"

rarrow :: Doc ann
rarrow = pretty "->"

dbar :: Doc ann
dbar = pretty "||"

bar :: Doc ann
bar = pretty "|"

block :: Doc ann -> [Doc ann] -> Doc ann
block separator = braces . sep . punctuate separator
