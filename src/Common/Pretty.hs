module Common.Pretty
  ( module Prettyprinter
  , typeAnn
  , drarrow
  , larrow
  , dbar
  , bar
  , block
  ) where

import           Data.List                      ( intersperse )
import           Prettyprinter

typeAnn :: Pretty t => t -> Doc ann -> Doc ann
typeAnn t d = parens $ d <> pretty t

drarrow :: Doc ann
drarrow = pretty "=>"

larrow :: Doc ann
larrow = pretty "<-"

dbar :: Doc ann
dbar = pretty "||"

bar :: Doc ann
bar = pretty "|"

block :: Doc ann -> [Doc ann] -> Doc ann
block seperator = braces . hsep . intersperse seperator
