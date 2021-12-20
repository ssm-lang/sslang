{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | Utilities for embedding source code in test cases.
module Data.String.SourceCode
  ( here
  , here'
  ) where

import           Data.Char                      ( isSpace )
import qualified Data.String.Here              as Here

import           Data.List                      ( dropWhileEnd )
import           Language.Haskell.TH.Quote

-- | Remove leading whitespace from multiple lines of text.
trimLeadingWhitespace :: String -> String
trimLeadingWhitespace = unlines . trimVertical . trimHorizontal . lines
 where
  trimHorizontal ss = map (drop $ minLeadingSpaces ss) ss
  trimVertical = dropWhileEnd emptyLine . dropWhile emptyLine

  minLeadingSpaces = minimum . map leadingSpaces . filter (not . emptyLine)
  leadingSpaces    = length . takeWhile isSpace
  emptyLine        = all isSpace

-- | Interpolated heredoc that trims leading, trailing, and per-line whitespace.
here :: QuasiQuoter
here = QuasiQuoter { quoteExp = quoteExp Here.i . trimLeadingWhitespace }

-- | Literal heredoc that trims leading, trailing, and per-line whitespace.
here' :: QuasiQuoter
here' =
  QuasiQuoter { quoteExp = quoteExp Here.hereLit . trimLeadingWhitespace }
