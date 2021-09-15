module Duration where

import Data.Word ( Word64 )

-- A duration encoded as an integral number of nanoseconds
newtype Duration = Duration Word64
  deriving (Eq)

instance Show Duration where
  show (Duration d) = coarsest [(3600000000000, "h")
                               ,(  60000000000, "m")
                               ,(   1000000000, "s")
                               ,(      1000000, "ms")
                               ,(         1000, "us")]
   where coarsest ((divider, suff) : xs) | r == 0 = show q ++ " " ++ suff
                                         | otherwise = coarsest xs
            where (q, r) = d `quotRem` divider
         coarsest [] = show d ++ " ns"
                               
