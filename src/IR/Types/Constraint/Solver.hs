{-# LANGUAGE GADTs #-}
{- | Constraint Solver -}

module IR.Types.Constraint.Solver where

import           IR.Types.Constraint.Type       ( Infer, Co(..), internalError)
import IR.IR (Program(..), Type)
import Control.Monad (unless)
import Control.Monad.Except (throwError)

solveAndElab :: Co (Program Type) -> Infer s (Program Type)
solveAndElab c = do
  unless
    (ok c)
    ( throwError
    $ internalError
    $ "Solver: ill-formed toplevel constraint"
    )
  r   <- solve c
  r

ok :: Co a -> Bool
ok CTrue             = True
ok (CPure _        ) = True
ok (CMap c _       ) = ok c
ok (CLet _ _ _ _ c2) = ok c2
ok (CConj c1 c2    ) = ok c1 && ok c2
ok _                 = False

solve :: Co a -> Infer s (Infer s a)
solve = undefined
