module Front.Check where

import qualified Front.Ast                     as A

{- | Predicate of whether all routine type signatures are consistent.

That is, given the type annotation style of a routine, check whether the
parameters are annotated accordingly.

If Pythonic-style annotation is used, all parameters must be annotated exactly
once. If Haskell-like annotation is used, the arrow-type's length must be
consistent with the number of parameters in the routine and the parameters
should not be annotated.

Technically, this is quite unnecessary if we have HM type inference. Yet it is
also good style to have all top-level functions type-annotated, which is what
this checks (it doesn't check any nested let-definitions).
-}
checkTopSignatures :: A.Program -> Bool
checkTopSignatures (A.Program defns) = all checkAnnotations defns
 where
  checkAnnotations :: A.Definition -> Bool
  checkAnnotations (A.DefFn _ binds (A.TypReturn _) _) =
    all annotatedOnce binds
  checkAnnotations (A.DefFn _ binds (A.TypProper ty) _) =
    countCurried ty >= length binds && all notAnnotated binds
  checkAnnotations (A.DefFn _ _ A.TypNone _) = False
  checkAnnotations (A.DefPat _ _           ) = True

  countCurried :: A.Typ -> Int
  countCurried (A.TArrow _ t2) = 1 + countCurried t2
  countCurried _               = 0

  annotatedOnce :: A.Bind -> Bool
  annotatedOnce (A.Bind (A.BindTup bs  ) [_]) = all notAnnotated bs
  annotatedOnce (A.Bind (A.BindCon _ bs) [_]) = all notAnnotated bs
  annotatedOnce (A.Bind (A.BindAs  _ b ) [_]) = notAnnotated b
  annotatedOnce (A.Bind _                [_]) = True
  annotatedOnce (A.Bind (A.BindTup bs  ) [] ) = all annotatedOnce bs
  annotatedOnce (A.Bind (A.BindCon _ bs) [] ) = all annotatedOnce bs
  annotatedOnce (A.Bind (A.BindAs  _ b ) [] ) = annotatedOnce b
  annotatedOnce (A.Bind _                _  ) = False

  notAnnotated :: A.Bind -> Bool
  notAnnotated (A.Bind (A.BindTup bs  ) []) = all notAnnotated bs
  notAnnotated (A.Bind (A.BindCon _ bs) []) = all notAnnotated bs
  notAnnotated (A.Bind (A.BindAs  _ b ) []) = notAnnotated b
  notAnnotated (A.Bind _                []) = True
  notAnnotated (A.Bind _                _ ) = False
