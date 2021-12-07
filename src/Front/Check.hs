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
checkTopSignatures (A.Program topdefs) = all checkTopDef topdefs
 where
  checkTopDef :: A.Top -> Bool
  checkTopDef (A.TopDef  d                 ) = checkAnnotations d
  checkTopDef (A.TopInst (A.InstDef _ _ ds)) = all checkAnnotations ds
  checkTopDef _                              = True
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

  annotatedOnce :: A.Pat -> Bool
  annotatedOnce (A.PatAs _ p  ) = annotatedOnce p
  annotatedOnce (A.PatTup ps  ) = all annotatedOnce ps
  annotatedOnce (A.PatCon _ ps) = all annotatedOnce ps
  annotatedOnce (A.PatAnn _ p ) = notAnnotated p
  annotatedOnce _               = False

  notAnnotated :: A.Pat -> Bool
  notAnnotated (A.PatAs _ p  ) = notAnnotated p
  notAnnotated (A.PatTup ps  ) = all notAnnotated ps
  notAnnotated (A.PatCon _ ps) = all notAnnotated ps
  notAnnotated (A.PatAnn _ _ ) = False
  notAnnotated _               = True
