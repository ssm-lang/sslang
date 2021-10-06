module Front.Check where

import qualified Front.Ast                     as A

{- | Predicate of whether all routine type signatures are consistent.

That is, given the type annotation style of a routine, check whether the
parameters are annotated accordingly.

If Pythonic-style annotation is used, all parameters must be annotated exactly
once. If Haskell-like annotation is used, the arrow-type's length must be
consistent with the number of parameters in the routine and the parameters
should not be annotated.
-}
checkRoutineSignatures :: A.Program -> Bool
checkRoutineSignatures (A.Program decls) = all checkAnnotations decls
 where
  checkAnnotations (A.Function _ binds _ (A.ReturnType _)) =
    all annotatedOnce binds
  checkAnnotations (A.Function _ binds _ (A.CurriedType ty)) =
    countParams ty == length binds && all notAnnotated binds

  countParams (A.TArrow _ t2) = 1 + countParams t2
  countParams _               = 0

  annotatedOnce (A.Bind    _   (Just _)) = True
  annotatedOnce (A.TupBind bds (Just _)) = all notAnnotated bds
  annotatedOnce (A.TupBind bds Nothing ) = all annotatedOnce bds
  annotatedOnce _                        = False

  notAnnotated (A.Bind    _   Nothing) = True
  notAnnotated (A.TupBind bds Nothing) = all notAnnotated bds
  notAnnotated _                       = False
