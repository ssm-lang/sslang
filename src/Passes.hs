module Passes where

import qualified Ast as A

checkRoutineSignatures :: A.Program -> A.Program
checkRoutineSignatures (A.Program decls) = A.Program $ map checkAnnotations decls
  where
    checkAnnotations decl@(A.Function _ binds _ fnTyAnn) =
        case fnTyAnn of 
          A.ReturnType _ -> let res = foldl annotatedOnce True binds in
                                if res then decl else error "bad"
          A.CurriedType ty -> let numParams = countParams ty
                                  noneAnnotated = foldl notAnnotated True binds in
                                  if numParams == length binds && noneAnnotated then decl else error "bad"

    annotatedOnce ret (A.Bind _ (Just _)) = ret && True
    annotatedOnce ret (A.TupBind bds tupTy) = ret && case tupTy of
                                                     Just _ -> foldl notAnnotated True bds
                                                     Nothing -> foldl annotatedOnce True bds
    annotatedOnce _ _ = False

    notAnnotated ret (A.Bind _ Nothing) = ret && True
    notAnnotated ret (A.TupBind bds Nothing) = ret && foldl notAnnotated True bds
    notAnnotated _ _ = False

    countParams (A.TArrow _ t2) = 1 + countParams t2
    countParams _ = 0

