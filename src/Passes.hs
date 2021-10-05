module Passes where

import qualified Ast as A

checkRoutineSignatures :: A.Program -> Bool 
checkRoutineSignatures (A.Program decls) = and $ map checkAnnotations decls
  where
    checkAnnotations (A.Function _ binds _ fnTyAnn) =
        case fnTyAnn of 
          A.ReturnType _ -> foldl annotatedOnce True binds
          A.CurriedType ty -> countParams ty == length binds && foldl notAnnotated True binds

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

