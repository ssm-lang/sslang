module Passes where

import qualified Ast as A

checkRoutineSignatures :: A.Program -> Bool 
checkRoutineSignatures (A.Program decls) = all checkAnnotations decls
  where
    checkAnnotations (A.Function _ binds _ fnTyAnn) =
        case fnTyAnn of 
          A.ReturnType _ -> all annotatedOnce binds
          A.CurriedType ty -> countParams ty == length binds && all notAnnotated binds

    countParams (A.TArrow _ t2) = 1 + countParams t2
    countParams _ = 0

    annotatedOnce (A.Bind _ (Just _)) = True
    annotatedOnce (A.TupBind bds tupTy) = case tupTy of
                                            Just _ -> all notAnnotated bds
                                            Nothing -> all annotatedOnce bds
    annotatedOnce _ = False

    notAnnotated (A.Bind _ Nothing) = True
    notAnnotated (A.TupBind bds Nothing) = all notAnnotated bds
    notAnnotated _ = False
