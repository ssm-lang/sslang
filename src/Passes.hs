module Passes where

import qualified Ast as A

checkRoutineSignatures :: A.Program -> A.Program
checkRoutineSignatures (A.Program decls) = A.Program $ map checkAnnotations decls
  where
    checkAnnotations decl@(A.Function _ binds _ fnTyAnn) =
        case fnTyAnn of 
          A.ReturnType _ -> let res = foldl annotatedOnce True binds in
                                if res then decl else error "bad"
          A.CurriedType _ -> decl --{- get arrow length, check same. -} foldl notAnnotated True binds

    annotatedOnce ret (A.Bind _ (Just _)) = ret && True
    annotatedOnce ret (A.TupBind bds tupTy) = ret && case tupTy of
                                                     Just _ -> foldl notAnnotated True bds
                                                     Nothing -> foldl annotatedOnce True bds
    annotatedOnce _ _ = False

    notAnnotated ret (A.Bind _ Nothing) = ret && True
    notAnnotated ret (A.TupBind bds Nothing) = ret && foldl notAnnotated True bds
    notAnnotated _ _ = False

