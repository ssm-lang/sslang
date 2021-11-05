module IR.LambdaLift where

import qualified Common.Compiler               as Compiler
import qualified IR.IR                         as I

import qualified IR.Types.Poly                 as Poly

import Debug.Trace

import qualified Data.Set as S
import Data.List (intercalate)





liftProgramLambdas
  :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
liftProgramLambdas p = do
  let defs = I.programDefs p
      globalScope = map (\(v, _) -> show v) defs
      funs = filter isFun defs
      freeVars = map (getFrees (S.fromList globalScope) (S.fromList globalScope)) (map snd funs)
  traceM (show $ zip (map (\(v, _) -> v) funs) freeVars)
  return p
    where
      isFun (_, I.Lambda _ _ _) = True
      isFun _ = False
      getFrees scp gs (I.Var v _) = if S.member (show v) scp then [] else [show v]
      getFrees scp gs (I.App e1 e2 _) = getFrees scp gs e1 ++ getFrees scp gs e2
      getFrees scp gs (I.Let binds e _) = let newScp = foldl (\s (Just v, _) -> S.insert (show v) s) scp binds in
                                        (concatMap (\(_, be) -> getFrees newScp gs be) binds) ++ getFrees newScp gs e
      getFrees scp gs (I.Lambda (Just v) e _) = let newScp = S.insert (show v) gs in
                                                trace (intercalate "\n" (getFrees newScp gs e) ++ "\n--") []
      getFrees scp gs (I.Match _ _ _ _) = undefined
      getFrees scp gs (I.Prim _ es _) = concatMap (getFrees scp gs) es
      getFrees _ _ _ = []
