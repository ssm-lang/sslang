{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Constraint.Generation where

import Common.Identifiers (IsString (fromString), TVarId (..), VarId (..))
import Constraint.Datatype (AstEnv)
import Constraint.Solver
import Constraint.SolverM (SolverM)
import Constraint.Structure
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import IR.IR
import IR.Types.Type

genConstraints :: Program Annotations -> SolverM s (Co (Program Type))
genConstraints prog = do
  let (n, e) = head $ programDefs prog
  c1 <-
    exist
      ( \v -> do
          c <- hastype prog e v
          return $ def n v c -- support recursion
      )
  c2 <- let0 c1
  let c3 = fmap (\(_, e') -> prog {programDefs = [(n, e')]}) c2
  return c3

lookupLitStruc :: Literal -> Structure Variable
lookupLitStruc (LitIntegral _) = TyConS (fromString "Int32") []
lookupLitStruc LitEvent = TyConS (fromString "()") []

lookupLitType :: Literal -> Type
lookupLitType (LitIntegral _) = I32
lookupLitType LitEvent = Unit

hastype :: Program Annotations -> Expr Annotations -> Variable -> SolverM s (Co (Expr Type))
hastype prog = hastype'
  where
    hastype' (Var x _) w =
      inst x w
        <$$> \((gs, t), witnesses) -> Var x (subTVars gs witnesses t)
    hastype' (Lit l _) w =
      w -==- lookupLitStruc l
        <$$> \_ -> Lit l (lookupLitType l)
    hastype' (Lambda x u _) w =
      exist
        ( \v1 ->
            exist
              ( \v2 ->
                  do
                    c1 <- w -==- TyConS (fromString "->") [v1, v2]
                    c2 <- hastype prog u v2
                    let c3 = def (fromJust x) v1 c2
                    return $ c1 ^& c3 ^& CDecode v1
              )
        )
        <$$> \((_, e), t) -> Lambda x e (Arrow t (extract e))
    hastype' (App e1 e2 _) w =
      exist
        ( \v -> do
            c1 <- liftB hastype' e1 (TyConS (fromString "->") [v, w])
            c2 <- hastype' e2 v
            return $ c1 ^& c2
        )
        <$$> \(e1', e2') -> App e1' e2' (Arrow (extract e1') (extract e2'))
    hastype' (Let binds u _) w =
      let (x, e) = head binds
       in ( do
              cu <- hastype' u w
              let1
                (fromJust x)
                ( \v -> do
                    c <- hastype' e v
                    return $ def (fromJust x) v c -- support recursion
                )
                cu
          )
            <$$> \(_, (_, t'), e', u') -> Let [(x, e')] u' t'
    hastype' (Match e branches _) w =
      exist
        ( \v ->
            ( hastypeBranches
                prog
                branches
                w
                v
                ( \branches' -> do
                    c1 <- hastype' e v
                    let ty = CDecode w
                    return $ c1 ^& branches' ^& ty
                )
            )
        )
        <$$> \((e', branches'), t) -> Match e' branches' t
    hastype' e _ = error $ show e

hastypeBranches ::
  Program Annotations ->
  [(Alt, Expr Annotations)] ->
  Variable ->
  Variable ->
  SBinder s (Co [(Alt, Expr Type)]) r
hastypeBranches prog branches vReturn vScrutinee =
  mapMlater onBranch branches
  where
    onBranch :: (Alt, Expr Annotations) -> SBinder s (Co (Alt, Expr Type)) r
    onBranch (p@(AltLit _), u) k = do
      res <-
        hastype prog u vReturn
          <$$> \u' -> (p, u')
      k res
    onBranch (p@(AltDefault Nothing), u) k = do
      res <-
        hastype prog u vReturn
          <$$> \u' -> (p, u')
      k res

hastypeBranch ::
  Program Annotations ->
  (Alt, Expr Annotations) ->
  Variable ->
  SolverM s (Co (Alt, Expr Type))
hastypeBranch prog (p, b) w = case p of
  AltLit _ ->
    hastype prog b w
      <$$> \b' -> (p, b')
  _ -> error "This type of Alt is not inferred yet"

subTVars :: [TVarId] -> [Type] -> Type -> Type
subTVars gs ws = subTVars'
  where
    subTVars' (TCon tcid ts) = TCon tcid (map subTVars' ts)
    subTVars' (TVar x) =
      let iopt = elemIndex x gs
       in case iopt of
            Just i -> ws !! i
            Nothing -> TVar x

mapMlater :: (a -> SBinder s (Co c) r) -> [a] -> SBinder s (Co [c]) r
mapMlater _ [] k = k (pure [])
mapMlater f (x : xs) k =
  f
    x
    ( \y ->
        mapMlater
          f
          xs
          ( \ys ->
              k
                (fmap (uncurry (:)) (y ^& ys))
          )
    )

-- hastype' :: AstEnv -> A.Expr -> Variable -> SolverM s (Co IExpr)
-- hastype' = undefined

-- exist :: (Variable -> SolverM s (Co IExpr)) -> SolverM s (Co IExpr)
-- exist = undefined
