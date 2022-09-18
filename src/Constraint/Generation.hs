{-# LANGUAGE OverloadedStrings #-}

module Constraint.Generation where

import Common.Identifiers (IsString (fromString), TVarId (..), VarId (..))
import Constraint.Datatype (AstEnv)
import Constraint.Solver as S
import Constraint.SolverM (SolverCtx (..), SolverM)
import Constraint.Structure
import Constraint.Utils (throwTypeError)
import Control.Monad (unless)
import Control.Monad.State.Class (get, put)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import IR.IR
import IR.Types.Type

genConstraints :: Program Annotations -> SolverM s (Co (Program Type))
genConstraints prog = do
  -- let (n, e) = head $ programDefs prog
  -- c1 <-
  --   exist
  --     ( \v -> do
  --         c <- hastype prog e v
  --         return $ def n v c -- support recursion
  --     )
  c1 <- hastypeDefs prog (programDefs prog)
  c2 <- let0 c1 -- make constraint well-formed
  let c3 = fmap (\(_, defs') -> prog {programDefs = defs'}) c2
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
      ( do
          let binders = map fst binds
              exprs = map snd binds
          names <- mapM unBinder binders
          let binds' = zip names exprs
          cu <- hastype' u w
          letn
            names
            ( \vs ->
                hastypeExprs
                  prog
                  binds'
                  vs
                  ( return . defDefs prog binds' vs
                  )
            )
            cu
      )
        <$$> \(_, _, binds', u') ->
          let binds'' = [(n, e) | ((n, _), (_, e)) <- zip binds binds']
           in Let binds'' u' (extract u')
    hastype' (Match e branches _) w =
      exist
        ( \v ->
            hastypeBranches
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
        <$$> \((e', branches'), t) -> Match e' branches' t
    hastype' (Prim prim es _) w = hastypePrim prog prim es w
    hastype' (Data dcid _) w = hastypeData prog dcid w

hastypePrim ::
  Program Annotations ->
  Primitive ->
  [Expr Annotations] ->
  Variable ->
  SolverM s (Co (Expr Type))
hastypePrim prog prim es w =
  let primConstraints' = primConstraints prog prim es w
   in case prim of
        New ->
          primConstraints'
            ( \vs ->
                let v1 = head vs
                 in Just $ DeepStructure (TyConS (TConId "&") [DeepVar v1])
            )
            (const [Nothing])
        Dup ->
          primConstraints'
            (Just . DeepVar . head)
            (const [Nothing])
        Drop ->
          primConstraints'
            (const $ Just $ DeepStructure $ TyConS "()" [])
            (const [Nothing])
        Deref ->
          primConstraints'
            ( \vs ->
                let v1 = head vs
                 in Just $ DeepStructure $ TyConS "&" [DeepVar v1]
            )
            (const [Nothing])
        Assign ->
          primConstraints'
            (const $ Just $ DeepStructure $ TyConS "()" [])
            ( \vs ->
                let v1 = head vs
                 in [ Just $ DeepStructure $ TyConS "&" [DeepVar v1],
                      Just $ DeepVar v1
                    ]
            )
        After ->
          primConstraints'
            (const $ Just $ DeepStructure $ TyConS "()" [])
            ( \vs ->
                let v1 = head vs
                 in [ Just $ DeepStructure $ TyConS "Int32" [],
                      Just $ DeepStructure $ TyConS "&" [DeepVar v1],
                      Just $ DeepVar v1
                    ]
            )
        Par ->
          primConstraints'
            (Just . tupleDeepStructure)
            (map $ const Nothing)
        Wait ->
          primConstraints'
            (const $ Just $ DeepStructure $ TyConS "()" [])
            (map $ const Nothing)
        Loop ->
          primConstraints'
            (const $ Just $ DeepStructure $ TyConS "()" [])
            (const [Nothing])
        Break ->
          primConstraints'
            (const $ Just $ DeepStructure $ TyConS "()" [])
            (const [])
        Now ->
          primConstraints'
            (const $ Just $ DeepStructure $ TyConS "Int32" [])
            (const [Just $ DeepStructure $ TyConS "()" []])
        PrimOp primOp -> hastypePrimOp prog primOp es w
        CQuote s ->
          primConstraints'
            (const Nothing)
            (const [])
        CCall csym ->
          primConstraints'
            (const Nothing)
            (const [])
        FfiCall vid -> undefined -- TODO

tupleDeepStructure :: [Variable] -> DeepType
tupleDeepStructure vs = DeepStructure $ TyConS (tupleId $ length vs) (map DeepVar vs)

hastypePrimOp ::
  Program Annotations ->
  PrimOp ->
  [Expr Annotations] ->
  Variable ->
  SolverM s (Co (Expr Type))
hastypePrimOp prog primOp es w =
  let primConstraints' = primConstraints prog (PrimOp primOp) es w
      unaryPrimOp =
        primConstraints'
          (const (Just $ DeepStructure $ TyConS (TConId "Int32") []))
          (const [Just $ DeepStructure $ TyConS (TConId "Int32") []])
      binaryPrimOp =
        primConstraints'
          (const (Just $ DeepStructure $ TyConS (TConId "Int32") []))
          ( const
              [ Just $ DeepStructure $ TyConS (TConId "Int32") [],
                Just $ DeepStructure $ TyConS (TConId "Int32") []
              ]
          )
   in case primOp of
        PrimNeg -> unaryPrimOp
        PrimNot -> unaryPrimOp
        PrimBitNot -> unaryPrimOp
        PrimAdd -> binaryPrimOp
        PrimSub -> binaryPrimOp
        PrimMul -> binaryPrimOp
        PrimDiv -> binaryPrimOp
        PrimMod -> binaryPrimOp
        PrimBitAnd -> binaryPrimOp
        PrimBitOr -> binaryPrimOp
        PrimEq -> binaryPrimOp
        PrimNeq -> binaryPrimOp
        PrimGt -> binaryPrimOp
        PrimGe -> binaryPrimOp
        PrimLt -> binaryPrimOp
        PrimLe -> binaryPrimOp

primConstraints ::
  Program Annotations ->
  Primitive ->
  [Expr Annotations] ->
  Variable ->
  ([Variable] -> Maybe DeepType) ->
  ([Variable] -> [Maybe DeepType]) ->
  SolverM s (Co (Expr Type))
primConstraints prog prim es w struc strucs = do
  existn
    es
    ( \vs -> do
        unless (length es == length (strucs vs)) $ throwIncorrectPrimArguments prim
        mapMlater
          perE
          (zip3 es vs (strucs vs))
          ( \ces ->
              ( do
                  c1 <- case struc vs of
                    Just struc' ->
                      deep struc' (w -=-)
                    Nothing -> return S.CTrue
                  let c2 = CDecode w
                  return $ c1 ^& ces ^& c2
              )
                <$$> \((_, es'), t) -> Prim prim es' t
          )
    )
  where
    perE (e, v, struc') k = do
      c <-
        ( do
            c1 <- hastype prog e v
            c2 <- case struc' of
              Just struc'' -> deep struc'' (v -=-)
              Nothing -> return S.CTrue
            return $ c1 ^& c2
          )
          <$$> fst
      k c

throwIncorrectPrimArguments :: Primitive -> SolverM s a
throwIncorrectPrimArguments prim = throwTypeError $ "incorrect argument number for " ++ show prim

throwIncorrectPrimOpArguments :: PrimOp -> SolverM s a
throwIncorrectPrimOpArguments primOp = throwTypeError $ "incorrect argument number for " ++ show primOp

hastypeData ::
  Program Annotations ->
  DConId ->
  Variable ->
  SolverM s (Co (Expr Type))
hastypeData prog dcid w = undefined

unBinder :: Binder -> SolverM s VarId
unBinder = maybe freshName return

freshName :: SolverM s VarId
freshName = do
  ctx <- get
  put ctx {currNameId = currNameId ctx + 1}
  return . VarId . fromString $ "_anon_var" ++ show (currNameId ctx)

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
    onBranch (p@(AltLit lit), u) k = do
      res <-
        ( do
            c1 <- vScrutinee -==- lookupLitStruc lit
            c2 <- hastype prog u vReturn
            return $ c1 ^& c2
          )
          <$$> \(_, u') -> (p, u')
      k res
    onBranch (p@(AltDefault Nothing), u) k = do
      res <-
        hastype prog u vReturn
          <$$> \u' -> (p, u')
      k res
    onBranch _ _ = error "this type of Alt is not inferred yet"

hastypeExprs ::
  Program Annotations ->
  [(VarId, Expr Annotations)] ->
  [Variable] ->
  SBinder s (Co [(VarId, Expr Type)]) r
hastypeExprs prog defs vs =
  mapMlater onDef (zip defs vs)
  where
    onDef :: ((VarId, Expr Annotations), Variable) -> SBinder s (Co (VarId, Expr Type)) r
    onDef ((name, e), v) k = do
      c <- hastype prog e v <$$> \e' -> (name, e')
      k c

existn ::
  [a] ->
  SBinder s [Variable] r
existn = mapMnow onDef
  where
    onDef :: a -> SBinder s Variable r
    onDef _ = exist

hastypeDefs ::
  Program Annotations ->
  [(VarId, Expr Annotations)] ->
  SolverM s (Co [(VarId, Expr Type)])
hastypeDefs prog defs =
  existn
    defs
    ( \vs ->
        hastypeExprs
          prog
          defs
          vs
          ( return . defDefs prog defs vs
          )
    )

defDefs ::
  Program Annotations ->
  [(VarId, Expr Annotations)] ->
  [Variable] ->
  Co [(VarId, Expr Type)] ->
  Co [(VarId, Expr Type)]
defDefs _ defs vs cdefs =
  foldl addDef cdefs (zip defs vs)
  where
    addDef c ((name, _), v) = CDef name v c

subTVars :: [TVarId] -> [Type] -> Type -> Type
subTVars gs ws = subTVars'
  where
    subTVars' (TCon tcid ts) = TCon tcid (map subTVars' ts)
    subTVars' (TVar x) =
      let iopt = elemIndex x gs
       in case iopt of
            Just i -> ws !! i
            Nothing -> TVar x

mapMnow :: (a -> SBinder s b r) -> [a] -> SBinder s [b] r
mapMnow _ [] k = k []
mapMnow f (x : xs) k =
  f
    x
    ( \y ->
        mapMnow
          f
          xs
          ( \ys ->
              k (y : ys)
          )
    )

mapMlater :: (a -> SBinder s (Co b) r) -> [a] -> SBinder s (Co [b]) r
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
