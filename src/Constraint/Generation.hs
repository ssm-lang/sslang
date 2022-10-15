{-# LANGUAGE OverloadedStrings #-}

module Constraint.Generation where

import           Common.Identifiers             ( IsString(fromString)
                                                , TVarId(..)
                                                )
import           Constraint.Solver             as S
import           Constraint.SolverM             ( SolverCtx(..)
                                                , SolverGen(..)
                                                , SolverM
                                                )
import           Constraint.Structure
import           Constraint.Utils               ( throwTypeError )
import           Control.Monad                  ( foldM
                                                , unless
                                                )
import           Control.Monad.State.Class      ( get
                                                , put
                                                )
import           Data.List                      ( elemIndex )
import qualified Data.Map                      as M
import           IR.IR
import           IR.Types.Type

genConstraints :: SolverM s (Co (Program Type))
genConstraints = do
  prog <- solverProg <$> get
  c    <-
    hastypeDefs (programDefs prog)
    >>= genDConConstraints -- define data constructors in constraints (using CLet)
    >>= let0 -- make constraint well-formed
  let c' = fmap (\(_, defs') -> prog { programDefs = defs' }) c
  return c'

genDConConstraints
  :: Co [(VarId, Expr Type)] -> SolverM s (Co [(VarId, Expr Type)])
genDConConstraints cdefs = do
  cmap <- dconMap . solverGen <$> get
  foldM dconConstraint cdefs cmap
 where
  dconDeepStruc tcid _ [] vs =
    return $ DeepStructure (TyConS tcid (map DeepVar vs))
  dconDeepStruc tcid tvs (t : ts) vs = do
    d <- dconDeepStruc tcid tvs ts vs
    let substT (TCon tcid' ts') = do
          ts'' <- mapM substT ts'
          return $ DeepStructure (TyConS tcid' ts'')
        substT (TVar tvid) = case elemIndex tvid tvs of
          Just i -> return $ DeepVar (vs !! i)
          Nothing ->
            throwTypeError
              $  "unknown data constructor type variable: "
              ++ show tvid
    dcurr <- substT t
    return $ DeepStructure (TyConS (fromString "->") [dcurr, d])
  dconConstraint c (DConId ident, tcid, tvs, ts) = do
    let vid  = VarId ident
        defC = let1
          vid
          (\w -> existn
            (length tvs)
            (\vs -> do
              deepstruc <- dconDeepStruc tcid tvs ts vs
              deep deepstruc (w -=-)
            )
          )
          c
    defC <$$> \(_, _, _, d) -> d

lookupLitStruc :: Literal -> Structure Variable
lookupLitStruc (LitIntegral _) = TyConS (fromString "Int32") []
lookupLitStruc LitEvent        = TyConS (fromString "()") []

lookupLitType :: Literal -> Type
lookupLitType (LitIntegral _) = I32
lookupLitType LitEvent        = Unit

hastype :: Expr Annotations -> Variable -> SolverM s (Co (Expr Type))
hastype (Var x _) w =
  inst x w <$$> \((gs, t), witnesses) -> Var x (subTVars gs witnesses t)
hastype (Data dcid@(DConId ident) _) w = inst (VarId ident) w
  <$$> \((gs, t), witnesses) -> Data dcid (subTVars gs witnesses t)
hastype (Lit l _) w =
  w -==- lookupLitStruc l <$$> \_ -> Lit l (lookupLitType l)
hastype (Lambda x u _) w =
  exist
      (\v1 -> exist
        (\v2 -> do
          c1 <- w -==- TyConS (fromString "->") [v1, v2]
          c2 <- hastype u v2
          x' <- unBinder x
          let c3 = def x' v1 c2
          return $ c1 ^& c3 ^& CDecode w
        )
      )
    <$$> \((_, e), t) -> Lambda x e t
hastype (App e1 e2 _) w =
  exist
      (\v -> do
        c1 <- liftB hastype e1 (TyConS (fromString "->") [v, w])
        c2 <- hastype e2 v
        return $ c1 ^& c2 ^& CDecode w
      )
    <$$> \((e1', e2'), t) -> App e1' e2' t
hastype (Let binds u _) w =
  (do
      let binders = map fst binds
          exprs   = map snd binds
      names <- mapM unBinder binders
      let binds' = zip names exprs
      cu <- hastype u w
      letn names (\vs -> hastypeExprs binds' vs (return . defDefs binds' vs)) cu
    )
    <$$> \(_, _, binds', u') ->
           let binds'' = [ (n, e) | ((n, _), (_, e)) <- zip binds binds' ]
           in  Let binds'' u' (extract u')
hastype (Match e branches _) w =
  exist
      (\v -> hastypeBranches
        branches
        w
        v
        (\branches' -> do
          c1 <- hastype e v
          let ty = CDecode w
          return $ c1 ^& branches' ^& ty
        )
      )
    <$$> \((e', branches'), t) -> Match e' branches' t
hastype (Prim prim es _) w = hastypePrim prim es w

hastypePrim
  :: Primitive -> [Expr Annotations] -> Variable -> SolverM s (Co (Expr Type))
hastypePrim prim es w =
  let primConstraints' = primConstraints prim es w
  in
    case prim of
      New -> primConstraints'
        (\[v1] -> Just $ DeepStructure (TyConS (TConId "&") [DeepVar v1]))
        (const [Nothing])
      Dup  -> primConstraints' (Just . DeepVar . head) (const [Nothing])
      Drop -> primConstraints'
        (const $ Just $ DeepStructure $ TyConS "()" [])
        (const [Nothing])
      Deref ->
        -- ( do
        --     c1 <- liftB hastype (head es) (TyConS (fromString "&") [w])
        --     return $ c1 ^& CDecode w
        -- )
        --   <$$> \(e, t) -> Prim prim [e] t
        -- ( do
        --     c <- hastype (head es) w
        --     return $ c ^& CDecode w
        -- )
        --   <$$> \(e, t) -> Prim prim [e] t

        -- deep
        --   (DeepStructure (TyConS (fromString "&") [DeepVar w]))
        --   ( \v -> do
        --       c1 <- hastype (head es) v
        --       return $ c1 ^& CDecode w
        --   )
        --   <$$> \(e, t) ->
        --     Prim
        --       prim
        --       [e]
        --       t
        -- existn
        --   2
        --   ( \[v1, v2] -> do
        --       c1 <- w -=- v2
        --       c2 <-
        --         deep
        --           (DeepStructure (TyConS (fromString "&") [DeepVar v2]))
        --           (\v' -> v1 -=- v')
        --       c3 <- hastype (head es) v1
        --       return $ c1 ^& c2 ^& c3
        --   )
        --   <$$> \(_, e) ->
        --     Prim
        --       prim
        --       [e]
        --       ( case extract e of
        --           TCon "&" [t] -> trace (show t) t
        --           _ -> error $ show e
        --       )
               primConstraints'
        (const Nothing)
        (const [Just $ DeepStructure $ TyConS "&" [DeepVar w]])
      -- exist
      --   ( \v ->
      --       primConstraints'
      --         (const $ Just $ DeepVar v)
      --         (const [Just $ DeepStructure $ TyConS "&" [DeepVar v]])
      --   )
      Assign -> primConstraints'
        (const $ Just $ DeepStructure $ TyConS "()" [])
        (\[_, v2] -> [Just $ DeepStructure $ TyConS "&" [DeepVar v2], Nothing])
      After -> primConstraints'
        (const $ Just $ DeepStructure $ TyConS "()" [])
        (\[_, _, v3] ->
          [ Just $ DeepStructure $ TyConS "Int32" []
          , Just $ DeepStructure $ TyConS "&" [DeepVar v3]
          , Nothing
          ]
        )
      Par  -> primConstraints' (Just . tupleDeepStructure) (map $ const Nothing)
      Wait -> primConstraints'
        (const $ Just $ DeepStructure $ TyConS "()" [])
        (map $ const Nothing)
      Loop -> primConstraints'
        (const $ Just $ DeepStructure $ TyConS "()" [])
        (const [Nothing])
      Break -> primConstraints'
        (const $ Just $ DeepStructure $ TyConS "()" [])
        (const [])
      Now -> primConstraints'
        (const $ Just $ DeepStructure $ TyConS "Int32" [])
        (const [Just $ DeepStructure $ TyConS "()" []])
      PrimOp  primOp -> hastypePrimOp primOp es w
      CQuote  _      -> primConstraints' (const Nothing) (const [])
      CCall   _      -> primConstraints' (const Nothing) (const [])
      FfiCall vid    -> undefined -- TODO

tupleDeepStructure :: [Variable] -> DeepType
tupleDeepStructure vs =
  DeepStructure $ TyConS (tupleId $ length vs) (map DeepVar vs)

hastypePrimOp
  :: PrimOp -> [Expr Annotations] -> Variable -> SolverM s (Co (Expr Type))
hastypePrimOp primOp es w =
  let primConstraints' = primConstraints (PrimOp primOp) es w
      unaryPrimOp      = primConstraints'
        (const (Just $ DeepStructure $ TyConS (TConId "Int32") []))
        (const [Just $ DeepStructure $ TyConS (TConId "Int32") []])
      binaryPrimOp = primConstraints'
        (const (Just $ DeepStructure $ TyConS (TConId "Int32") []))
        (const
          [ Just $ DeepStructure $ TyConS (TConId "Int32") []
          , Just $ DeepStructure $ TyConS (TConId "Int32") []
          ]
        )
  in  case primOp of
        PrimNeg    -> unaryPrimOp
        PrimNot    -> unaryPrimOp
        PrimBitNot -> unaryPrimOp
        PrimAdd    -> binaryPrimOp
        PrimSub    -> binaryPrimOp
        PrimMul    -> binaryPrimOp
        PrimDiv    -> binaryPrimOp
        PrimMod    -> binaryPrimOp
        PrimBitAnd -> binaryPrimOp
        PrimBitOr  -> binaryPrimOp
        PrimEq     -> binaryPrimOp
        PrimNeq    -> binaryPrimOp
        PrimGt     -> binaryPrimOp
        PrimGe     -> binaryPrimOp
        PrimLt     -> binaryPrimOp
        PrimLe     -> binaryPrimOp

primConstraints
  :: Primitive
  -> [Expr Annotations]
  -> Variable
  -> ([Variable] -> Maybe DeepType)
  -> ([Variable] -> [Maybe DeepType])
  -> SolverM s (Co (Expr Type))
primConstraints prim es w struc strucs = do
  existn
    (length es)
    (\vs -> do
      unless (length es == length (strucs vs))
        $ throwIncorrectPrimArguments prim
      mapMlater
        perE
        (zip3 es vs (strucs vs))
        (\ces ->
          (do
              c1 <- case struc vs of
                Just struc' -> deep struc' (w -=-)
                Nothing     -> return S.CTrue
              let c2 = CDecode w
              return $ c1 ^& ces ^& c2
            )
            <$$> \((_, es'), t) -> Prim prim es' t
        )
    )
 where
  perE (e, v, struc') k = do
    c <-
      (do
          c1 <- hastype e v
          c2 <- case struc' of
            Just struc'' -> deep struc'' (v -=-)
            Nothing      -> return S.CTrue
          return $ c1 ^& c2
        )
        <$$> fst
    k c

throwIncorrectPrimArguments :: Primitive -> SolverM s a
throwIncorrectPrimArguments prim =
  throwTypeError $ "incorrect argument number for " ++ show prim

throwIncorrectPrimOpArguments :: PrimOp -> SolverM s a
throwIncorrectPrimOpArguments primOp =
  throwTypeError $ "incorrect argument number for " ++ show primOp

unBinder :: Binder -> SolverM s VarId
unBinder = maybe freshName return

freshName :: SolverM s VarId
freshName = do
  ctx <- get
  put ctx { currNameId = currNameId ctx + 1 }
  return . VarId . fromString $ "_type_pass_anon_var" ++ show (currNameId ctx)

hastypeBranches
  :: [(Alt, Expr Annotations)]
  -> Variable
  -> Variable
  -> SBinder s (Co [(Alt, Expr Type)]) r
hastypeBranches branches vReturn vScrutinee = mapMlater onBranch branches
 where
  onBranch :: (Alt, Expr Annotations) -> SBinder s (Co (Alt, Expr Type)) r
  onBranch (p@(AltLit lit), u) k = do
    res <-
      (do
          c1 <- vScrutinee -==- lookupLitStruc lit
          c2 <- hastype u vReturn
          return $ c1 ^& c2
        )
        <$$> \(_, u') -> (p, u')
    k res
  onBranch (p@(AltDefault Nothing), u) k = do
    res <- hastype u vReturn <$$> \u' -> (p, u')
    k res
  onBranch (p@(AltData dcid binders), u) k = do
    hastypeBranchData dcid
                      binders
                      u
                      vReturn
                      vScrutinee
                      (k . fmap (\u' -> (p, u')))

hastypeBranchData
  :: DConId
  -> [Binder]
  -> Expr Annotations
  -> Variable
  -> Variable
  -> SBinder s (Co (Expr Type)) r
hastypeBranchData dcid binders u vReturn vScrutinee k = do
  res <-
    (do
        vids <- mapM unBinder binders
        existn
          (length vids)
          (\vs -> do
            cmap <- dconMap . solverGen <$> get
            case M.lookup dcid cmap of
              Just (_, tcid, tvs, ts) -> existn
                (length tvs)
                (\dataVs -> do
                  let scrutineeType =
                        DeepStructure (TyConS tcid (map DeepVar dataVs))
                  cScrutinee <- deep scrutineeType (vScrutinee -=-)
                  cInner <- hastype u vReturn
                  cData <- foldM (defAltVar tvs dataVs) cInner (zip3 vids ts vs)
                  return $ cScrutinee ^& cData
                )
              Nothing ->
                throwTypeError $ "Alt data pattern does not exist: " ++ show dcid
          )
      )
      <$$> \(_, inner) -> inner
  k res
 where
  defAltVar tvs dataVs cInner (vid, t, v) = do
    let substT (TCon tcid' ts') = do
          ts'' <- mapM substT ts'
          return $ DeepStructure (TyConS tcid' ts'')
        substT (TVar tvid) = case elemIndex tvid tvs of
          Just i -> return $ DeepVar (dataVs !! i)
          Nothing ->
            throwTypeError
              $  "unknown data constructor type variable: "
              ++ show tvid
    deepT <- substT t
    (deep
        deepT
        (\w -> do
          c1 <- v -=- w
          return $ c1 ^& def vid v cInner
        )
      )
      <$$> \(_, e) -> e

hastypeExprs
  :: [(VarId, Expr Annotations)]
  -> [Variable]
  -> SBinder s (Co [(VarId, Expr Type)]) r
hastypeExprs defs vs = mapMlater onDef (zip defs vs)
 where
  onDef
    :: ((VarId, Expr Annotations), Variable)
    -> SBinder s (Co (VarId, Expr Type)) r
  onDef ((name, e), v) k = do
    c <- hastype e v <$$> \e' -> (name, e')
    k c

existn :: Int -> SBinder s [Variable] r
existn k = mapMnow onDef [1 .. k]
 where
  onDef :: a -> SBinder s Variable r
  onDef _ = exist

hastypeDefs
  :: [(VarId, Expr Annotations)] -> SolverM s (Co [(VarId, Expr Type)])
hastypeDefs defs =
  existn (length defs) (\vs -> hastypeExprs defs vs (return . defDefs defs vs))

defDefs
  :: [(VarId, Expr Annotations)]
  -> [Variable]
  -> Co [(VarId, Expr Type)]
  -> Co [(VarId, Expr Type)]
defDefs defs vs cdefs = foldl addDef cdefs (zip defs vs)
  where addDef c ((name, _), v) = CDef name v c

subTVars :: [TVarId] -> [Type] -> Type -> Type
subTVars gs ws = subTVars'
 where
  subTVars' (TCon tcid ts) = TCon tcid (map subTVars' ts)
  subTVars' (TVar x) =
    let iopt = elemIndex x gs
    in  case iopt of
          Just i  -> ws !! i
          Nothing -> TVar x

mapMnow :: (a -> SBinder s b r) -> [a] -> SBinder s [b] r
mapMnow _ []       k = k []
mapMnow f (x : xs) k = f x (\y -> mapMnow f xs (\ys -> k (y : ys)))

mapMlater :: (a -> SBinder s (Co b) r) -> [a] -> SBinder s (Co [b]) r
mapMlater _ [] k = k (pure [])
mapMlater f (x : xs) k =
  f x (\y -> mapMlater f xs (\ys -> k (fmap (uncurry (:)) (y ^& ys))))
