{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.DesugarPattern
  ( desugarPattern
  ) where

import           Common.Compiler               as Compiler
                                                ( Error(PatternError)
                                                , MonadError(throwError)
                                                , Pass
                                                , fromString
                                                )
import           Common.Identifiers             ( Identifier(..) )
import           Control.Monad                  ( replicateM )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( Bifunctor(second)
                                                , first
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import qualified Data.Set                      as S
import qualified IR.IR                         as I


type Equation = ([I.Alt], I.Expr I.Type)

data CInfo = CInfo
  { cName    :: I.DConId
  , cType    :: I.TConId
  , argsType :: [I.Type]
  , cArity   :: Int
  }
  deriving (Eq, Show)

data TInfo = TInfo
  { tName :: I.TConId
  , tCSet :: S.Set I.DConId
  }
  deriving (Eq, Show)

data DesugarCtx = DesugarCtx
  { typeMap   :: M.Map I.TConId TInfo
  , consMap   :: M.Map I.DConId CInfo
  , anonCount :: Int
  }

newtype DesugarFn a = DesugarFn (StateT DesugarCtx Compiler.Pass a)
  deriving Functor                      via (StateT DesugarCtx Compiler.Pass)
  deriving Applicative                  via (StateT DesugarCtx Compiler.Pass)
  deriving Monad                        via (StateT DesugarCtx Compiler.Pass)
  deriving MonadFail                    via (StateT DesugarCtx Compiler.Pass)
  deriving (MonadError Error)  via (StateT DesugarCtx Compiler.Pass)
  deriving (MonadState DesugarCtx)     via (StateT DesugarCtx Compiler.Pass)

runDesugarFn :: DesugarFn a -> DesugarCtx -> Compiler.Pass a
runDesugarFn (DesugarFn m) = evalStateT m

freshVar :: DesugarFn Identifier
freshVar = do
  currCount <- gets anonCount
  modify $ \ctx -> ctx { anonCount = anonCount ctx + 1 }
  return $ fromString ("__pat_anon" ++ show currCount)

buildCtx :: [(I.TConId, I.TypeDef)] -> DesugarCtx
buildCtx tds = DesugarCtx { anonCount = 0
                          , typeMap   = buildTypeMap tds
                          , consMap   = buildConsMap tds
                          }


desugarPattern :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
desugarPattern p@I.Program { I.programDefs = defs, I.typeDefs = tds } =
  runDesugarFn
    (do
      defs' <- desugarExprsDefs defs
      return $ p { I.programDefs = defs' }
    )
    ctx
  where ctx = buildCtx tds

desugarExprsDefs
  :: [(I.VarId, I.Expr I.Type)] -> DesugarFn [(I.VarId, I.Expr I.Type)]
desugarExprsDefs defs = do
  es' <- mapM desugarExpr es
  return $ zip vs es'
  where (vs, es) = unzip defs

desugarExpr :: I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarExpr e@(I.Var  _ _) = return e
desugarExpr e@(I.Data _ _) = return e
desugarExpr e@(I.Lit  _ _) = return e
desugarExpr (I.App e1 e2 t) =
  I.App <$> desugarExpr e1 <*> desugarExpr e2 <*> do
    return t
desugarExpr (I.Let bins e t) =
  I.Let
    <$> (do
          let (bs, es) = unzip bins
          es' <- mapM desugarExpr es
          return $ zip bs es'
        )
    <*> desugarExpr e
    <*> (do
          return t
        )
desugarExpr (I.Lambda b e t) = I.Lambda b <$> desugarExpr e <*> do
  return t
desugarExpr (I.Prim p es t) = I.Prim p <$> mapM desugarExpr es <*> do
  return t
desugarExpr (I.Match e arms t) = do
  arms' <- mapM
    (\(a, body) -> do
      body' <- desugarExpr body
      return (a, body')
    )
    arms
  let armsForDesugar = map (first (: [])) arms'
  case e of
    I.Var _ _ -> desugarMatch [e] armsForDesugar (I.NoExpr t)   -- TODO: add let alias
    _         -> do
      v <- freshVar
      let var = I.VarId v
          vt  = I.extract e
      singleLet var e
        <$> desugarMatch [I.Var var vt] armsForDesugar (I.NoExpr t)
        <*> do
              return vt
desugarExpr e@(I.NoExpr _) = return e

desugarMatch
  :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatch [] []            def = return def
desugarMatch [] (([], e) : _) _   = return e
desugarMatch [] _             _   = error "can't happen 1"
desugarMatch us qs            def = do
  foldrM (desugarMatchGen us) def (partitionEqs qs)

desugarMatchGen
  :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchGen us qs def | isVarEq (head qs)  = desugarMatchVar us qs def
                          | isConsEq (head qs) = desugarMatchCons us qs def
                          | isLitEq (head qs)  = desugarMatchLit us qs def
                          | isWildEq (head qs) = desugarMatchWild us qs def
                          | otherwise          = error "can't happen 2"
 where
  isWildEq (as, _) = case head as of
    I.AltBinder b -> isNothing b
    _             -> False
  isVarEq (as, _) = case head as of
    I.AltBinder b -> isJust b
    _             -> False
  isConsEq (as, _) = case head as of
    I.AltData _ _ -> True
    _             -> False
  isLitEq (as, _) = case head as of
    I.AltLit _ -> True
    _          -> False

desugarMatchVar
  :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchVar []       _  _   = error "can't happen 3"
desugarMatchVar (u : us) qs def = desugarMatch
  us
  [ (ps, singleAlias v u e) | (I.AltBinder v : ps, e) <- qs ]
  def

desugarMatchWild
  :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchWild [] _ _ = error "can't happen 4"
desugarMatchWild (_ : us) qs def =
  desugarMatch us [ (ps, e) | (_ : ps, e) <- qs ] def

desugarMatchCons
  :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchCons (u : us) qs def = do
  cs   <- getConstructors . getCon . head $ qs
  arms <- sequence [ desugarArm c (choose c) | c <- S.toList cs ]
  return $ I.Match u arms (I.extract def)
 where
  getCon ((I.AltData dcon _) : _, _) = dcon
  getCon _                           = error "can't happen 5"
  choose c = [ q | q <- qs, getCon q == c ]
  desugarArm :: I.DConId -> [Equation] -> DesugarFn (I.Alt, I.Expr I.Type)
  desugarArm dcon qs' = do
    k      <- getArity dcon
    newIds <- replicateM k freshVar
    cinfo  <- getCInfo dcon
    let newVars  = map I.VarId newIds
        argsTyps = argsType cinfo
        makeVar (i, t) = do
          return (I.Var i t)
        makeBinder (I.Var vid _) = I.AltBinder $ Just vid
        makeBinder _             = error "can't happen 6"
    us' <- mapM makeVar $ zip newVars argsTyps
    case qs' of
      [] -> return (I.AltData dcon (map makeBinder us'), def)
      _  -> do
        body <- desugarMatch
          (us' ++ us)
          [ (as' ++ as, e) | ((I.AltData _ as') : as, e) <- qs' ]
          def
        return (I.AltData dcon (map makeBinder us'), body)
desugarMatchCons _ _ _ = error "can't happen 7"

desugarMatchLit
  :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchLit (u : us) qs def = do
  arms <- sequence [ desugarArm a as e | (a : as, e) <- qs ]
  let arms' = arms ++ [(I.AltBinder Nothing, def)] -- WARN: assuming that PatLit is never exhaustive, adding a default case
  return $ I.Match u arms' (I.extract def)
 where
  desugarArm a as e = do
    body <- desugarMatch us [(as, e)] def
    return (a, body)
desugarMatchLit _ _ _ = error "can't happen 8"


partitionEqs :: [Equation] -> [[Equation]]
partitionEqs []  = []
partitionEqs [x] = [[x]]
partitionEqs (x : x' : xs) | sameGroup x x' = tack x (partitionEqs (x' : xs))
                           | otherwise      = [x] : partitionEqs (x' : xs)
 where
  tack y yss = (y : head yss) : tail yss
  sameGroup (as, _) (as', _) = case (head as, head as') of
    (I.AltLit    _, I.AltLit _   ) -> True
    (I.AltBinder _, I.AltBinder _) -> True
    (I.AltData _ _, I.AltData _ _) -> True
    _                              -> False

singleLet
  :: I.VarId -> I.Expr I.Type -> I.Expr I.Type -> I.Type -> I.Expr I.Type
singleLet i e = I.Let [(Just i, e)]

singleAlias :: I.Binder -> I.Expr I.Type -> I.Expr I.Type -> I.Expr I.Type
singleAlias alias i e = I.Let [(alias, i)] e (I.extract e)

getConstructors :: I.DConId -> DesugarFn (S.Set I.DConId)
getConstructors dcon = do
  c <- getCInfo dcon
  t <- getTInfo (cType c)
  return $ tCSet t

getArity :: I.DConId -> DesugarFn Int
getArity dcon = do
  c <- getCInfo dcon
  return $ cArity c

getCInfo :: I.DConId -> DesugarFn CInfo
getCInfo dcon = do
  cm <- gets $ M.lookup dcon . consMap
  maybe throwDesugarError return cm

getTInfo :: I.TConId -> DesugarFn TInfo
getTInfo tcon = do
  tm <- gets $ M.lookup tcon . typeMap
  maybe throwDesugarError return tm

throwDesugarError :: DesugarFn a
throwDesugarError = throwError $ PatternError "Can't desugar pattern match"

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d []       = return d
foldrM f d (x : xs) = f x =<< foldrM f d xs

buildTypeMap :: [(I.TConId, I.TypeDef)] -> M.Map I.TConId TInfo
buildTypeMap = foldr tAcc M.empty
 where
  tAcc td tmap' =
    let typ   = fst td
        clist = map fst (I.variants $ snd td)
        cset  = S.fromList clist
    in  M.insert typ (TInfo { tName = typ, tCSet = cset }) tmap'

buildConsMap :: [(I.TConId, I.TypeDef)] -> M.Map I.DConId CInfo
buildConsMap args = foldr buildConsMap' M.empty args'
  where args' = map (second I.variants) args

buildConsMap'
  :: (I.TConId, [(I.DConId, I.TypeVariant)])
  -> M.Map I.DConId CInfo
  -> M.Map I.DConId CInfo
buildConsMap' arg m = foldr cAcc m (snd arg)
 where
  typ = fst arg
  getTypes (_, I.VariantUnnamed typs) = typs
  getTypes _                          = error "cannot happen"
  cAcc cd cmap' = M.insert cid c cmap'
   where
    cid       = fst cd
    argsType' = getTypes cd
    c         = CInfo { cName    = cid
                      , cType    = typ
                      , argsType = argsType'
                      , cArity   = length argsType'
                      }
