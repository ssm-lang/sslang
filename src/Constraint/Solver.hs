{-# LANGUAGE GADTs #-}

module Constraint.Solver where

import Common.Compiler (Error (..), throwError)
import Common.Identifiers (TVarId (..), fromString)
import qualified Constraint.Decoder as D
import qualified Constraint.Generalization as G
import qualified Constraint.ShadowMap as SM
import Constraint.SolverM (SolverM)
import Constraint.Structure (Structure (..))
import qualified Constraint.Unifier as U
import Constraint.Utils (throwTypeError)
import Control.Monad (unless, void, zipWithM, zipWithM_)
import IR.IR (VarId (..))
import qualified IR.IR as I

type Variable = Int

type Scheme = (TVarId, I.Type)

type OnSolve s a = SolverM s a

data Co a where
  CTrue :: Co ()
  CMap :: Co a -> (a -> b) -> Co b
  CPure :: a -> Co a
  CConj :: Co a -> Co b -> Co (a, b)
  CEq :: Variable -> Variable -> Co ()
  CExist :: Variable -> Maybe (Structure Variable) -> Co a -> Co a
  CDecode :: Variable -> Co I.Type
  CInstance :: VarId -> Variable -> Co [I.Type]
  CDef :: VarId -> Variable -> Co a -> Co a
  CLet :: [Variable] -> [VarId] -> [Variable] -> Co a -> Co b -> Co ([TVarId], [Scheme], a, b)

solveAndElab :: Co (I.Expr I.Type) -> SolverM s (I.Expr I.Type)
solveAndElab c = do
  unless (ok c) (throwError . TypeError . fromString $ "Solver: ill-formed toplevel constraint")
  ctx <- initCtx
  r <- solve ctx c
  r

ok :: Co a -> Bool
ok CTrue = True
ok (CPure _) = True
ok (CMap c _) = ok c
ok (CLet _ _ _ _ c2) = ok c2
ok (CConj c1 c2) = ok c1 && ok c2
ok _ = False

solve :: Ctx s -> Co a -> SolverM s (OnSolve s a)
solve ctx = solve'
  where
    solve' CTrue = return $ return ()
    solve' (CPure x) = return $ return x
    solve' (CMap c f) = do
      r <- solve ctx c
      return $ f <$> r
    solve' (CConj c1 c2) = do
      r1 <- solve ctx c1
      r2 <- solve ctx c2
      return $ do
        a1 <- r1
        a2 <- r2
        return (a1, a2)
    solve' (CEq v w) = do
      uv <- uvar ctx v
      uw <- uvar ctx w
      U.unify uv uw
      return $ return ()
    solve' (CExist v s c) = do
      void $ flexible ctx v s
      result <- solve ctx c
      uunbind ctx v
      return result
    solve' (CDecode v) = do
      uv <- uvar ctx v
      return $ decode ctx uv
    solve' (CInstance x w) = do
      s <- elookup ctx x
      (witnesses, v) <- G.instantiate s
      uw <- uvar ctx w
      U.unify v uw
      return $ mapM (decode ctx) witnesses
    solve' (CDef x v c) = do
      uv <- uvar ctx v
      s <- G.trivial uv
      bind ctx x s
      r <- solve ctx c
      unbind ctx x
      return r
    solve' (CLet rs xs vs c1 c2) = do
      enter
      urs <- mapM (rigid ctx) rs
      uvs <- mapM (\v -> flexible ctx v Nothing) vs
      r1 <- solve ctx c1
      (gammas, ss) <- exit uvs
      unless (all (`elem` gammas) urs) $ throwTypeError "all rigid variables of [rs] must be generalizable"
      mapM_ (uunbind ctx) vs
      zipWithM_ (bind ctx) xs ss
      r2 <- solve ctx c2
      mapM_ (unbind ctx) xs
      return $ do
        gammas' <- mapM D.decodeVariable gammas
        ss' <- mapM decodeScheme ss
        r1' <- r1
        r2' <- r2
        return (gammas', ss', r1', r2')

decodeScheme :: G.Scheme s -> SolverM s Scheme
decodeScheme = undefined

enter :: SolverM s ()
enter = undefined

exit :: [U.Variable s] -> SolverM s ([U.Variable s], [G.Scheme s])
exit = undefined

decode :: Ctx s -> U.Variable s -> SolverM s I.Type
decode (Ctx {ctxDecoder = dec}) = D.decode dec

data Ctx s = Ctx
  { ctxEnv :: Env s,
    ctxTab :: Tab s,
    ctxDecoder :: D.Decoder s
  }

initCtx :: SolverM s (Ctx s)
initCtx = do
  env <- initEnv
  tab <- initTab
  dec <- D.initDecoder
  return
    Ctx
      { ctxEnv = env,
        ctxTab = tab,
        ctxDecoder = dec
      }

-- | *Env*
-- |
-- | The solver carries an environment, an immutable mapping of term variables
-- | to type schemes.
type Env s = SM.Map s VarId (G.Scheme s)

initEnv :: SolverM s (Env s)
initEnv = SM.new

elookup :: Ctx s -> VarId -> SolverM s (G.Scheme s)
elookup (Ctx {ctxEnv = env}) vid = do
  res <- SM.lookup env vid
  case res of
    Just s -> return s
    Nothing -> throwTypeError $ "unvound variable: " ++ show vid

bind :: Ctx s -> VarId -> G.Scheme s -> SolverM s ()
bind (Ctx {ctxEnv = env}) = SM.add env

unbind :: Ctx s -> VarId -> SolverM s ()
unbind (Ctx {ctxEnv = env}) = SM.remove env

-- | *Tab*
-- |
-- | The solver maintains a mutable mapping of immutable type variables to
-- | unifier variables.
-- |
-- | [uvar] looks up the mapping. [flexible] and [rigid] create a new unifier
-- | variable and extend the mapping. [uunbind] removes a mapping. *)
-- type Table s = M.Map Variable (U.Variable s)

-- type TableRef s = STRef s (Table s)
type Tab s = SM.Map s Variable (U.Variable s)

initTab :: SolverM s (Tab s)
initTab = SM.new

uvar :: Ctx s -> Variable -> SolverM s (U.Variable s)
uvar (Ctx {ctxTab = tab}) v = do
  res <- SM.lookup tab v
  case res of
    Just uv -> return uv
    Nothing -> throwTypeError $ "unbound type variable: " ++ show v

flexible :: Ctx s -> Variable -> Maybe (Structure Variable) -> SolverM s (U.Variable s)
flexible ctx@(Ctx {ctxTab = tab}) v so = do
  notmem <- SM.notMember tab v
  unless notmem $ throwTypeError $ "variable already in table: " ++ show v
  so' <- mapM (mapM (uvar ctx)) so
  uv <- G.flexible so'
  SM.add tab v uv
  return uv

rigid :: Ctx s -> Variable -> SolverM s (U.Variable s)
rigid (Ctx {ctxTab = tab}) v = do
  notmem <- SM.notMember tab v
  unless notmem $ throwTypeError $ "variable already in table: " ++ show v
  uv <- G.rigid
  SM.add tab v uv
  return uv

uunbind :: Ctx s -> Variable -> SolverM s ()
uunbind (Ctx {ctxTab = tab}) = SM.remove tab
