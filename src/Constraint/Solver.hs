{-# LANGUAGE GADTs #-}

module Constraint.Solver where

import           Common.Compiler                ( Error(..)
                                                , throwError
                                                )
import           Common.Identifiers             ( TVarId(..)
                                                , fromString
                                                )
import qualified Constraint.Decoder            as D
import qualified Constraint.Generalization     as G
import qualified Constraint.ShadowMap          as SM
import           Constraint.SolverM             ( SolverM )
import           Constraint.Structure           ( Structure(..) )
import qualified Constraint.Unifier            as U
import           Constraint.Utils               ( modifySTRef
                                                , throwTypeError
                                                )
import           Control.Monad                  ( replicateM
                                                , unless
                                                , void
                                                , zipWithM_
                                                )
import           Control.Monad.ST.Trans         ( newSTRef
                                                , readSTRef
                                                )
import           IR.IR                          ( VarId(..) )
import qualified IR.IR                         as I

type Variable = Int

type Scheme = ([TVarId], I.Type)

type OnSolve s a = SolverM s a

data Co a where
  CTrue :: Co ()
  CMap :: Co a -> (a -> b) -> Co b
  CPure :: a -> Co a
  CConj :: Co a -> Co b -> Co (a, b)
  CEq :: Variable -> Variable -> Co ()
  CExist :: Variable -> Maybe (Structure Variable) -> Co a -> Co a
  CDecode :: Variable -> Co I.Type
  CInstance :: VarId -> Variable -> Co (Scheme, [I.Type])
  CDef :: VarId -> Variable -> Co a -> Co a
  CLet :: [Variable] -> [VarId] -> [Variable] -> Co a -> Co b -> Co ([TVarId], [Scheme], a, b)

instance Show (Co a) where
  show CTrue             = "(CTrue)"
  show (CMap c _       ) = "(CMap " ++ show c ++ ")"
  show (CPure _        ) = "(CPure)"
  show (CConj c1 c2    ) = "(" ++ show c1 ++ " ^& " ++ show c2 ++ ")"
  show (CEq   v1 v2    ) = "(CEq " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (CExist v _ c   ) = "(CExist " ++ show v ++ " " ++ show c ++ ")"
  show (CDecode v      ) = "(CDecode " ++ show v ++ ")"
  show (CInstance vid v) = "(CInstance " ++ show vid ++ " " ++ show v ++ ")"
  show (CDef vid v c) =
    "(CDef " ++ show vid ++ " " ++ show v ++ " " ++ show c ++ ")"
  show (CLet vs vids vs' c1 c2) =
    "(CLet "
      ++ show vs
      ++ " "
      ++ show vids
      ++ " "
      ++ show vs'
      ++ " "
      ++ show c1
      ++ " "
      ++ show c2
      ++ ")"

instance Functor Co where
  fmap f c = CMap c f

instance Applicative Co where
  pure = CPure
  mf <*> mx = fmap (\(f, x) -> f x) (CConj mf mx)

-- solveAndElab :: Co (I.Expr I.Type) -> SolverM s (I.Expr I.Type)
solveAndElab :: Co (I.Program I.Type) -> SolverM s (I.Program I.Type)
solveAndElab c = do
  unless
    (ok c)
    ( throwError
    . TypeError
    . fromString
    $ "Solver: ill-formed toplevel constraint"
    )
  ctx <- initCtx
  r   <- solve ctx c
  r

ok :: Co a -> Bool
ok CTrue             = True
ok (CPure _        ) = True
ok (CMap c _       ) = ok c
ok (CLet _ _ _ _ c2) = ok c2
ok (CConj c1 c2    ) = ok c1 && ok c2
ok _                 = False

solve :: Ctx s -> Co a -> SolverM s (OnSolve s a)
solve ctx = solve'
 where
  solve' CTrue      = return $ return ()
  solve' (CPure x ) = return $ return x
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
    s              <- elookup ctx x
    (witnesses, v) <- instantiate ctx s
    uw             <- uvar ctx w
    U.unify v uw
    return $ do
      s'         <- decodeScheme ctx s
      witnesses' <- mapM (decode ctx) witnesses
      return (s', witnesses')
  solve' (CDef x v c) = do
    uv <- uvar ctx v
    let s = G.trivial uv
    bind ctx x s
    r <- solve ctx c
    unbind ctx x
    return r
  -- res <- r
  -- t <- decode ctx uv
  -- return (t, res)
  solve' (CLet rs xs vs c1 c2) = do
    enter ctx
    urs          <- mapM (rigid ctx) rs
    uvs          <- mapM (\v -> flexible ctx v Nothing) vs
    r1           <- solve ctx c1
    (gammas, ss) <- exit ctx uvs
    unless (all (`elem` gammas) urs)
      $ throwTypeError "all rigid variables of [rs] must be generalizable"
    mapM_ (uunbind ctx) vs
    zipWithM_ (bind ctx) xs ss
    r2 <- solve ctx c2
    mapM_ (unbind ctx) xs
    return $ do
      gammas' <- mapM D.decodeVariable gammas
      ss'     <- mapM (decodeScheme ctx) ss
      r1'     <- r1
      r2'     <- r2
      return (gammas', ss', r1', r2')

data Ctx s = Ctx
  { ctxEnv     :: Env s
  , ctxTab     :: Tab s
  , ctxDecoder :: D.Decoder s
  , ctxGen     :: G.Gen s
  }

initCtx :: SolverM s (Ctx s)
initCtx = do
  env <- initEnv
  tab <- initTab
  dec <- D.initDecoder
  gen <- G.initGen
  return Ctx { ctxEnv = env, ctxTab = tab, ctxDecoder = dec, ctxGen = gen }

-- | auxilliary mappings
enter :: Ctx s -> SolverM s ()
enter (Ctx { ctxGen = gen }) = G.enter gen

exit :: Ctx s -> [U.Variable s] -> SolverM s ([U.Variable s], [G.Scheme s])
exit (Ctx { ctxGen = gen }) = G.exit gen

instantiate :: Ctx s -> G.Scheme s -> SolverM s ([U.Variable s], U.Variable s)
instantiate (Ctx { ctxGen = gen }) = G.instantiate gen

decode :: Ctx s -> U.Variable s -> SolverM s I.Type
decode (Ctx { ctxDecoder = dec }) = D.decode dec

decodeScheme :: Ctx s -> G.Scheme s -> SolverM s Scheme
decodeScheme Ctx { ctxDecoder = dec } s = do
  tvs <- mapM D.decodeVariable (G.schemeQuantifiers s)
  t   <- D.decode dec (G.schemeRoot s)
  return (tvs, t)

-- | *Env*
-- |
-- | The solver carries an environment, an immutable mapping of term variables
-- | to type schemes.
type Env s = SM.Map s VarId (G.Scheme s)

initEnv :: SolverM s (Env s)
initEnv = SM.new

elookup :: Ctx s -> VarId -> SolverM s (G.Scheme s)
elookup (Ctx { ctxEnv = env }) vid = do
  res <- SM.lookup env vid
  case res of
    Just s  -> return s
    Nothing -> throwTypeError $ "unbound variable: " ++ show vid

bind :: Ctx s -> VarId -> G.Scheme s -> SolverM s ()
bind (Ctx { ctxEnv = env }) = SM.add env

unbind :: Ctx s -> VarId -> SolverM s ()
unbind (Ctx { ctxEnv = env }) = SM.remove env

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
uvar (Ctx { ctxTab = tab }) v = do
  res <- SM.lookup tab v
  case res of
    Just uv -> return uv
    Nothing -> throwTypeError $ "unbound type variable: " ++ show v

flexible
  :: Ctx s -> Variable -> Maybe (Structure Variable) -> SolverM s (U.Variable s)
flexible ctx@(Ctx { ctxTab = tab, ctxGen = gen }) v so = do
  notmem <- SM.notMember tab v
  unless notmem $ throwTypeError $ "variable already in table: " ++ show v
  so' <- mapM (mapM (uvar ctx)) so
  uv  <- G.flexible gen so'
  SM.add tab v uv
  return uv

rigid :: Ctx s -> Variable -> SolverM s (U.Variable s)
rigid (Ctx { ctxTab = tab, ctxGen = gen }) v = do
  notmem <- SM.notMember tab v
  unless notmem $ throwTypeError $ "variable already in table: " ++ show v
  uv <- G.rigid gen
  SM.add tab v uv
  return uv

uunbind :: Ctx s -> Variable -> SolverM s ()
uunbind (Ctx { ctxTab = tab }) = SM.remove tab

-- | Combinators
type SBinder s v a = (v -> SolverM s (Co a)) -> SolverM s (Co a)

type ShallowType = Structure Variable

data DeepType
  = DeepVar Variable
  | DeepStructure (Structure DeepType)
  deriving (Show)

inst :: VarId -> Variable -> SolverM s (Co (Scheme, [I.Type]))
inst x v = return $ CInstance x v

(-=-) :: Variable -> Variable -> SolverM s (Co ())
v1 -=- v2 = return $ CEq v1 v2

(-==-) :: Variable -> Structure Variable -> SolverM s (Co ())
(-==-) = liftB (-=-)

(<$$>) :: SolverM s (Co a) -> (a -> b) -> SolverM s (Co b)
m <$$> f = do
  c <- m
  return $ CMap c f

(^&) :: Co a -> Co b -> Co (a, b)
c1 ^& c2 = CConj c1 c2

def :: VarId -> Variable -> Co a -> Co a
def = CDef

letrn
  :: Int
  -> [VarId]
  -> ([Variable] -> [Variable] -> SolverM s (Co a))
  -> Co b
  -> SolverM s (Co ([TVarId], [Scheme], a, b))
letrn k xs f1 c2 = do
  rs <- replicateM k U.freshId
  vs <- mapM (const U.freshId) xs
  c1 <- f1 rs vs
  return $ CLet rs xs vs c1 c2

letr1
  :: Int
  -> VarId
  -> ([Variable] -> Variable -> SolverM s (Co a))
  -> Co b
  -> SolverM s (Co ([TVarId], Scheme, a, b))
letr1 k x f1 c2 = letrn k [x] (\rs vs -> f1 rs (head vs)) c2
  <$$> \(gammas, ss, v1, v2) -> (gammas, head ss, v1, v2)

letn
  :: [VarId]
  -> ([Variable] -> SolverM s (Co a))
  -> Co b
  -> SolverM s (Co ([TVarId], [Scheme], a, b))
letn xs f1 = letrn 0 xs (\_rs vs -> f1 vs)

let1
  :: VarId
  -> (Variable -> SolverM s (Co c))
  -> Co d
  -> SolverM s (Co ([TVarId], Scheme, c, d))
let1 x f1 c2 = letn [x] (f1 . head) c2
  <$$> \(gammas, ss, v1, v2) -> (gammas, head ss, v1, v2)

let0 :: Co b -> SolverM s (Co ([TVarId], b))
let0 c1 =
  letn [] (const (return c1)) CTrue <$$> \(gammas, _, v1, _) -> (gammas, v1)

exist :: SBinder s Variable r
exist = exist' Nothing

shallow :: ShallowType -> SBinder s Variable r
shallow t = exist' (Just t)

deep :: DeepType -> SBinder s Variable r
deep dty f = do
  vsRef <- newSTRef []
  let convert dty' = case dty' of
        DeepVar       v -> return v
        DeepStructure s -> do
          v  <- U.freshId
          s' <- mapM convert s
          modifySTRef vsRef ((v, s') :)
          return v
  v  <- convert dty
  c  <- f v
  vs <- readSTRef vsRef
  return $ foldl (\rc (v', s) -> CExist v' (Just s) rc) c vs

liftB
  :: (a -> Variable -> SolverM s (Co b)) -> a -> ShallowType -> SolverM s (Co b)
liftB f v1 t2 = shallow t2 (f v1)

exist' :: Maybe ShallowType -> SBinder s Variable a
exist' t f = do
  v <- U.freshId
  c <- f v
  return $ CExist v t c
