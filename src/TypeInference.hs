-- After Mark P. Jones, Typing Haskell in Haskell, 2000

-- See also, Implementing Type Classes
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3952&rep=rep1&type=pdf

-- See also, https://crypto.stanford.edu/~blynn/compiler/type.html

-- See also:
--
-- Wadler and Blott. How to make ad-hoc polymorphism less ad hoc.
--   POPL 1989.
--
-- Peterson and Jones. Implementing Type Classes. PLDI 1993.

module TypeInference where

--import Ast
--import qualified Ast as A
import Ast ( TVarId, TConId, TClassId, VarId, Lit(..) )

import Data.List( union, nub, intersect )
import Control.Monad ( msum, liftM, ap )


------------------------------ Kinds, Types, Predicated/Qualified types


-- | A Kind: The type of a type, either simple or a type constructor
--   that takes an argument.  E.g., *, * -> *, * -> * -> *
data Kind = Star                -- *, such as Bool, Int, etc.
          | Kfun Kind Kind      -- * -> * is for Maybe, etc.
  deriving Eq

-- | A type variable: a named variable with an associated kind
data Tyvar = Tyvar TVarId Kind
  deriving Eq

-- | A type constructor: a name with a kind
data Tycon = Tycon TConId Kind
  deriving Eq

-- | Types: type variables, constructors, applications of constructors
--   and "generic variables" used in type schemes.
--  E.g., Int, Maybe a, List Int, '0, '1
data Type = TVar Tyvar    -- Type variable, e.g., a
          | TCon Tycon    -- Type constructor, e.g., Bool
          | TAp Type Type -- Application, e.g., Maybe Int
          | TGen Int      -- "Generic" type variable, used in Schemes
  deriving Eq

-- | A type predicate -- a type class constraint on a type, e.g., Num a
data Pred = IsIn TClassId Type
  deriving Eq

-- | A qualified type (or predicate, used in typeclass hierarchies):
--   a list of predicates (typeclass constraints) on a type or predicate.
--
--   Num a, Ord b => a -> b               (:=> in Jones)
data Qual t = Qual [Pred] t
  deriving Eq

-- | A type scheme:  a forall-quantified type consisting of
--  a list of type variable kinds (for each TGen index)
--  for a typeclass-qualified type.
--
--  E.g., forall '0, '1 . Num '0 -> '0 -> '0 -> '1
data Scheme = Forall [Kind] (Qual Type)
  deriving Eq

-- | An assumption about the type of a normal variable.  (:>: in Jones)
data Assump = IsA VarId Scheme

------------------------------ Kinds and Types

-- | Return the kind of type variables, type constructors, and types
class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TVar tv) = kind tv
  kind (TCon tc) = kind tc
  kind (TAp t _) = case kind t of
                     (Kfun _ k) -> k
                     Star       -> error "applying a *?"
  kind (TGen _) =
    error "internal error: can't take the kind of a generic type variable"    

------------------------------ Substitutions

-- | A type substitution: a mapping of type variables to concrete types
type Subst = [(Tyvar, Type)]

-- | The empty substition
nullSubst :: Subst
nullSubst = []

-- | A substitution of a single type variable.  Should have kind t = kind v
--   ( +-> in Jones )
singleton :: Tyvar -> Type -> Subst
singleton v t = [(v, t)]

-- | Things with type variables to which a substitution can be applied
class Types t where
  apply :: Subst -> t -> t -- | Apply a substitution
  tvars :: t -> [Tyvar]    -- | Report all the

instance Types Type where
  apply s vv@(TVar v) = case lookup v s of
                           Just t  -> t
                           Nothing -> vv
  apply s (TAp l r)  = TAp (apply s l) (apply s r)
  apply _ t          = t

  tvars (TVar v)  = [v]
  tvars (TAp l r) = tvars l `union` tvars r
  tvars _         = []

instance Types a => Types [a] where
  apply s = map $ apply s
  tvars = nub . concat . map tvars

-- | Compose two substitutions, apply (s1 @@ s2) = apply s1 . apply s2
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [ (v, apply s1 t) | (v, t) <- s2 ] ++ s1

-- | Merge two substitutions that commute; fail if they don't
merge :: MonadFail m => Subst -> Subst -> m Subst
merge s1 s2 | agree     = return (s1 ++ s2)
            | otherwise = fail "inconsistent merge attempted"
  where
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) $
      map fst s1 `intersect` map fst s2

-- | Determine the most general unifier: the smallest binding that
--   makes the two types equivalent
mgu :: MonadFail m => Type -> Type -> m Subst
mgu (TAp l r) (TAp l' r') = do
  b1 <- mgu l l'
  b2 <- mgu (apply b1 r) (apply b1 r')
  return $ b2 @@ b1
mgu (TVar v) t                       = unifyVar v t
mgu t (TVar v)                       = unifyVar v t
mgu (TCon tc) (TCon tc') | tc == tc' = return nullSubst
mgu _ _                              = fail "types do not unify"

-- | Unify a variable with a type, if possible.  Fails if
--   the variable is already within the type (occurs check)
--   or when the kinds don't match. ("varBind" in Jones)
unifyVar :: MonadFail m => Tyvar -> Type -> m Subst
unifyVar v t | t == TVar v      = return nullSubst
             | v `elem` tvars t = fail "occurs check fails"
             | kind v /= kind t = fail "mismatched kinds"
             | otherwise        = return $ singleton v t

-- | Compute a binding s.t. substitute (match t1 t2) t1 = t2
match :: MonadFail m => Type -> Type -> m Subst
match (TAp l r) (TAp l' r')            = do bl <- match l l'
                                            br <- match r r'
                                            merge bl br
match (TVar v) t | kind v == kind t    = return $ singleton v t
match (TCon tc) (TCon tc') | tc == tc' = return nullSubst
match _ _                              = fail "types do not match"


instance Types t => Types (Qual t) where
  apply b (Qual ps t) = Qual (apply b ps) (apply b t)
  tvars (Qual ps t) = tvars ps `union` tvars t

instance Types Pred where
  apply b (IsIn c t) = IsIn c (apply b t)
  tvars (IsIn _ t) = tvars t

lift :: MonadFail m => (Type -> Type -> m Subst)
      -> Pred -> Pred -> m Subst
lift m (IsIn c t) (IsIn c' t') | c == c'   = m t t'
                               | otherwise = fail "classes differ"

 -- | Compute most general unifier and match up predicates
--   by making sure their classes match
mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred   = lift mgu
matchPred = lift match

------------------------------ Types for Type Classes

-- | An instance of a typeclass, e.g., (Enum a, Eq a) => Ord a
type Instance = Qual Pred

-- | Information about a typeclass: its superclasses and its instances
data TypeClass = TypeClass { superclasses :: [TClassId]
                           , instances :: [Instance]
                           }

-- | The typeclass enviromment: a query function and a list of
--   default types
data ClassEnv = ClassEnv { classes :: TClassId -> Maybe TypeClass
                         , defaults :: [Type]
                         }

-- | Superclasses of a given class
super :: ClassEnv -> TClassId -> [TClassId]
super env ci = case classes env ci of Just tc -> superclasses tc
                                      Nothing -> error "unknown typeclass"

-- | Instances of a given class
insts :: ClassEnv -> TClassId -> [Instance]
insts env ci = case classes env ci of Just tc -> instances tc
                                      Nothing -> error "unknown typeclass"

-- | True if the Maybe isn't Nothing
defined          :: Maybe a -> Bool
defined (Just _) =  True
defined Nothing  =  False

-- | Extend a class environment with a new class (modify in Jones)
addToEnv :: ClassEnv -> TClassId -> TypeClass -> ClassEnv
addToEnv env cn c = env { classes = \n -> if cn == n then Just c
                                                     else classes env n }
                                           
initialEnv :: ClassEnv
initialEnv = ClassEnv { classes = \_ -> Nothing
                      , defaults = [] -- FIXME: tInteger, tDouble
                      }

-- | A function that evolves a type class environment or may fail
type EnvTransformer = ClassEnv -> Maybe ClassEnv

-- | Sequence EnvTransformers: pass the result of the first to the second
infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) env = do env' <- f env
                   g env'
                   
-- | Add a new typeclass to an environment after checking for consistency
addClass :: TClassId -> [TClassId] -> EnvTransformer
addClass cid sids env
  | defined (classes env cid)              = fail "class already defined"
  | any (not . defined . classes env) sids = fail "superclass not defined"
  | otherwise                              = return $ addToEnv env cid newtc
    where newtc = TypeClass { superclasses = sids
                            , instances    = [] }

-- | Add a new typeclass instance to the environment if it doesn't conflict
addInst              :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn cid _) env
  | not (defined (classes env cid)) = fail "undefined typeclass"
  | any (overlap p) qs              = fail "overlapping instance"
  | otherwise                       = return $ addToEnv env cid tc'
  where
    its = insts env cid
    qs  = [ q | Qual _ q <- its ] -- existing predicates
    tc' = TypeClass { superclasses = super env cid
                    , instances = Qual ps p : its }          
    overlap p1 p2 = defined $ mguPred p1 p2

------------------------------ Entailment: Whether a predicate is implied

-- | Return all the superclasses of a given predicate (may include dups)
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper env p@(IsIn cid t) =
  p : concat [ bySuper env (IsIn cid' t) | cid' <- super env cid ]

-- | If possible, return a list of predicates that, together, can
--   guarantee the given predicate in the given environment
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst env p@(IsIn cid _) = msum [ tryInst it | it <- insts env cid ]
  where tryInst (Qual ps h) = do u <- matchPred h p
                                 Just $ map (apply u) ps

-- | In an environment, does any of the listed predicates entail the
--   given one?
--
--   First, check whether p is among any of the ps's superclasses (bySuper).
--   Second, check whether all the subgoals demanded by p are entailed by
--   those in ps (byInst)
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail env ps p = any (p `elem`) (map (bySuper env) ps) ||
                  case byInst env p of
                    Nothing -> False
                    Just qs -> all (entail env ps) qs

------------------------------ Context reduction

-- | Is a type predicate in head normal form,
--   or is more complicated than it needs to be?
--
-- A type predicate is <class-id> `IsIn` <type>, e.g., Num a
--
-- To be in head normal form, the type should be
-- a simple variable, or a simple variable applied to something.
--
-- E.g., Num a is in HNF; Num (String a) isn't.
inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
  where hnf (TVar _) = True
        hnf (TCon _) = False
        hnf (TAp t' _) = hnf t'
        hnf (TGen _) = error "inHnf on TGen?"

-- | Transform a list of predicates to a list in head normal form
toHnfs :: MonadFail m => ClassEnv -> [Pred] -> m [Pred]
toHnfs env ps = do pss <- mapM (toHnf env) ps
                   return $ concat pss

-- | Transform a single predicate into a simpler list of predicates,
--   e.g., by removing those are redundant due to entailment, e.g.,
--   Eq a   is eliminated if  Ord a  is in the environment
--   because     Eq a => Ord a
toHnf :: MonadFail m => ClassEnv -> Pred -> m [Pred]
toHnf env p | inHnf p = return [p]
            | otherwise = case byInst env p of
                            Nothing -> fail "context reduction"
                            Just ps -> toHnfs env ps

-- | Simplify a list of predicates by removing those that
--   are entailed by others, e.g.,
--
--   (Eq a, Ord a) =>   becomes   (Ord a) =>  because Eq a is a superclass
--   of Ord a.
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify env = loop []
  where
    loop rs [] = rs
    loop rs (p : ps) | entail env (rs ++ ps) p = loop rs ps
                     | otherwise               = loop (p : rs) ps
    
-- | Context reduction: transform a list of predicates into head normal
--   form, then remove those that are entailed by others
reduce :: MonadFail m => ClassEnv -> [Pred] -> m [Pred]
reduce env ps = do qs <- toHnfs env ps
                   return $ simplify env qs

------------------------------ Type Schemes

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tvars (Forall _ qt)     = tvars qt

-- | Construct a type scheme by supplying a list of type variables to
--   be replaced with TGens numbered 0, 1, 2, ...
--
--  1. Get those variables in the qualified type listed in vs
--     in the order they're returned by "variables"
--  2. Return the Scheme that includes the kinds of those variables
--     after substituting those variables with TGen 0, TGen 1, etc.
quantify  :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall (map kind vs') (apply s qt)
     where vs' = [ v | v <- tvars qt, v `elem` vs ]
           s   = zip vs' (map TGen [0..])

-- | Promote a type to a scheme without variables or predicates
toScheme :: Type -> Scheme
toScheme t = Forall [] (Qual [] t)

------------------------------ Assumptions: Schemes for normal variables

instance Types Assump where
  apply s (IsA v sc) = IsA v (apply s sc)
  tvars (IsA _ sc) = tvars sc

-- | Return the scheme for a variable if it exists in
--   the given list of assumptions
find :: MonadFail m => VarId -> [Assump] -> m Scheme
find vid [] = fail $ "unknown identifier " ++ vid
find vid (IsA vid' sc : as ) | vid == vid' = return sc
                             | otherwise   = find vid as

------------------------------ The Type Inference Monad

-- | Type inference monad: the current collection of type bindings
--   and an integer holding the next fresh variable name
newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Functor TI where
  fmap = liftM

instance Applicative TI where
  pure = return
  (<*>) = ap

instance Monad TI where
  return x   = TI $ \bs n -> (bs, n, x)
  
  TI f >>= g = TI (\bs n -> let (bs', m, x) = f bs n
                                TI gx = g x
                            in gx bs' m)

instance MonadFail TI where
  fail s = error s

-- | Run something in the TI monad starting from the empty binding with
--   no already-assigned type variables
runTI :: TI a -> a
runTI (TI f) = x where (_, _, x) = f nullSubst 0

-- | Get the current TBinds from the TI monad
getBinds :: TI Subst
getBinds  = TI $ \bs n -> (bs, n, bs)

-- | Add bindings to the environment
extendBinds :: Subst -> TI ()
extendBinds bs' = TI (\bs n -> (bs' @@ bs, n, ()))

-- | Unify two types by applying the environment bindings to each,
--   computing the most general unifier, and adding that to the environment
unify :: Type -> Type -> TI ()
unify t1 t2 = do bs <- getBinds
                 unifier <- mgu (apply bs t1) (apply bs t2)
                 extendBinds unifier

-- | Return a new type variable identifier from a number
enumId :: Int -> TVarId
enumId n = 'v' : show n

-- | Get a fresh, numbered type variable
newTVar :: Kind -> TI Type
newTVar k  = TI (\bs n -> let v = Tyvar (enumId n) k
                          in (bs, n + 1, TVar v))

-- | Replace generic type variables with the type with that index in the
--   given list

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r) -- Recurse
  inst ts (TGen n)  = ts !! n                     -- Replace this type
  inst _  t         = t

instance Instantiate a => Instantiate [a] where
  inst ts = map $ inst ts

instance Instantiate t => Instantiate (Qual t) where
  inst ts (Qual ps t) = Qual (inst ts ps) (inst ts t)

instance Instantiate Pred where
  inst ts (IsIn cid t) = IsIn cid (inst ts t)

-- | Create a fresh instance of a type scheme by instantiating its
--   generics with fresh type variables
freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return $ inst ts qt

------------------------------ Built-in types

tUnit, tChar, tInt, tInteger, tFloat, tDouble,        
  tList, tDuration, tArrow, tTuple2, tString :: Type

tUnit     = TCon (Tycon "()"      Star)
tChar     = TCon (Tycon "Char"    Star)
tInt      = TCon (Tycon "Int"     Star)
tInteger  = TCon (Tycon "Integer" Star)
tFloat    = TCon (Tycon "Float"   Star)
tDouble   = TCon (Tycon "Double"  Star)
tList     = TCon (Tycon "[]"      (Kfun Star Star))
tDuration = TCon (Tycon "Duration" Star)
tArrow    = TCon (Tycon "(->)"    (Kfun Star (Kfun Star Star)))
tTuple2   = TCon (Tycon "(,)"     (Kfun Star (Kfun Star Star)))

tString   = TAp tList tChar       -- type String = [Char]



------------------------------ Type Inference for the AST

-- | Type inference for a literal: Chars and Strings are a specific type;
--   Integer and Rational literals are part of the Num and Fractional
--   typeclasses
tiLit                :: Lit -> TI ([Pred], Type)
tiLit (IntLit _)     = do v <- newTVar Star
                          return ([IsIn "Num" v], v)
tiLit (StringLit _)  = return ([], tString)
tiLit (DurLit _)     = return ([], tDuration)
tiLit (RatLit _)     = do v <- newTVar Star
                          return ([IsIn "Fractional" v], v)
tiLit (CharLit _)    = return ([], tChar)
