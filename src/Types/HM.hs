{-

Exercise to implement algorithm W for Hindley Milner polymorphic type inference.

-}

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Monomorphic types.
data Tau
    = VarT String               -- e.g., VarT "Int" => `Int`, VarT "Maybe_Int" => `Maybe Int`
    | ArrowT Tau Tau            -- e.g., ArrowT (VarT "Int") (VarT "Bool") => `Int -> Bool`

-- | 'varT' <x> constructs a type variable named <x>.
varT :: String -> Tau
varT = VarT

-- | 'arrowT' <t0> <t1> constructs an arrow type from <t0> to <t1>.
arrowT :: Tau -> Tau -> Tau
arrowT t0 t1 = ArrowT t0 t1


-- | Polymorphic types.
data Sigma
    = MonoT Tau               -- e.g., MonoT (VarT "Int") => `Int` as a Polymorphic type
    | ForAllT String Sigma    -- e.g., ForAllT "a" ForAllT "b" ArrowT (ArrowT (ArrowT (VarT "a")
                              --                                                      (VarT "b"))
                              --                                              (VarT "List_a"))
                              --                                      (VarT "List_b")
                              --       => map :: (a->b) -> [a] -> [b]

-- | 'monoT' <t> lifts a monomorophic type <t> to a polymorphic one.
monoT :: Tau -> Sigma
monoT = MonoT

-- | 'forAllT' <x> <t> universally quantifies <x> in <t>.
forAllT :: String -> Sigma -> Sigma
forAllT x t = ForAllT x t

--------------------------------------------------------------------------------
-- TypeError
--------------------------------------------------------------------------------

-- | Type errors.
data TypeError
    = Err String
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

-- | The type of variable names.
type VarId = String

data TermF v
    = Var v                     -- Variables.
    | App TermF TermF           -- Applications.
    | Abs v TermF               -- Abstractions.
    | Let v TermF TermF         -- Let bindings.
    deriving (Show, Functor, Foldable, Traversable)

-- | The type of terms.
type Term = TermF VarId

-- | 'varE' <x> constructs a variable whose name is <x>.
varE :: VarId -> Term
varE = Var

-- | 'appE' <l> <r> constructs an application of <l> to <r>.
appE :: Term -> Term -> Term
appE l r = App l r

-- | 'absE' <x> <e> constructs an abstraction of <x> over <e>.
absE :: VarId -> Term -> Term
absE x e = Abs x e

-- | 'letE' <x> <e0> <e1> constructs a binding of <e0> to <x> in <e1>.
letE :: VarId -> Term -> Term -> Term
letE x e0 e1 = Let x e0 e1

-- | Things with type annotations.
data Typed t v
    = Typed { unTyped :: v, tyAnn :: t }
    deriving (Show, Functor)

-- | Typed term variables.
type TyVar = Typed Sigma VarId

newtype TypedF t f r = TypedF { unTypedF :: Typed t (f r) }     -- TypedF :: Typed t (f r) -> TypedF t f r
    deriving Show                                               -- unTypedF :: TypedF t f r -> Typed t (f r)

instance Functor fct => Functor (TypedF t fct) where
    fmap f (TypedF typedt) = TypedF (fmap (fmap f) typedt)      -- typedt :: Typed t (fct r)
                                                                -- f :: r -> r'
                                                                -- fmap :: (r -> r') -> Typed t (fct r) -> Typed t (fct r')
