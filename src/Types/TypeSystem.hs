{- | Common definitions for the type system(s) used in this compiler.

The following is a sketch of the type pipeline (from top to bottom):

Types.Ast: type classes + polymorphic types + implicit types (to be inferred)

   (type inference)

Types.Classes: type classes + polymorphic types

   (typeclass instantiation)

Types.Poly: polymorphic types

   (monomophisation)

Types.Flat: concrete only
-}
module Types.TypeSystem where

import           Common.Identifiers             ( DConId
                                                , FieldId
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )

-- | The number of arguments a type constructor will take.
type Arity = Int

-- | Builtin type constructors that should be preserved across all type systems.
data Builtin t
  = Unit        -- ^ Singleton type (())
  | Void        -- ^ Uninhabited type (!), coerces with anything
  | Ref t       -- ^ Reference of (&)
  | Arrow t t   -- ^ Function arrow (a -> b)
  | Tuple [t]   -- ^ Tuple, where arity must be >= 2
  deriving Eq

instance Functor Builtin where
  fmap _ Unit        = Unit
  fmap _ Void        = Void
  fmap f (Ref t    ) = Ref $ f t
  fmap f (Arrow l r) = Arrow (f l) (f r)
  fmap f (Tuple tys) = Tuple $ fmap f tys

{- | A type system must allow us to construct and access underlying builtins.

Instances should satisfy the following law:

* @injectBuiltin . projectBuiltin = Just@

-}
class TypeSystem t where
  projectBuiltin :: Builtin t -> t
  injectBuiltin :: t -> Maybe (Builtin t)

-- | Helper to construct 'Unit' type in any 'TypeSystem'.
unit :: TypeSystem t => t
unit = projectBuiltin Unit

-- | Helper to construct 'Void' type in any 'TypeSystem'.
void :: TypeSystem t => t
void = projectBuiltin Void

-- | Helper to construct 'Ref' type in any 'TypeSystem'.
ref :: TypeSystem t => t -> t
ref = projectBuiltin . Ref

-- | Helper to construct 'Arrow' type in any 'TypeSystem'.
arrow :: TypeSystem t => t -> t -> t
arrow a b = projectBuiltin $ Arrow a b

-- | Helper to unwrap 'Ref' in any 'TypeSystem'.
deref :: TypeSystem t => t -> Maybe t
deref t = case injectBuiltin t of
  Just (Ref t') -> Just t'
  _             -> Nothing

-- | Helper to unwrap 'Arrow' in any 'TypeSystem'.
dearrow :: TypeSystem t => t -> Maybe (t, t)
dearrow t = case injectBuiltin t of
  Just (Arrow a b) -> Just (a, b)
  _                -> Nothing

-- | Decompose an 'Arrow' type into a list of argument types and a return type.
collectArrow :: TypeSystem t => t -> ([t], t)
collectArrow t = case dearrow t of
  Just (a, b) -> let (as, rt) = collectArrow b in (a : as, rt)
  _           -> ([], t)

{- | The type definition associated with a type constructor.

A definition for `data MyList a = Cons a (MyList a) | Nil` looks like:

  TypeDef { arity = 1
          , [ ("Cons", [VariantUnnamed [TVar 0, TCon ("MyList" [TVar 0])]])
            , ("Nil", [VariantUnnamed []])
            ]
          }

(Data constructors for identifiers are omitted for brevity.)

Note that for a flat type system, where all type constructors are nullary, arity
will just be set to 0.
-}
data TypeDef t = TypeDef
  { variants :: [(DConId, TypeVariant t)]
  , arity    :: Arity
  }

data TypeVariant t
  = VariantNamed [(FieldId, t)]
  | VariantUnnamed [t]
  deriving Eq

instance Functor TypeDef where
  fmap f TypeDef { variants = vs, arity = a } =
    TypeDef { variants = fmap (second $ fmap f) vs, arity = a }

instance Functor TypeVariant where
  fmap f (VariantNamed   fs) = VariantNamed $ fmap (second f) fs
  fmap f (VariantUnnamed fs) = VariantUnnamed $ fmap f fs
