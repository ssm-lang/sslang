{-# LANGUAGE DeriveDataTypeable #-}
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
module IR.Types.TypeSystem where

import           Common.Identifiers             ( DConId
                                                , VarId
                                                )
import           Common.Pretty

import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Data                      ( Data
                                                , Typeable
                                                )

-- | The number of arguments a type constructor will take.
type Arity = Int

-- | Builtin type constructors that should be preserved across all type systems.
data Builtin t
  = Unit          -- ^ Singleton type @()@
  | Void          -- ^ Uninhabited type @!@, coerces with anything
  | Ref t         -- ^ Reference of @&@
  | Arrow t t     -- ^ Function arrow @a -> b@
  | Tuple [t]     -- ^ Tuple with two or more fields
  | Integral Int  -- ^ Two's complement binary type with size in bits
  deriving (Eq, Show, Ord, Typeable, Data)

instance Functor Builtin where
  fmap _ Unit           = Unit
  fmap _ Void           = Void
  fmap f (Ref t       ) = Ref $ f t
  fmap f (Arrow l r   ) = Arrow (f l) (f r)
  fmap f (Tuple    tys) = Tuple $ fmap f tys
  fmap _ (Integral s  ) = Integral s

instance Pretty t => Pretty (Builtin t) where
  pretty Unit           = pretty "()"
  pretty Void           = pretty "!"
  pretty (Ref t       ) = parens $ pretty "&" <> pretty t
  pretty (Arrow a b   ) = parens $ pretty a <+> rarrow <+> pretty b
  pretty (Tuple    tys) = parens $ hsep $ punctuate comma $ map pretty tys
  pretty (Integral s  ) = pretty $ "Int" ++ show s

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

-- | Helper to construct tuples from list of types, accounting for size < 2.
tuple :: TypeSystem t => [t] -> t
tuple []  = unit -- Empty tuples are just voids
tuple [t] = t   -- Singleton tuples are just tuples.
tuple ts  = projectBuiltin $ Tuple ts

-- | Helper to construct integral type.
int :: TypeSystem t => Int -> t
int s = projectBuiltin $ Integral s

-- | Decompose an 'Arrow' type into a list of argument types and a return type.
collectArrow :: TypeSystem t => t -> ([t], t)
collectArrow t = case dearrow t of
  Just (a, b) -> let (as, rt) = collectArrow b in (a : as, rt)
  _           -> ([], t)

{- | The type definition associated with a type constructor.

A definition for `data MyList a = Cons a (MyList a) | Nil` looks like:

@
  TypeDef { arity = 1
          , [ ("Cons", VariantUnnamed [TVar 0, TCon ("MyList" [TVar 0])])
            , ("Nil", VariantUnnamed [])
            ]
          }
@

(Data constructors for identifiers are omitted for brevity.)

Note that for a flat type system, where all type constructors are nullary, arity
will just be set to 0.
-}
data TypeDef t = TypeDef
  { variants :: [(DConId, TypeVariant t)]
  , arity    :: Arity
  }
  deriving (Show, Eq, Typeable, Data)

-- | Arguments to a data constructor, whose fields may or may not be named
data TypeVariant t
  = VariantNamed [(VarId, t)] -- ^ A record with named fields
  | VariantUnnamed [t]        -- ^ An algebraic type with unnamed fields
  deriving (Show, Eq, Typeable, Data)

instance Functor TypeDef where
  fmap f TypeDef { variants = vs, arity = a } =
    TypeDef { variants = fmap (second $ fmap f) vs, arity = a }

instance Functor TypeVariant where
  fmap f (VariantNamed   fs) = VariantNamed $ fmap (second f) fs
  fmap f (VariantUnnamed fs) = VariantUnnamed $ fmap f fs

-- | The number of fields in a 'TypeVariant'.
variantFields :: TypeVariant t -> Int
variantFields (VariantNamed fields ) = length fields
variantFields (VariantUnnamed fields ) = length fields
