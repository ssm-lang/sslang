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

-- | The number of arguments a type constructor will take.
type Arity = Int

-- | A type system must provide at least syntactic equality and these methods.
class Eq t => TypeSystem t where
  unit :: t
  void :: t
  ref :: t -> t
  arrow :: t -> t -> t

-- | Builtin type constructors.
data Builtin t
  = Unit        -- ^ Singleton type (())
  | Void        -- ^ Uninhabited type (!), coerces with anything
  | Ref t       -- ^ Reference of (&)
  | Arrow t t   -- ^ Function arrow (a -> b)
  | Tuple [t]   -- ^ Tuple, where arity must be >= 2
  deriving Eq

deref :: Builtin t -> Maybe t
deref (Ref t) = Just t
deref _       = Nothing

{- | The type definition associated with a type constructor.

A definition for `data MyList a = Cons a (MyList a) | Nil` looks like:

  TypeDef { arity = 1
          , [ ("Cons", [VariantUnnamed [TVar 0, TCon ("MyList" [TVar 0])]])
            , (DConId "Nil", [VariantUnnamed []])
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
