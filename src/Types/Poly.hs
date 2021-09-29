{- | Type system supporting (parametric) polymorphism.

Based on the Hindley-Milner-Damas type system and the definitions in
OCaml and Caml Light.

This is written with the intention of being a lower-level type system, one
that a more expressive type system compiles down into. Notably, this type
system does not support aliases, records (data constructors
with named field), etc. These will have been instantiated/desugared by this
point.

This type system also does not support type classes or any sort of
higher-kinded polymorphism. This is due to following restrictions:

- No partially-applied type constructors; whenever a type constructor appears
  in a type expression, it is fully applied. e.g., `Option` is not allowed.

- No abstraction over type constructors; the head (left-most term) of a type
  application must be a concrete type constructor. e.g., `a b` is not
  allowed.

- No quantifiers inside of type expressions; all quantifiers must be
  top-level (also known as prenex normal form, or PNF). This is what
  distinguishes HM from System F, and what makes the former decidable while
  the latter undecidable.
-}
module Types.Poly
  ( Type(..)
  , TypeDef(..)
  , Builtin(..)
  ) where

import           Common.Identifiers             ( DConId
                                                , TConId
                                                , TVarIdx
                                                )
import           Types.TypeSystem               ( Arity
                                                , Builtin(..)
                                                , TypeSystem(..)
                                                , TypeVariant
                                                )


-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type)         -- ^ Builtin types
  | TCon TConId [Type]              -- ^ Type constructor, e.g., Option '0
  | TVar TVarIdx                    -- ^ Type variables, e.g., '0
  deriving Eq

{- | The type definition associated with a type constructor.

A definition for `data MyList a = Cons a (MyList a) | Nil` looks like:

  TypeDef { arity = 1
          , [ ("Cons", [VariantUnnamed [TVar 0, TCon ("MyList" [TVar 0])]])
            , (DConId "Nil", [VariantUnnamed []])
            ]
          }

(Data constructors for identifiers are omitted for brevity.)
-}
data TypeDef = TypeDef
  { arity    :: Arity
  , variants :: [(DConId, TypeVariant Type)]
  }

-- | 'Type' is a type system.
instance TypeSystem Type where
  unit = TBuiltin Unit
  void = TBuiltin Void
  ref  = TBuiltin . Ref
  arrow a b = TBuiltin $ Arrow a b
