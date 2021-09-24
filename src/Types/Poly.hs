-- | Type system supporting (parametric) polymorphism.
--
-- Based on the Hindley-Milner-Damas type system and the definitions in
-- OCaml and Caml Light.
--
-- This is written with the intention of being a lower-level type system, one
-- that a more expressive type system compiles down into. Notably, this type
-- system does not support aliases, records (data constructors
-- with named field), etc. These will have been instantiated/desugared by this
-- point.
--
-- This type system also does not support type classes or any sort of
-- higher-kinded polymorphism. This is due to following restrictions:
-- - No partially-applied type constructors; whenever a type constructor appears
--   in a type expression, it is fully applied. e.g., `Option` is not allowed.
-- - No abstraction over type constructors; the head (left-most term) of a type
--   application must be a concrete type constructor. e.g., `a b` is not
--   allowed.
-- - No quantifiers inside of type expressions; all quantifiers must be
--   top-level (also known as prenex normal form, or PNF). This is what
--   distinguishes HM from System F, and what makes the former decidable while
--   the latter undecidable.
module Types.Poly where

import Types.Common (DConId, TConId, TVarId)

-- | How many arguments a type constructor can take.
type Arity = Int


-- | The language of type expressions, e.g., what appears in a type signature.
data Type = TVar TVarId         -- ^ Type variables, e.g., a
          | TArrow Type Type    -- ^ Function type constructor, e.g., a -> b
          | TTuple [Type]       -- ^ Tuple (product) type, e.g., (a, b)
          | TCon TConId [Type]  -- ^ Type constructor, e.g., Option a


-- | The language of type definitions, e.g., the type definition associated with
-- a type constructor.
--
-- The definition for built-in types are left abstract. For instance, the
-- definition associated with Int looks like:
--
--    TConBuiltin 0
--
-- A definition for `data MyList a = Cons a (MyList a) | Nil` looks like:
--
--   TConUserDef 1 [("Cons", [TVar "a", TCon "MyList" [TVar "a"]]), ("Nil", [])]
--
-- This representation has the field names of a record stripped out, and just
-- represents them as data constructors. So the definition for a type like
-- `data MyRec a b = Con1 { a1: a, b2: b } | Con2 Int b` is desugared to
-- `data MyRec a b = Con1 a b | Con Int b` and thus represented as:
--
--   TConUserDef 2 [ ("Con1", [TVar "a", TVar "b"])
--                 , ("Con2", [TCon "Int" [], TVar "b"])
--                 ]
data TConDef = TConBuiltin Arity                    -- ^ Builtin type constructor
             | TConUserDef Arity [(DConId, [Type])] -- ^ User-defined type constructor
