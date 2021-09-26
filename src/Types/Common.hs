-- | Common definitions for the type system(s) used in this compiler.
--
-- Note that placement of definitions here is provisionary; some of these may
-- make more sense in the AST, depending on how and where that's defined. That
-- is, this is stuff that John thinks should be shared across multiple modules,
-- but does not yet know where they should ultimately belong.
--
-- The following is a sketch of the type pipeline (from top to bottom):
--
-- Types.Ast: type classes + polymorphic types + implicit types (to be inferred)
--
--    (type inference)
--
-- Types.Classes: type classes + polymorphic types
--
--    (typeclass instantiation)
--
-- Types.Poly: polymorphic types
--
--    (monomophisation)
--
-- Types.Mono: concrete only
--
-- We still need to flesh out details of exactly what features the higher-level
-- type systems will support, e.g., aliases, associated types, multi-parameter
-- type classes, higher-kinded type classes, etc.
module Types.Common where

-- | Identifier for type variable, e.g., "a".
newtype TVarId = TVarId String

-- | de Bruijn index for type variables, e.g., "'0".
newtype TVarIdx = TVarIdx Int

-- | Identifier for type constructors, e.g., "Option".
type TConId = String

-- | Identifier for data constructors, e.g., "None".
type DConId = String
