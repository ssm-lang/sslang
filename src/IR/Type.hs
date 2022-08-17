module IR.Type where
import           Common.Identifiers             ( TConId(..)
                                                , TVarId(..)
                                                )

data Scheme = Scheme [TVarId] Constraint Type

{- | Constraints on a type scheme.

For now, we only support trivial constraints.
-}
data Constraint = CTrue

data Type
  = TCon TConId [Type]
  | TVar TVarId

