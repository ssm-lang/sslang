module Ir where
import           Ast                            ( VarId )
import Types.Common (DConId)

type FfiId = String
type Arith = String -- "+i32" TODO
data Literal t = Literal String t -- TODO

{- Notes

  type ! = forall a. a -- the ! type "void" is not meant to be used for data
  declaration

   Break : () -> !
   Loop : (() -> ()) .... -> !
   Return : a -> !
   Wait : ... -> !
   Assign : Ref t -> t -> () -- immediate assignment
   Later : Ref t -> t -> Time -> ()
     where Time is stricly greater than 0.

   We have a separate Later statement because we can use this to infer causality
   loops etc.
 -}

data PrimFun = Loop
             | Break
             | Return
             | Fork
             | Wait
             | Assign
             | Later
             | Arith Arith
             | Ffi FfiId

-- | The type of expressions.
--
-- t represents the type of this expression. At various stages, this may
-- represent a richer or simpler type system.
--
-- Note that sequencing should be performed using chained let-expressoins:
--
-- let _ = stmt1 in
-- let _ = stmt2 in
-- ...
data Expr t = Let [(VarId, Expr t)] (Expr t) -- ^
            | Lambda VarId t (Expr t)
            | Lit (Literal t)
            | App (Expr t) (Expr t) -- ^ Curried application
            | Var VarId t
            | PrimApp PrimFun [Expr t] t -- ^ Fully applied, primitive function call
            | Match (Expr t) VarId [Alt t] t -- ^ Match statement

data Alt t = AltData DConId [VarId] (Expr t)
           | AltLit (Literal t) (Expr t)
           | AltDefault (Expr t)
