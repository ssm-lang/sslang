{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
--make draft, add 2 test cases

--to debug, look at the isBad

{- | Remove unnecessary Par expressions from the IR

This pass detects unnecessary par expressions and then replaces them with equivalent sequential expressions.
-}
module IR.OptimizePar (
  optimizePar,
) where

import Common.Compiler
import qualified Common.Compiler as Compiler
import Control.Monad.State.Lazy (
  MonadState,
  StateT (..),
  evalStateT,
  gets,
  modify,
 )
import IR.IR (Literal (LitIntegral))
import qualified IR.IR as I
import Data.Bifunctor
import Common.Identifiers(Identifier (Identifier), TVarId (..))
import IR.Types.Type
import           Data.Generics.Aliases          ( mkM )
import           Data.Generics.Schemes          ( everywhereM )

-- | Optimization Environment
data OptParCtx = OptParCtx
  { -- | 'numPars' the number of par nodes in the input program's IR.
    numPars :: Int
  , -- | 'numLitInts' the number of "bad" par node in the input program's IR.
    numBadPars :: Int
  }


-- | OptPar Monad
newtype OptParFn a = OptParFn (StateT OptParCtx Compiler.Pass a)
  deriving (Functor) via (StateT OptParCtx Compiler.Pass)
  deriving (Applicative) via (StateT OptParCtx Compiler.Pass)
  deriving (Monad) via (StateT OptParCtx Compiler.Pass)
  deriving (MonadFail) via (StateT OptParCtx Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT OptParCtx Compiler.Pass)
  deriving (MonadState OptParCtx) via (StateT OptParCtx Compiler.Pass)


-- | Example func to delete later! Demonstrates how to extract a value from the OptParFn Monad
getNumberOfPars :: OptParFn Int
getNumberOfPars = gets numPars


-- | Example func to delete later! Demonstrates how to modify a value in the OptParFn Monad
updateNumberOfPars :: Int -> OptParFn ()
updateNumberOfPars num = do
  modify $ \st -> st{numPars = num}


-- | Run a OptParFn computation.
runOptParFn :: OptParFn a -> Compiler.Pass a
runOptParFn (OptParFn m) =
  evalStateT
    m
    OptParCtx
      { numPars = 0
      , numBadPars = 0
      }


--traversing the ir replaced with everywhere

--rewrite of case1, transorm into tuple is operational

--isbad is working

-- run on all regression testss 

--check the types 

--can only take ut the instantenous expression if they occur before 

  

-- prepare for case 2 and casse 3

{- | Entry-point to Par Optimization.

Maps over top level definitions, removing unnecessary pars.
-}
optimizePar :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
optimizePar p = runOptParFn $ do

  defs' <- everywhereM (mkM findFixBadPar) $ I.programDefs p
 -- optimizedDefs <- mapM optimizeParTop $ I.programDefs p
 -- fail ("Number of Bad Par Exprs in " ++ show (map fst (map tupleMatch1 optimizedDefs)) ++ ": " ++ (show (map tupleMatch optimizedDefs)))
 -- return $ p{I.programDefs = map tupleMatch1 optimizedDefs}
  
  --return $ p{I.programDefs = p}
  --return $ p{I.programDefs = I.programDefs p}
  return $ p{I.programDefs = defs'}

{- | Given an Expr as input, if it turns out to be a bad Par expr, rewrite it.

Otherwise, leave the expression alone.

checks for case1: 

case 1:
par 5 + 1
    3 + 2
-}

--import foldApp from IR.IR
-- import tempTupleId from IR.Types.Type
findFixBadPar :: I.Expr I.Type -> OptParFn (I.Expr I.Type)
findFixBadPar e@__ = if isBad e then rewrite e else pure e 
 where rewrite :: I.Expr I.Type -> OptParFn (I.Expr I.Type)
       -- structure of IR
       rewrite p@(I.Prim I.Par exprlist _) = pure x
        where dataConstructorName = "Pair2"
              t = (TVar $ TVarId (Identifier "PINEAPPLE"))
              --construct type that is tuple of arguments
              x = (I.foldApp dConNode argsToTuple)
              dConNode = I.Data (I.DConId (Identifier dataConstructorName)) t
              argsToTuple = (zip exprlist (repeat t))
        
      -- pure dummy --TODO: rewrite the bad par as good one

       rewrite _ = fail "rewrite should only be called on a Par IR node!"
       dummy = I.Var (I.VarId (Identifier "PINEAPPLE")) (TVar $ TVarId (Identifier "dummy"))
{-
case 1:
par 5 + 1
    3 + 2
^ we agree this par expr is bad 
^ this par returns the value (5+1,3+2)

We want to rewrite case 1 into 
(5+1,3+2)
which really desugars into 
(Pair2 5+1 3+2)
which as an IR node is 
(I.App (I.App (I.DCon DConId "Pair2") (I.Prim (I.PrimOp PrimAdd) [(I.Lit 5), (I.Lit 1)])) (I.Prim (I.PrimOp PrimAdd) [(I.Lit 3), (I.Lit 1)]))

What does par 5 + 1 look like as an IR node?
              3 + 2
Prim I.Par [I.Prim (I.PrimOp PrimAdd) [(I.Lit 5), (I.Lit 1)], I.Prim (I.PrimOp PrimAdd) [(I.Lit 3), (I.Lit 1)]] t

let arg1 = I.Prim (I.PrimOp PrimAdd) [(I.Lit 5), (I.Lit 1)]
let arg2 =  I.Prim (I.PrimOp PrimAdd) [(I.Lit 3), (I.Lit 1)]
foldApp (I.Dcon I.DConId "Pair2") [arg1, arg2]
^foldApp returns a nested application such that "Pair2" is applied to a list of arguments

(I.Prim I.Par exprlist _) 
  |
  |
  v
let tupleDataConstructorName = tempTupleId (length exprlist) // returns "Pair2" or "Pair3", or whatever you need
foldApp tupleDataConstructorName exprlist




--Case on type of Prime, whether has a Wait or a PrimOp
what is 5+1 as an IR node?
I.Prim (I.PrimOp PrimAdd) [(I.Lit 5), (I.Lit 1)]
^ do you agree with this?

what is 3+2 as an IR node?
I.Prim (I.PrimOp PrimAdd) [(I.Lit 3), (I.Lit 1)]


What is (Pair2 5+1 3+2) as an IR node?
We know for reference: add 5 1 as an IR node is (I.App (I.App (I.Var VarId "add") (I.Lit 5)) (I.Lit 1))
so we know that pair2 applied to its two arguments will look like in the IR as
(I.App (I.App (I.DCon DConId "Pair2") (I.Prim (I.PrimOp PrimAdd) [(I.Lit 5), (I.Lit 1)])) (I.Prim (I.PrimOp PrimAdd) [(I.Lit 3), (I.Lit 1)]))

We have a library function called foldApp that takes a function name and a list of arguments,
and wraps them up in application.
-}

{- | 1.5) Implement IsBad Predicate

Suggested by John during Monday Meeting.
Returns true if par expr contains only instantaneous expressions as arguments.
False otherwise.
Useful for exercise 2.
-}

--helper function 
--bad par expr: par nodes lisit of arguments contains a wait 
-- we assume that all function calls are blocking
--variable are non blocking 
--prim operatiions that are not wait are non blocking 
--literals are non blockiing 
-- function calls are application nodes App (Expr t) (Expr t) t
-- nested function application, 
-- add x 0 -> App(App(add,x),0)
-- a@(App _ _ )
--whenever there any any non blocking calls, reqrite the expression so that the non blocking calls 
--outside with a let, and blocking call remain with the par
-- if par has just 0 or 1 blocking call, no need for par 
-- take our non blocking, append to tuple in correct position

isNotWait :: I.Expr I.Type -> Bool
isNotWait (I.Prim I.Wait _ _) = False
isNotWait (_) = True 

isNotFunction :: I.Expr I.Type -> Bool
isNotFunction (I.App expr1 expr2 t) = False
isNotFunction ( _ ) = True 


isBad :: I.Expr I.Type -> Bool
--isBad theExpr = False -- currently a stub
isBad (I.Prim I.Par exprlist _) = do
  (and (map isNotWait exprlist)) && (and (map isNotFunction exprlist))
isBad _ = False



--isBad look at arguments to par if there is a wait priumitive, its bad




{- | 2) Count Bad Par Nodes

Practice Exercise to Delete Later!

Traverse the IR representation of the body of a top level defintion,
and count the number of BAD par expressions present.
Use the helper predicate "isBad" in your implementation.
Return the body unchanged, as well as the count numBadPars.
-}

--not using this 

countBadPars :: I.Expr I.Type -> OptParFn (I.Expr I.Type, Int)
countBadPars e = do
  -- currently a stub
  let y = isBad (I.Lit (LitIntegral 5) (I.extract e)) -- calling this so we don't get an "unused" warning
  return (e, fromEnum y)
