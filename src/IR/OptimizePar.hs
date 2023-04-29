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

{- | Entry-point to Par Optimization.

Maps over top level definitions, removing unnecessary pars.
-}
optimizePar :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
optimizePar p = runOptParFn $ do
  defs' <- everywhereM (mkM findFixBadPar) $ I.programDefs p
 -- optimizedDefs <- mapM optimizeParTop $ I.programDefs p
 -- fail ("Number of Bad Par Exprs in " ++ show (map fst (map tupleMatch1 optimizedDefs)) ++ ": " ++ (show (map tupleMatch optimizedDefs)))
 -- return $ p{I.programDefs = map tupleMatch1 optimizedDefs}
  return $ p{I.programDefs = defs'}

{- | Given an Expr as input, if it turns out to be a bad Par expr, rewrite it.

Otherwise, leave the expression alone.
-}
findFixBadPar :: I.Expr I.Type -> OptParFn (I.Expr I.Type)
findFixBadPar e@__ = if isBad e then rewrite e else pure e 
 where rewrite :: I.Expr I.Type -> OptParFn (I.Expr I.Type)
       rewrite p@(I.Prim I.Par exprlist _) = pure dummy --TODO: rewrite the bad par as good one
       rewrite _ = fail "rewrite should only be called on a Par IR node!"
       dummy = I.Var (I.VarId (Identifier "PINEAPPLE")) (TVar $ TVarId (Identifier "dummy"))



--helper to pattern match on optimizedDefs tuple

tupleMatch :: (I.VarId, I.Expr I.Type, (Int,Int)) -> (Int, Int)
tupleMatch (_, _, theInt) = theInt

--another helper to pattern match on optimizedDefs tuple 

tupleMatch1 :: (I.VarId, I.Expr I.Type, (Int,Int)) -> (I.VarId, I.Expr I.Type)
tupleMatch1 (nm,rhs, _) = (nm,rhs)

-- TO DO make sure works correctly

-- | Given a top-level definition, detect + replace unnecessary par expressions
optimizeParTop :: (I.VarId, I.Expr I.Type) -> OptParFn (I.VarId, I.Expr I.Type, (Int, Int))
optimizeParTop (nm, rhs) = do
  
 -- rhs' <- detectReplaceBadPar rhs
  (badParCount) <- countPars rhs -- calling this so we don't get an "unused" warning
  --(rhs''', _) <- countBadPars rhs' -- calling this so we don't get an "unused" warning
  -- uncomment the line below to test countPars
  -- (_, result) <- countPars rhs
  --_ <- fail (show nm ++ ": Number of Par Exprs: " ++ show theint)
  -- uncomment the two lines below to test countBadPars
  -- (_, result') <- countBadPars rhs
  -- _ <- fail (show nm ++ ": Number of Bad Par Exprs: " ++ show result')
  
  -- if bad par 
  return (nm, rhs, badParCount)
 -- return (nm, rhs', snd theint)


-- | Detect Unnecessary Par Expressions + Replace With Equivalent Sequential Expression

-- call is bad par on itself, if yess, change the par expression into a sequence of let expressions
  --in par node, call detect and replace bad par, call is bad par, if am bad, return let 
    --return tuple of arguments of par 
--list of stuff on par node is the arguments to the par expression, sos add x 0 and add y 0, both of those are expr nodes
--  let q = par add x 0
--                add y 0
--    let q =(add x 0, add y 0)
--    let r = (x,y)

--detectReplaceBadPar :: I.Expr I.Type -> OptParFn (I.Expr I.Type)
--detectReplaceBadPar (rhs) = do
  --pure e -- for now, just return the same thing (don't do anyting)
--  if isBad rhs == 0 then do
  --  rhs' <- rhs
   -- return rhs'
  --else do
   -- replaceBadPar rhs


--replaceBadPar :: I.Expr I.Type -> OptParFn (I.Expr I.Type)
--replaceBadPar (rhs) = do
  --rhs' <- rhs
  --return rhs'

{- | 1) Count Par Nodes

Practice Exercise to Delete Later!

Traverse the IR representation of the body of a top level defintion,
and count the number of par expressions present.
Return the body unchanged, as well as the count numPars.
-}
countPars :: I.Expr I.Type -> OptParFn (Int, Int)
--countPars e = do 
--countPars var ( I.Var _ _) = pure (var, 0)
--countPars e = pure(e,87)
countPars ( I.Var thevar t) = pure(0,0) 
countPars ( I.Data thedata t) = pure(0,0) 
countPars ( I.Lit theliteral t) = pure(0,0) 
countPars (I.Exception exceptype t) = pure(0,0) 

--To Do finish case for app and match 

countPars (I.App theExpr theExpr2 t) = do
    counterFirst <- countPars theExpr
    counterSecond <- countPars theExpr2 
    let left = fst counterFirst + fst counterSecond
    let right = snd counterFirst + snd counterSecond
    return (left,right)

countPars (I.Lambda _ exprbody t) = countPars exprbody



{-
let x = 5
    y  = 6 
    x+y
I.Let [("x", 5),("y",6)] (I.Prim PrimOp [x,y])
Let [(Binder, Expr t)] (Expr t) t
-}
--recurse on the exprlist and the exprbody and add them together
--in the right hand side, examine all of the contents of exprlist, recurse countPars on contents and recurse on Expr body and sum counts

countPars( I.Let exprlist exprbody t) = do 
  (numGoodParsInBody,numBadParsInBody) <- countPars exprbody 
--call countPars on all the 2nd index of the nodes in the exprList 
--TO DO: make moore readable
  listPars <- mapM countPars (map snd (exprlist))
  let unzipped = (unzip listPars)
  let sumPars = bimap sum sum unzipped
 -- (numGoodParsInBinders,numBadParsInBinders) <- bimap  sum (unzip listPars)
     
  let finalGoodCount =  numGoodParsInBody + fst sumPars
  let finalBadCount = numBadParsInBody + snd sumPars
  -- return (finalCount,counterBadPars + counterBadPars1)
  return (finalGoodCount, finalBadCount)
-- tedius, check return types are tuples
 -- change the names and types for below 

--countPars (I.Prim )
--pattern match for primitive, then do a case for the nickname of primitive if it is par, then increment 
--look at IR.hs for Prim, might have to recurse on something in prim, determine if prim is a par, or something else, and increment


--countPars (I.Prim I.Par exprlist t) = do 
  --(numGoodParsInList,numBadParsInList) <- mapM countPars exprlist
  --numBadParsInList <- numBadParsInList + fromEnum (isBad (I.Prim I.Par exprlist t))
  --let numGoodParsInList = numGoodParsInList + 1 
  --return (numGoodParsInList, numBadParsInList)
--countPars (I.Prim thePrimitive exprlist t) = do 
  -- (numGoodParsInList,numBadParsInList) <- mapM countPars exprlist
   --return (numGoodParsInList,numBadParsInList)

countPars (I.Prim I.Par exprlist t) = do 
  listPars <- mapM countPars exprlist
  let unzipped = (unzip listPars)
  let sumPars = bimap sum sum unzipped
  let finalGoodCount =  fst sumPars
  let finalBadCount = snd sumPars + fromEnum (isBad (I.Prim I.Par exprlist t))
  return (finalGoodCount, finalBadCount)
countPars (I.Prim thePrimitive exprlist t) = do 
  listPars <- mapM countPars (exprlist)
  let unzipped = (unzip listPars)
  let sumPars = bimap sum sum unzipped
  return sumPars
countPars(_) = pure (0,0)

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
  (and (map isNotWait exprlist)) || (and (map isNotFunction exprlist))
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
