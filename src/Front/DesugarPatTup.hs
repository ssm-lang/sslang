module Front.DesugarPatTup (desugarPatTup) where

import qualified Common.Compiler as Compiler
import Common.Identifiers
import Data.Bifunctor (first)
import Data.Generics (Data (..), everywhere, mkT)
import qualified Front.Ast as A


desugarPatTup :: A.Program -> Compiler.Pass A.Program
desugarPatTup p = return $ desugarSubst p


desugarSubst :: (Data a) => a -> a
desugarSubst = desugarExpr . desugarDef


desugarDef :: (Data a) => a -> a
desugarDef = everywhere $ mkT substDef


desugarExpr :: (Data a) => a -> a
desugarExpr = everywhere $ mkT substExpr


substDef :: A.Definition -> A.Definition
substDef (A.DefFn i ps t e) = A.DefFn i pats t rese
 where
  desugarPat [] ex _ = ([], ex)
  desugarPat (p : rps) ex n =
    case p of
      A.PatTup _ ->
        let (rpats, rexpr) = desugarPat rps (A.Match (A.Id (Identifier ("_temp_id_" ++ show n))) [(p, ex)]) (n + 1)
         in (A.PatId (fromString ("_temp_id_" ++ show n)) : rpats, rexpr)
      _ -> first (p :) $ desugarPat rps e (n + 1)
  (pats, rese) = desugarPat ps e (0 :: Int)
substDef e = e


substExpr :: A.Expr -> A.Expr
substExpr (A.Let defs e) = A.Let ndefs rese
 where
  desugarPat [] ex _ = ([], ex)
  desugarPat (def : rdefs) ex n =
    case def of
      (A.DefPat p@(A.PatTup _) defe) ->
        let (d, nexpr) = desugarPat rdefs (A.Match (A.Id (fromString ("_temp_id_" ++ show n))) [(p, ex)]) (n + 1)
         in (A.DefPat (A.PatId (fromString ("_temp_id_" ++ show n))) defe : d, nexpr)
      _ -> first (def :) $ desugarPat rdefs e (n + 1)
  (ndefs, rese) = desugarPat defs e (0 :: Int)
substExpr e = e
