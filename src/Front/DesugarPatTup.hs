module Front.DesugarPatTup where

import qualified Common.Compiler as Compiler
import Common.Identifiers
import qualified Front.Ast as A
import           Data.Bifunctor                 ( first )

desugarTupPat :: A.Program -> Compiler.Pass A.Program
desugarTupPat (A.Program decls) = return $ A.Program $ desugarTop <$> decls
    where desugarTop (A.TopDef defs) = A.TopDef $ desugarDef defs
          desugarTop t = t
          desugarDef (A.DefFn i [] t e) = A.DefFn i [] t $ desugarExpr e
          desugarDef (A.DefFn i ps t e) =
                let (pats, rese) = desugarPatTup ps e (0::Int) in
                    A.DefFn i pats t $ desugarExpr rese
                where desugarPatTup [] ex _ = ([],ex)
                      desugarPatTup (p:rps) ex n = 
                        case p of
                            A.PatTup _ -> let (pats, rese) = desugarPatTup rps (A.Match (A.Id (Identifier ("_temp_id_" ++ show n))) [(p,ex)]) (n+1) in
                                        (A.PatId (Identifier ("_temp_id_" ++ show n)) : pats, rese)
                            _ -> first (p:) $ desugarPatTup rps e (n+1)
          desugarDef (A.DefPat p e) = A.DefPat p $ desugarExpr e
          desugarExpr (A.Let defs e) = desugarLet defs e
          desugarExpr e = e
          desugarLet defs e = 
            let (ndefs, rese) = desugarPatTup defs e (0::Int) in
                A.Let ndefs $ desugarExpr rese
            where desugarPatTup [] ex _ = ([],ex)
                  desugarPatTup (def:rdefs) ex n =
                    case def of
                        (A.DefPat p@(A.PatTup _) defe) ->
                            let (d, nexpr) = desugarPatTup rdefs (A.Match (A.Id (Identifier ("_temp_id_" ++ show n))) [(p,ex)]) (n+1) in
                                ((A.DefPat (A.PatId (Identifier ("_temp_id_" ++ show n))) defe) : d, nexpr)
                        _ -> first (def:) $ desugarPatTup rdefs e (n+1)
