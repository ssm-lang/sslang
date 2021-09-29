module Ast2IR ( astToIR ) where

import qualified Ast as A
import qualified IR as I
import Data.Maybe ( mapMaybe )
import Prelude hiding ( id, lookup )
import Control.Monad.State

type SymbolTable = [I.Bind]

data IRGenState = IRGenState { nextLabel :: I.Lab
                             , symbolTable :: SymbolTable
                             , generatedStatements :: [I.Statement]
                             }

initialState :: SymbolTable -> IRGenState
initialState env = IRGenState { nextLabel = I.initialLabel
                              , symbolTable = env
                              , generatedStatements = []
                              }

type IRGenMonad = State IRGenState

freshLabel :: IRGenMonad I.Lab
freshLabel = do st <- get
                let lab = nextLabel st
                put $ st { nextLabel = I.nextLabel lab }
                return lab

lookup :: String -> IRGenMonad I.Bind
lookup n = do symbols <- gets symbolTable
              return $ lhelper symbols
  where lhelper [] = error $ "Undefined symbol " ++ n
        lhelper (b@(I.Bind n' _):_) | n' == n = b
        lhelper (_:syms) = lhelper syms              

emit :: I.Statement -> IRGenMonad ()
emit stmt = do st <- get
               put $ st { generatedStatements =
               stmt : generatedStatements st }

generateStatements :: SymbolTable -> IRGenMonad () -> [I.Statement]
generateStatements env f =
  reverse $ generatedStatements $ execState f (initialState env)

astToIR :: A.Program -> I.Program
astToIR (A.Program decls) = I.Program functions
  where    
    functions = mapMaybe convertFunction decls

    topDefs =
      I.Bind "Occur" (I.TCon "Event") :  -- FIXME: This is really ugly
      mapMaybe bindOfDecl decls

    bindOfDecl decl@(A.Function name _ _ _) = Just $ I.Bind name $ typeOfDecl decl
    --bindOfDecl _ = Nothing
    
    typeOfDecl (A.Function _ [singleFormal] _ rty) =
      I.functionType (formalTypes ++ [resultType rty])
      where
        resultType (A.ReturnType ty) = convertTy ty
        resultType _ = undefined
        formalTypes = bindTypes singleFormal
        bindTypes (A.TupBind binds _) = map (\(A.Bind _ (Just t)) -> convertTy t) binds
        bindTypes (A.Bind _ (Just t))  = [convertTy t]
        bindTypes _ = undefined
    typeOfDecl _ = undefined

    convertFunction decl@(A.Function fname [singleFormal] body _) =
      Just $ I.Function bind formals locals body'
      where
        functionType = typeOfDecl decl
        bind = I.Bind fname functionType
        formals = zipWith I.Bind formalNames $ I.functionArgsTypes functionType
        formalNames = bindNames singleFormal
        bindNames (A.TupBind binds _) = map (\(A.Bind id _) -> id) binds
        bindNames (A.Bind id _) = [id]

        locals = collectBinders body
        environment = locals ++ formals ++ topDefs
        body' = generateStatements environment $ emitStmt body
    convertFunction _ = undefined
{-    
    convertBind :: A.Bind -> [I.Bind]
    convertBind (A.Bind ids t) = map (\id -> I.Bind id t') ids
      where
        t' = convertTy t
-}

    convertTy (A.TCon "()") = I.unitTy
    convertTy (A.TCon t) = I.TCon t
    convertTy (A.TApp t1 t2) = I.TApp (convertTy t1) (convertTy t2)
    convertTy (A.TTuple (ty:tys)) = foldl (\tupApps t -> I.TApp tupApps (convertTy t))
                                          (I.TApp (I.TCon "(,)") (convertTy ty)) tys
    convertTy _ = undefined

    emitStmt :: A.Expr -> IRGenMonad ()
    emitStmt (A.Let _) = return () -- FIXME
    emitStmt (A.Seq e1 e2) = emitStmt e1 >> emitStmt e2
    emitStmt (A.Assign (A.PId id) e) = do bind <- lookup id
                                          e' <- toExpr e
                                          emit $ I.Assign bind e'
    emitStmt (A.Loop e) = do lab <- freshLabel
                             emit $ I.Label lab
                             emitStmt e
                             emit $ I.Goto lab
    emitStmt (A.While p e) = do
      firstLab <- freshLabel
      lastLab <- freshLabel
      emit $ I.Label firstLab
      p' <- toExpr p
      emit $ I.IfGoto (I.Unary I.Not p' I.boolTy) lastLab
      emitStmt e
      emit $ I.Goto firstLab
      emit $ I.Label lastLab
    emitStmt (A.IfElse e thenStmt A.NoExpr) = do
      lab <- freshLabel
      e' <- toExpr e
      emit $ I.IfGoto (I.Unary I.Not e' I.boolTy) lab
      emitStmt thenStmt
      emit $ I.Label lab
      
    emitStmt (A.IfElse e thenStmt elseStmt) = do
      elseLabel <- freshLabel
      contLabel <- freshLabel
      e' <- toExpr e      
      emit $ I.IfGoto (I.Unary I.Not e' I.boolTy) elseLabel
      emitStmt thenStmt
      emit $ I.Goto contLabel      
      emit $ I.Label elseLabel
      emitStmt elseStmt
      emit $ I.Label contLabel
      
    emitStmt (A.Later e1 (A.PId id) e2) = do bind <- lookup id
                                             e2' <- toExpr e2
                                             e1' <- toExpr e1
                                             emit $ I.After e1' bind e2'
    emitStmt (A.Wait ids) = do binds <- mapM lookup ids
                               emit $ I.Wait binds

    emitStmt (A.Par exprs) = do branches <- mapM toBranch exprs
                                emit $ I.Fork branches
      where
         toBranch e = let (f, args) = applyToCall e in
                       do args' <- mapM toExpr args
                          f' <- lookup f
                          return (f', args')
      
    emitStmt e@(A.Apply _ _) = do args' <- mapM toExpr args
                                  f' <- lookup f
                                  emit $ I.Fork [(f', args')]
      where (f, args) = applyToCall e

    
{-  emitStmt (A.Call f args) = do 
    emitStmt (A.Verbatim s) = emit $ I.Verbatim s -}

{-    emitStmt calls@(A.BinOp _ "||" (A.Call _ _)) = do
      let calls' = reverse $ collectCalls calls
      as <- mapM (mapM toExpr) (map snd calls')
      fs <- mapM lookup (map fst calls')
      emit $ I.Fork $ zip fs as       -}
      
    emitStmt A.NoExpr = return ()
    emitStmt e = error $ "Unsupported statement expression " ++ show e

    applyToCall :: A.Expr -> (String, [A.Expr])
    applyToCall e = let (A.Id f) : args = reverse $ collectArgs e in (f, args)
      where
        collectArgs (A.Apply e1 e2) = e2 : collectArgs e1
        collectArgs i@(A.Id _) = [i]
        collectArgs ex = error $ "unsupported expression in apply " ++ show ex

--    collectCalls _ = []
{-
    collectCalls (A.Call f args) = [(f, args)]
    collectCalls (A.BinOp calls "||" (A.Call f1 a1)) =
      (f1, a1) : collectCalls calls
    collectCalls e = error $ "Unexpected call expression " ++ show e
-}

    toExpr :: A.Expr -> IRGenMonad I.Expression
    toExpr (A.Literal (A.IntLit i)) = return $ I.IntLit i I.intTy
    toExpr (A.Literal (A.DurLit d)) = return $ I.DurLit d I.timeTy
    toExpr (A.Id v) = I.Var <$> lookup v
    toExpr (A.Apply (A.Apply (A.Id op) e1) e2) = do -- FIXME: incomplete
      e1' <- toExpr e1
      e2' <- toExpr e2
      let (op', ty) = case op of
            "+" -> (I.Add, I.intTy)
            "-" -> (I.Sub, I.intTy)
            "*" -> (I.Mult, I.intTy)
            "<" -> (I.LT, I.boolTy)
            "<=" -> (I.LE, I.boolTy)
            ">" -> (I.GT, I.boolTy)
            ">=" -> (I.GE, I.boolTy)
            "/=" -> (I.NE, I.boolTy)
            _   -> error $ "unsupported binary operator " ++ op
      return $ I.Binary e1' op' e2' ty
           
    toExpr e = error $ "Unsupported expression " ++ show e

    collectBinders :: A.Expr -> [I.Bind]
    collectBinders A.Id{} = []
    collectBinders A.Literal{} = []
    collectBinders (A.Apply e1 e2) = collectBinders e1 ++ collectBinders e2
    collectBinders (A.OpRegion e r) = collectBinders e ++ helper r
      where helper A.EOR = []
            helper (A.NextOp _ e' r') = collectBinders e' ++ helper r'
    collectBinders A.NoExpr = []
    collectBinders (A.Let defs) = concatMap collectDefBinder defs
      where
        collectDefBinder (A.Def (A.PId v) (A.Constraint e t)) = -- FIXME
          collectBinders e ++ [I.Bind v (convertTy t)]
        collectDefBinder d = error $ "unsupported let expression " ++ show d
    collectBinders (A.While e1 e2) = collectBinders e1 ++ collectBinders e2
    collectBinders (A.Loop e) = collectBinders e
    collectBinders (A.Par es) = concatMap collectBinders es
    collectBinders (A.IfElse e1 e2 e3) = concatMap collectBinders [e1,e2,e3]
    collectBinders (A.Later e1 _ e2) = collectBinders e1 ++ collectBinders e2
    collectBinders (A.Assign _ e) = collectBinders e
    collectBinders (A.Constraint e _) = collectBinders e
    collectBinders A.As{} = [] -- should just be a pattern
    collectBinders A.Wait{} = []
    collectBinders (A.Seq e1 e2)= collectBinders e1 ++ collectBinders e2
    collectBinders A.Wildcard = []
