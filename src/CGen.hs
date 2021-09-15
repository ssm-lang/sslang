{-|
Module:  CGen
Description: Translates an IR program into a C program

-}

module CGen(
  cgen
, hgen
) where

import Prelude hiding (id)
import Data.Maybe ( mapMaybe )
import Control.Monad.State hiding (void)
import Duration
import Data.Char ( toUpper )
import Data.Bits ( shiftL )
import Data.List ( intersperse )

import qualified IR as I
import C

-- | Macro instantiated at the beginning of each activation record
activationRecordFieldsMacro :: String
activationRecordFieldsMacro = "SSM_ACT_FIELDS"

-- | Expression for calling "enter" to set up the activation record
callEnterFunction :: [Expression] -> Expression
callEnterFunction acts = Call (ID "ssm_enter") acts

-- | Expression for calling "leave" to free the activation record
callLeaveFunction :: Expression -> Expression -> Expression
callLeaveFunction actPtr size = Call (ID "ssm_leave") [actPtr, size]

-- | Expression for calling "initialize" on a scheduled variable
callInitializeFunction :: I.Ty -> Expression -> Expression
callInitializeFunction t e = Call (ID $ "initialize_" ++ typeName t) [e]

-- | Expression for calling "later" on a scheduled variable
-- First argument is the time for the update, second argument is
-- a pointer to the variable to update, which will be cast to
-- a sv_t *

callLaterFunction :: I.Ty -> Expression -> Expression -> Expression
                  -> Expression
callLaterFunction (I.TCon "Event") time var _ =
  Call (ID $ "later_event") [ var, time ]
callLaterFunction typ time var value =
  Call (ID $ "ssm_later_" ++ typeName typ) [ var, time, value ]

callAssignFunction :: I.Ty -> Expression -> Expression -> Expression
                   -> Expression
callAssignFunction (I.TCon "Event") lvalue priority _ =
  Call (ID $ "assign_event") [ lvalue, priority ]                   
callAssignFunction typ lvalue priority rvalue =
  Call (ID $ "ssm_assign_" ++ typeName typ) [ lvalue, priority, rvalue ]

-- | Expression for calling "sensitize" on a scheduled variable
-- and a trigger.  First argument is a pointer to the variable;
-- second argument is an expression for the trigger
callSensitizeFunction :: Expression -> Expression -> Expression
callSensitizeFunction v t = Call (ID "ssm_sensitize") [ Cast (Pointer sv_t) v
                                                  , Pre Address t]

-- | Expression for calling "desensitize" on a trigger.  Argument is
-- the trigger.
callDesensitizeFunction :: Expression -> Expression
callDesensitizeFunction t = Call (ID "ssm_desensitize") [Pre Address t]


-- | Type of a "bare" activation record
bare_act_t :: CType
bare_act_t = namedType "ssm_act_t"

-- | C type of a trigger
trigger_t :: CType
trigger_t = namedType "ssm_trigger_t"

-- | C type of a priority number
priority_t :: CType
priority_t = namedType "ssm_priority_t"

-- | C type of a priority "depth" number
depth_t :: CType
depth_t = namedType "ssm_depth_t"

-- | C type of a bare scheduled variable
sv_t :: CType
sv_t = namedType "ssm_sv_t"

-- Name of the "step" function for a given function
stepf :: String -> String
stepf n = "step_" ++ n

-- Name of the "enter" function for a given function
enterf :: String -> String
enterf n = "enter_" ++ n

-- Name of the activation record type for a given function
actTypeName :: String -> String
actTypeName n = "act_" ++ n ++ "_t"

actStructName :: String -> String
actStructName n = "act_" ++ n

-- | Name of a numbered trigger
triggerName :: Int -> String
triggerName n = "trigger" ++ show n

-- | Expression for "now," the current model time
nowVar :: Expression
nowVar = ID "ssm_now()"

-- | Return the minimum such that n <= 2 ^ (log2ceil n) 
log2ceil :: Int -> Int
log2ceil n = fst $ head $ filter (\(_, nn) -> nn >= n)
                                 [ (k, 1 `shiftL` k) | k <- [1..] ]

-- | The main entry point
cgen :: String -> I.Program -> TranslationUnit
cgen modName (I.Program fs) = TranslationUnit $
                      TopVerbatim ("#include \"" ++ modName ++ ".h\"") :
                      concatMap toFunc fs
                      
-- CType for a simple named type, e.g., int or act_foo_t
namedType :: String -> CType
namedType n = Spec n NT


void :: CType
void = namedType "void"


-- | Convert an IR binder to a C binder
bindToBind :: I.Bind -> Bind
bindToBind (I.Bind s t) = Bind s $ cTypeOf t



-- | C type of an IR type
cTypeOf :: I.Ty -> CType
cTypeOf t = case I.stripRef t of
              Just t' -> Pointer (cTypeOf t')
              Nothing -> case I.stripSched t of
                           Just t' -> namedType $
                                         "ssm_" ++ typeName t' ++ "_t"
                           Nothing -> case t of
                             I.TCon "Int" -> namedType "int"
                             I.TCon "Bool" -> namedType "bool"
                             I.TCon "Time" -> namedType "ssm_time_t"
                             _ -> error $ "don't know the C type for " ++ show t
                             
-- Names used in the C program, e.g., for initialize_int()
typeName ::I.Ty -> String
typeName (I.TCon "Int") = "i32"
typeName (I.TCon "Bool") = "bool"
typeName (I.TCon "Event") = "event"
typeName t = error $ "don't know a name for " ++ show t


-- Return an assignment statement: variable = expression
--assign :: String -> Expression -> Statement
--assign v e = Expr $ Binary (ID v) Assign e

assign :: Expression -> Expression -> Expression
assign lv rv = Binary lv Assign rv

data CodeGenState = CodeGenState { nextPCState :: Int
                                 , generatedStatements :: [Statement]
                                 }

initialState :: CodeGenState
initialState = CodeGenState { nextPCState = 0
                            , generatedStatements = []
                            }

type CodeGenMonad = State CodeGenState

getNextPCState :: CodeGenMonad Expression
getNextPCState = do st <- get
                    let pcval = nextPCState st
                    put $ st { nextPCState = succ pcval }
                    return $ IntLit pcval

-- | Add a C statement to the generated code
emit :: Statement -> CodeGenMonad ()
emit stmt = do st <- get
               put $ st { generatedStatements =
                             stmt : generatedStatements st }

-- | Add a C expression statement to the generated code
emitExpr :: Expression -> CodeGenMonad ()
emitExpr e = emit $ Expr e

generateStatements :: CodeGenMonad () -> [Statement]
generateStatements f = reverse $ generatedStatements $ execState f initialState


-- | Basic parameters for an "enter" function

                    
toFunc :: I.Function -> [ExternalDeclaration]
toFunc (I.Function (I.Bind fname _) formals locals body) =
               [actTypedef, stepFun, enterFun]
  where

    numTriggers :: Int -- Maximum number ever needed at a single wait
    numTriggers = maxNeededTriggersStatements body

    localScheduledVariables :: [(String, I.Ty)]
    localScheduledVariables = mapMaybe scheduledBind locals
    
    scheduledBind (I.Bind v t) = case I.stripSched t of Just t' -> Just (v, t')
                                                        Nothing -> Nothing

    ---------- Activation record type definition

    act_t = namedType $ actTypeName fname  -- Type of our activation record

    actTypedef = External
                 ""
                 (Struct (actStructName fname) actFields NT)
                 NoInit

    actFields = Bind activationRecordFieldsMacro NT : -- Actually a #define
                varFields ++ triggerFields

    varFields = map bindToBind (formals ++ locals)

    triggerFields = [ Bind (triggerName n) trigger_t | n <- [1..numTriggers] ]

    ---------- "Enter" function definition
    
    enterFun =
      FunctionDefinition (enterf fname)
      (Function (Pointer act_t) $ baseParams ++ formalParams)
      enterBody

    baseParams = [ Bind parent (Pointer bare_act_t)
                 , Bind priority priority_t
                 , Bind depth depth_t]

    formalParams = map bindToBind formals
                 
    enterBody = Compound [enterActDecl] $
                map copyFormalToAct formals ++
                [initTriggers | numTriggers > 0] ++
                map initializeScheduled localScheduledVariables ++
                [actReturn]
                
    enterActDecl = Declaration act (Pointer act_t) callEnter
    callEnter = ExprInitializer $ Cast (Pointer act_t) $
                callEnterFunction [ Sizeof act_t
                                  , ID $ stepf fname
                                  , ID parent
                                  , ID priority
                                  , ID depth ]

    -- act->trigger2.act = act->trigger1.act = (continuation_t *) act
    initTriggers = Expr $ assignTriggerCont numTriggers

    assignTriggerCont :: Int -> C.Expression
    assignTriggerCont 0 = Cast (Pointer bare_act_t) (ID act)
    assignTriggerCont n = assign (triggerFieldCont n)
                                 (assignTriggerCont (pred n))

    triggerFieldCont n = Field (actField $ triggerName n) "act"

    -- initialize_bool( &act->mybool );
    initializeScheduled (v, t) = Expr $ callInitializeFunction t $
                                           Pre Address $ actField v
   
                      
    actReturn = Return $ Just (ID act)

    -- Formal parameters and locals in the C enter function for the routine
    parent   = "parent"
    priority = "priority"
    depth    = "depth"
    act      = "act"    

    -- Named field in the activation record
    actField :: String -> Expression
    actField = Arrow (ID act)

    copyFormalToAct :: I.Bind -> Statement
    copyFormalToAct (I.Bind f _) = Expr $ assign (actField f) (ID f)
                                     
    ---------- "Step" function definition

    stepFun = FunctionDefinition (stepf fname)
              (Function void [Bind bare_act (Pointer bare_act_t)]) $
              Compound [bodyActDecl] [ bodySwitch, finalLeave ]

    bare_act = "bare_act"
    bodyActDecl = Declaration act (Pointer act_t) $
                  ExprInitializer $ Cast (Pointer act_t) (ID bare_act)
    bodySwitch = Switch (actField "pc") $ Compound [] switchStmts
    switchStmts = generateStatements $ do entryPC <- getNextPCState
                                          emit $ Case entryPC
                                          mapM_ generate body

    generate :: I.Statement -> CodeGenMonad ()
    
    generate (I.Label l) = emit $ Label $ show l
    generate (I.Goto l) = emit $ Goto $ show l
    generate (I.IfGoto e l) = do
      e' <- genExpr e
      emit $ If e' (Goto $ show l)
    
    generate (I.Assign (I.Bind v typ) e) = do
      e' <- genExpr e
      emitExpr $ case I.stripRef typ of
        Just t -> case I.stripSched t of
          Just t' -> callAssignFunction t' (actField v) (actField priority) e'
          Nothing -> assign (Pre Deref (actField v)) e'
        Nothing -> case I.stripSched typ of
          Just t' -> callAssignFunction t' (Pre Address (actField v))
                                            (actField priority) e'
          Nothing -> assign (actField v) e'
            
    generate (I.After e1 b e2) = do
       e2' <- genExpr e2
       (vp, t) <- varPtr b
       e1' <- genExpr e1
       let typ = case I.stripSched t of
              Just t' -> t'
              Nothing -> error $ "after on unscheduled type " ++ show t
       emitExpr $ callLaterFunction typ (Binary nowVar Add e1') vp e2'
                                     
    generate (I.Wait binds) = do
      mapM_ sensitize $ zip binds [1..]
      pcVal <- getNextPCState
      emitExpr $ assign (actField "pc") pcVal
      emit $ Return Nothing
      emit $ Case pcVal
      mapM_ desensitize [1..length binds]
      where
        sensitize (b, tnum) = do
          (vp, _) <- varPtr b
          emitExpr $ callSensitizeFunction vp (actField $ triggerName tnum)
        desensitize tnum =
          emitExpr $ callDesensitizeFunction (actField $ triggerName tnum)

    -- Unary fork: a simple call
    generate (I.Fork [(f, a)]) = do
      pcVal <- getNextPCState
      emitExpr $ assign (actField "pc") pcVal
      enterCall <- genEnterCall (actField priority) (actField depth) (f, a)
      emitExpr $ Call (ID "call") [Cast (Pointer bare_act_t) enterCall]
      emit $ Return Nothing
      emit $ Case pcVal

    -- Multi-way fork
    generate (I.Fork children) = do
      let localDepth    = "new_depth"
          localPriority = "new_priority"
          localInc      = "priority_inc"
          
          childrenBits = log2ceil $ length children
          localDepthDecl = Declaration localDepth depth_t $
                              ExprInitializer $ Binary (actField depth) Sub
                                                        $ IntLit childrenBits
          localPriorityDecl = Declaration localPriority priority_t $
                                ExprInitializer $ actField priority
          localIncDecl = Declaration localInc priority_t $
                                ExprInitializer $ Binary (IntLit 1) Lshift
                                                          $ ID localDepth
          incrStmt = Expr $
                       Binary (ID localPriority) (AssignOp Add) (ID localInc)
      enterCalls <- mapM (genEnterCall (ID localPriority) (ID localDepth))
                         children
      let calls = map (\e -> Expr $ Call (ID "fork_routine")
                                    [Cast (Pointer bare_act_t) e]) enterCalls
      emit $ Compound [ localDepthDecl
                      , localPriorityDecl
                      , localIncDecl ] $ intersperse incrStmt calls
      pcVal <- getNextPCState
      emitExpr $ assign (actField "pc") pcVal
      emit $ Return Nothing
      emit $ Case pcVal

    generate (I.Verbatim s) = emit $ StmtVerbatim s


    genEnterCall priorityExpr depthExpr (I.Bind cfname ftype, args) = do
      let formalTypes = init $ I.functionArgsTypes ftype
      passedArgs <- mapM passArg $ zip formalTypes args
      let enterArgs = [ Cast (Pointer bare_act_t) (ID act)
                      , priorityExpr
                      , depthExpr ] ++ passedArgs
      return $ Call (ID $ enterf cfname) enterArgs
       where         
          passArg (formalType, actual) =
            case I.stripRef formalType of
              Nothing -> genExpr actual -- Pass-by-value argument
              Just _ -> -- Pass-by-reference argument
                case I.stripRef (I.exprType actual) of
                  Nothing -> Pre Address <$> genExpr actual -- Not a reference variable
                  Just _ -> case actual of
                    I.Var (I.Bind v _) -> return $ actField v -- Passing a reference by-reference
                    e -> error $ "passing illegal by-reference expression " ++ show e

    genExpr :: I.Expression -> CodeGenMonad Expression
    
    genExpr (I.Var (I.Bind v ty)) = return $ case I.stripRef ty of
      Nothing -> case I.stripSched ty of
         Nothing -> actField v -- local scalar
         Just _ -> Field (actField v) "value" -- local scheduled
      Just ty' -> case I.stripSched ty' of
         Nothing -> Pre Deref (actField v) -- referenced scalar
         Just _ -> Arrow (actField v) "value" -- referenced scheduled
                  
    genExpr (I.IntLit i _) = return $ IntLit (fromInteger i)
    
    genExpr (I.DurLit (Duration d) _) =
      return $ LongLit (fromIntegral d)

    genExpr (I.Unary I.Not e _) = do
      e' <- genExpr e
      return $ Pre LogNot e'
    genExpr (I.Binary e1 op e2 _) = do
      e1' <- genExpr e1
      e2' <- genExpr e2
      let op' = case op of
            I.Add -> Add
            I.Sub -> Sub
            I.Mult -> Mult
            I.LT -> Lt
            I.LE -> Le
            I.GT -> Gt
            I.GE -> Ge
            I.NE -> Neq
            -- _ -> error $ "unsupported operator " ++ show op
      return $ Binary e1' op' e2'
    
    -- genExpr e = error $ "unsupported expression " ++ show e

    -- Return an expression that is a pointer to the given variable
    -- and the type (after removing any Ref)
    varPtr :: I.Bind -> CodeGenMonad (Expression, I.Ty)
    varPtr (I.Bind n ty) = case I.stripRef ty of
      Just t -> return (field, t)
      Nothing -> return (Pre Address field, ty)
      where field = actField n

    finalLeave = Expr $ callLeaveFunction (Cast (Pointer bare_act_t) (ID act))
                                          (Sizeof act_t)
    

maxNeededTriggersStatements :: [I.Statement] -> Int
maxNeededTriggersStatements stmts = foldl max 0 $ map maxNeededTriggers stmts

maxNeededTriggers :: I.Statement -> Int
maxNeededTriggers (I.Wait waits) = length waits
maxNeededTriggers I.Assign{}   = 0
maxNeededTriggers I.After{}    = 0
maxNeededTriggers I.Fork{}     = 0
maxNeededTriggers I.Label{}    = 0
maxNeededTriggers I.IfGoto{}   = 0
maxNeededTriggers I.Goto{}     = 0
maxNeededTriggers I.Verbatim{} = 0



-- | Generate a header file for the program
hgen :: String -> I.Program -> TranslationUnit
hgen modName (I.Program fs) = TranslationUnit $
                        TopVerbatim ("#ifndef " ++ predicate) :
                        TopVerbatim ("#define " ++ predicate) :
                        TopVerbatim "#include \"ssm.h\"" :
                        concatMap externalDecls fs ++
                        [TopVerbatim "#endif"]
  where
    predicate = '_' : map toUpper modName ++ "_H"

    externalDecls (I.Function (I.Bind fname _) formals _ _) = [ structDef
                                                              , structTypedef
                                                              , enterExtern]
      where
        structDef = External "" (Struct (actStructName fname) [] NT) NoInit
        structTypedef = External (actTypeName fname)
          (Spec "typedef" (Struct (actStructName fname) [] NT)) NoInit
          
        enterExtern = External (enterf fname)
                         (Function (Pointer act_t) params) NoInit
        act_t = namedType $ actTypeName fname
        params = [Bind "" (Pointer bare_act_t)
                 , Bind "" priority_t
                 , Bind "" depth_t] ++ formalParams
        formalParams = map (\ (I.Bind _ t) -> Bind "" $ cTypeOf t) formals

                                              
