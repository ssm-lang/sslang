-- | Translate SSM program to C compilation unit.
--
-- Each procedure in a program is turned into three components:
--
-- 1) A struct (the activation record)
-- 2) An initialization function (the enter function)
-- 3) A step function, which corresponds to the actual procedure body

{-# LANGUAGE QuasiQuotes #-}

module Codegen.Codegen where

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           Control.Monad.Except           ( ExceptT(..)
                                                , MonadError(..)
                                                , runExceptT
                                                )
import           Control.Monad.State.Lazy       ( State
                                                , evalState
                                                , gets
                                                , modify
                                                )

import           Codegen.Identifiers
import           Common.Identifiers             ( ident )
import qualified IR.IR                         as L
import qualified Types.Flat                    as L
import qualified Types.TypeSystem              as L

todo :: a
todo = error "Not yet implemented"

nope :: a
nope = error "Not yet supported"

type Type = L.Type
type Program = L.Program Type
type Expr = L.Expr Type
type PrimOp = L.PrimOp
type Primitive = L.Primitive
type Literal = L.Literal
type TypeDef = L.TypeDef Type

{- Notes about what is expected of the IR:

- All primitive functions are applied to the right number of arguments.
- All applications are fully-applied; the only time a term of type a -> b
  appears anywhere is on the left-hand side of an application.
- No closures; the only kinds of terms that may appear on the left side of an
  application is a Var or another application.
-}

{- | State maintained while compiling a 'Procedure'.

The information here is populated while generating the step function, so that
should be computed first, before this information is used to generate the act
struct and enter definitions.
-}
data TRState = TRState
  { fnName   :: CIdent
  , fnArgs   :: [(CIdent, C.Type)]
  , locals   :: [(CIdent, C.Type)]
  , fnBody   :: Expr
  , caseNum  :: Int
  , maxWaits :: Int
  , tmpNum   :: Int
  }

data CompileError = CompileErrorMsg String | CompileErrorOther

-- | Translation monad.
type TR a = ExceptT CompileError (State TRState) a

unwrapJust :: Maybe t -> TR t
unwrapJust = maybe (throwError CompileErrorOther) return

-- | Run a TR computation on a procedure.
runTR :: CIdent -> [(CIdent, C.Type)] -> Expr -> TR a -> Either CompileError a
runTR name args body tra = evalState (runExceptT tra) $ TRState { fnName = name
                                                                , fnArgs = args
                                                                , fnBody = body
                                                                , caseNum = 0
                                                                , locals = []
                                                                , maxWaits = 0
                                                                , tmpNum = 0
                                                                }

-- | Read and increment the number of cases in a procedure, i.e., caseNum++.
nextCase :: TR Int
nextCase = do
  n <- gets caseNum
  modify $ \st -> st { caseNum = n + 1 }
  return n

-- | Register a local variable for which an sv should be allocated.
addLocal :: (CIdent, C.Type) -> TR ()
addLocal l = modify $ \st -> st { locals = l : locals st }

-- | Register a name to be waited on
addWait :: Int -> TR ()
addWait n = modify $ \st -> st { maxWaits = n `max` maxWaits st }

-- | Read and increment the number of temporaries allocated, i.e., tmpNum++
nextTmp :: C.Type -> TR CIdent
nextTmp ty = do
  t <- tmpOf <$> gets tmpNum
  modify $ \st -> st { tmpNum = tmpNum st + 1 }
  addLocal (t, ty)
  return t

-- | Return type of current function.
returnType :: TR L.Type
returnType =
  snd . L.dearrow . L.typeExpr . snd . L.collectLambda <$> gets fnBody

{-------- Compilation-specific identifiers --------}

-- | Identifier for generic (inner) struct act.
actg :: CIdent
actg = "actg"

-- | Identifier for specialized (outer) struct act.
acts :: CIdent
acts = "acts"

-- | Identifier for act member in act struct.
actm :: CIdent
actm = "act"

-- | Per-variable trigger
trigOf :: Int -> CIdent
trigOf i = "__trig_" ++ show i

-- | Temporary variable name
tmpOf :: Int -> CIdent
tmpOf i = "__tmp_" ++ show i

-- | Argument name from 'Binder', using number to generate name if necessary.
argName :: L.Binder -> Int -> CIdent
argName (Just v) _ = ident v
argName Nothing  i = "__arg_" ++ show i

leaveLabel :: CIdent
leaveLabel = "__leave_process"

-- | Name of return argument.
ret :: CIdent
ret = "__return_val"

-- | Return the C variant of the base type of a SSM type
typeIdent :: Type -> CIdent
typeIdent (L.TCon     t ) = ident t
typeIdent (L.TBuiltin tb) = typeBuiltin tb
 where
  typeBuiltin L.Unit        = todo
  typeBuiltin L.Void        = todo
  typeBuiltin (L.Arrow _ _) = nope
  typeBuiltin (L.Ref   t  ) = sv_ $ typeIdent t -- TODO: doesn't work for nested Refs
  typeBuiltin (L.Tuple _  ) = nope

ctype :: CIdent -> C.Type
ctype t = [cty|typename $id:t|]

genType :: Type -> C.Type
genType = todo

{-------- Compilation --------}

-- | Include statements in the generated file
includes :: [C.Definition]
includes = [cunit|
$esc:("#include \"ssm-platform.h\"")
|]

-- | Setup the entry point of the program.
genInitProgram :: Program -> [C.Definition]
genInitProgram L.Program { L.programEntry = entry } = [cunit|
  int $id:initialize_program(void) {
     $id:activate($id:(enter_ $ ident entry)(&$id:top_parent, $id:root_priority, $id:root_depth));
    return 0;
  }
|]

{- | Generate definitions for an SSM 'Procedure'.
-}
genTop :: (L.VarId, Expr) -> Either CompileError [C.Definition]
genTop (n, l@(L.Lambda _ _ ty)) = runTR name params body $ do
  (stepDecl , stepDefn ) <- genStep
  (enterDecl, enterDefn) <- genEnter
  structDefn             <- genStruct
  return [structDefn, enterDecl, stepDecl, enterDefn, stepDefn]
 where
  name            = ident n
  (args  , body ) = L.collectLambda l
  (argTys, retTy) = L.dearrow ty
  argIds          = zipWith argName args [0 ..]
  params          = map param $ zip argIds argTys ++ [(ret, L.ref retTy)]

  param (a, t) = (ident a, genType t)

genTop (_, L.Lit _ _) = todo
genTop (_, _        ) = nope

{- | Generate struct definition for an SSM 'Procedure'.

This is where local variables, triggers, and parameter values are stored.
-}
genStruct :: TR C.Definition
genStruct = do
  name <- gets fnName
  args <- gets fnArgs
  ts   <- gets maxWaits
  ls   <- gets locals
  return [cedecl|
    typedef struct {
      $ty:act_t $id:actm;

      $sdecls:(map structField args)
      $sdecls:(map structField ls)
      $sdecls:(map trig [1..ts])

    } $id:(act_ name);
  |]
 where
  structField :: (CIdent, C.Type) -> C.FieldGroup
  structField (n, t) = [csdecl|$ty:t $id:n;|]

  trig :: Int -> C.FieldGroup
  trig i = [csdecl|$ty:trigger_t $id:(trigOf i);|]


{- | Generate the enter function for an SSM 'Procedure'.

Its struct is allocated and initialized (partially; local variables' values are
left uninitialized).
-}
genEnter :: TR (C.Definition, C.Definition)
genEnter = do
  actname <- gets fnName
  args    <- gets fnArgs
  ts      <- gets maxWaits
  let act   = [cty|typename $id:act'|]
      act'  = act_ actname -- hack to use this typename as expr in macros
      enter = enter_ actname
      step  = step_ actname

      param (n, t) = [cparam|$ty:t $id:n|]
      initParam (n, _) = [[cstm|acts->$id:n = $id:n;|]]
      initTrig i = [cstm| $id:acts->$id:(trigOf i).act = $id:actg;|]

      params =
        [cparams|$ty:act_t *caller, $ty:priority_t priority, $ty:depth_t depth|]
          ++ map param args
  return
    ( [cedecl|$ty:act_t *$id:enter($params:params);|]
    , [cedecl|
        $ty:act_t *$id:enter($params:params) {
          $ty:act_t *$id:actg = $id:act_enter(sizeof($ty:act), $id:step, caller, priority, depth);
          $ty:act *$id:acts = container_of($id:actg, $id:act', act);

          /* Assign parameters */
          $stms:(concatMap initParam args)

          /* Initialize triggers */
          $stms:(map initTrig [1..ts])

          return $id:actg;
        }
      |]
    )

{- | Generate the step function for an SSM 'Procedure'.

This function just defines the function definition and switch statement that
wraps the statements of the procedure. The heavy lifting is performed by
'genExpr'.
-}
genStep :: TR (C.Definition, C.Definition)
genStep = do
  actName   <- gets fnName
  actBody   <- gets fnBody
  _         <- nextCase -- Toss away 0th case
  (_, stms) <- genExpr actBody -- Toss away return value
  -- ls        <- gets locals
  let act  = [cty|typename $id:actt|]
      actt = act_ actName -- hack to use this typename as expr in macros
      step = step_ actName

      -- | Dequeue any outstanding event on a reference
      -- dequeue :: (L.VarId, Type) -> C.BlockItem
      -- dequeue (n, _) = [citem|$id:unsched_event(&$id:acts->$id:n.sv);|]
  return
    ( [cedecl|void $id:step($ty:act_t *$id:actg);|]
    , [cedecl|
        void $id:step($ty:act_t *$id:actg) {
          $ty:act *$id:acts = container_of($id:actg, $id:actt, act);

          switch ($id:actg->pc) {
          case 0:;
            $items:stms

          default:
            break;
          }
        $id:leaveLabel:;
          $id:act_leave($id:actg, sizeof($ty:act));
        }
      |]
    )

-- TODO: we currently don't do anything to differentiate between when
-- a Var is a top-level reference vs when it is a local variable. Here
-- we're just inferring it from how it's used, i.e., on the LHS of an app vs on
-- its own.
genExpr :: Expr -> TR (C.Exp, [C.BlockItem])
genExpr (L.Var n (L.TBuiltin (L.Arrow _ _))) =
  -- Any identifiers of L.Arrow type must refer to a (global) function, so we
  -- just return a handle to its enter function.
  return ([cexp|$id:(enter_ $ ident n)|], [])
genExpr (L.Var n _) = do
  ls <- gets locals
  as <- gets fnArgs
  -- Check if the identifier is a local or a global variable.
  case lookup (ident n) (ls ++ as) of
    Just _ -> -- Local variables are stored in the activation record
      return ([cexp|&$id:acts->$id:(ident n)|], [])
    Nothing -> -- Who knows how to access global variables (yet)
      todo
genExpr (L.Data _ _             ) = nope
genExpr (L.Lit  l t             ) = genLiteral l t
genExpr (L.Let [(Just n, v)] b _) = do
  addLocal (ident n, genType $ L.typeExpr v)
  (defVal, defStms) <- genExpr v
  let var = ident n
      defBlock = [citems|
          $items:defStms
          $id:(initialize_ $ typeIdent $ L.typeExpr v)(&$id:acts->$id:var);
          $id:acts->$id:var.$id:value = $exp:defVal;
        |]
  (bodyVal, bodyStms) <- genExpr b
  return (bodyVal, defBlock ++ bodyStms)
genExpr L.Let{}          = error "Cannot handle mutually recursive bindings"
genExpr a@(L.App _ _ ty) = do
  let (fn, as) = L.collectApp a
  (fnEnter, fnStms ) <- genExpr fn
  (argVals, argStms) <- unzip <$> mapM genExpr as
  tmp                <- nextTmp $ ctype $ sv_ $ typeIdent ty
  yield              <- genYield
  let call = [citems|
          $id:(initialize_ $ typeIdent ty)(&$id:acts->$id:tmp);
          $id:activate($exp:fnEnter($args:argVals, &$id:acts->$id:tmp));
        |]
      unsched = [citems|$id:unsched_event(&$id:acts->$id:tmp.sv);|]
  return
    ( [cexp|$id:acts->$id:tmp|]
    , fnStms ++ concat argStms ++ call ++ yield ++ unsched
    )
genExpr L.Match{}       = nope
genExpr L.Lambda{}      = error "Cannot handle lambdas"
genExpr (L.Prim p es t) = genPrim p es t

genPrim :: Primitive -> [Expr] -> Type -> TR (C.Exp, [C.BlockItem])
genPrim L.New [e, _] _ = do
  (val, stms) <- genExpr e
  let sv_type = ctype $ sv_ $ typeIdent $ L.typeExpr e
  tmp <- nextTmp [cty|$ty:sv_type *|]
  let alloc = [citems|
          $id:acts->$id:tmp = malloc(sizeof($ty:sv_type));
          *$id:acts->$id:tmp = $exp:val;
        |]
  return ([cexp|$id:acts->$id:tmp|], stms ++ alloc)
genPrim L.Deref [a] _ = do
  (val, stms) <- genExpr a
  -- SSM Refs are all SVs, so we access the value field in order to dereference.
  -- TODO: handle event type
  return ([cexp|$exp:val->$id:value|], stms)
genPrim L.Assign [lhs, rhs] _ = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  -- TODO: handle event type
  let Just lhsTy = L.deref $ L.typeExpr lhs
      assignBlock = [citems|
          $items:lhsStms
          $items:rhsStms
          $id:(assign_ $ typeIdent lhsTy)($exp:lhsVal, $id:actg->$id:priority, $exp:rhsVal);
        |]
  return (unit, assignBlock)
genPrim L.After [time, lhs, rhs] _ = do
  (timeVal, timeStms) <- genExpr time
  (lhsVal , lhsStms ) <- genExpr lhs
  (rhsVal , rhsStms ) <- genExpr rhs
  -- TODO: handle event type
  let Just lhsTy = L.deref $ L.typeExpr lhs
      laterBlock = [citems|
          $items:timeStms
          $items:lhsStms
          $items:rhsStms
          $id:(later_ $ typeIdent lhsTy)
            ($exp:lhsVal, $exp:rhsVal, $id:now() + $exp:timeVal);
        |]
  return (unit, laterBlock)
genPrim L.Fork procs _ = do
  yield <- genYield
  let
    depthSub =
      (ceiling $ logBase (2 :: Double) $ fromIntegral $ length procs) :: Int
    newDepth = [cexp|actg->depth - $int:depthSub|]
    checkNewDepth = [citems|
                        if ($id:actg->$id:depth < $int:depthSub)
                          $id:throw($id:exhausted_priority);
                      |]

    genActivate :: (Int, Expr) -> TR ([C.BlockItem], C.BlockItem)
    genActivate (i, L.App (L.Var var _) a _) = do
      (arg, argStms) <- genExpr a
      let args =
            [ [cexp|$id:actg|]
            , [cexp|$id:actg->$id:priority + $int:i * (1 << $exp:newDepth)|]
            , arg
            ]
      return
        (argStms, [citem|$id:activate($id:(enter_ $ ident var)($args:args));|])
    -- For now, we only support forking expressions that have a top-level
    -- application (i.e., no thunks), whose left operand is a var.
    genActivate _ = nope

  (evals, activates) <- unzip <$> mapM genActivate (zip [0 ..] procs)
  return (unit, checkNewDepth ++ concat evals ++ activates ++ yield)
-- For forks, we expect that every argument is some kind of application
genPrim L.Wait vars _ = do
  (varVals, varStms) <- unzip <$> mapM genExpr vars
  addWait $ length varVals
  yield <- genYield
  let trigs = zip [1 :: Int ..] varVals
      sens (i, var) =
        [citem|$id:sensitize($exp:var, &$id:acts->$id:(trig_ i));|]
      desens (i, _) = [citem|$id:desensitize(&$id:acts->$id:(trig_ i));|]
  return (unit, concat varStms ++ map sens trigs ++ yield ++ map desens trigs)
genPrim L.Loop [b] _ = do
  (_, bodyStms) <- genExpr b
  return (unit, [citems|for (;;) { $items:bodyStms }|])
genPrim L.Break  []  _ = return (undef, [citems|break;|])
genPrim L.Return [e] _ = do
  (val, stms) <- genExpr e
  retTy       <- returnType
  -- Assign to return argument and jump to leave
  let retBlock = [citems|
                    $id:(assign_ $ typeIdent retTy)
                      ($id:acts->$id:ret, $id:actg->$id:priority, $exp:val);
                    goto $id:leaveLabel;
                 |]
  return (undef, stms ++ retBlock)
genPrim (L.PrimOp op) es t = genPrimOp op es t
genPrim _ _ _ = error "Unsupported Primitive or wrong number of arguments"

genLiteral :: Literal -> Type -> TR (C.Exp, [C.BlockItem])
genLiteral (L.LitIntegral i    ) _ = return ([cexp|$int:i|], [])
genLiteral (L.LitBool     True ) _ = return ([cexp|true|], [])
genLiteral (L.LitBool     False) _ = return ([cexp|false|], [])

genPrimOp :: PrimOp -> [Expr] -> Type -> TR (C.Exp, [C.BlockItem])
genPrimOp L.PrimAdd [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal + $exp:rhsVal|], stms)
genPrimOp L.PrimSub [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal - $exp:rhsVal|], stms)
genPrimOp L.PrimMul [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal * $exp:rhsVal|], stms)
genPrimOp L.PrimDiv [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal / $exp:rhsVal|], stms)
genPrimOp L.PrimMod [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal % $exp:rhsVal|], stms)
genPrimOp L.PrimNeg [opr] _ = do
  (val, stms) <- genExpr opr
  return ([cexp|- $exp:val|], stms)
genPrimOp L.PrimBitAnd [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal & $exp:rhsVal|], stms)
genPrimOp L.PrimBitOr [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal | $exp:rhsVal|], stms)
genPrimOp L.PrimBitNot [opr] _ = do
  (val, stms) <- genExpr opr
  return ([cexp|~ $exp:val|], stms)
genPrimOp L.PrimEq [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal == $exp:rhsVal|], stms)
genPrimOp L.PrimNot [opr] _ = do
  (val, stms) <- genExpr opr
  return ([cexp|! $exp:val|], stms)
genPrimOp L.PrimGt [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal < $exp:rhsVal|], stms)
genPrimOp L.PrimGe [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal <= $exp:rhsVal|], stms)
genPrimOp L.PrimLt [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal > $exp:rhsVal|], stms)
genPrimOp L.PrimLe [lhs, rhs] _ = do
  (lhsVal, rhsVal, stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal >= $exp:rhsVal|], stms)
genPrimOp _ _ _ = error "Unsupported PrimOp or wrong number of arguments"

-- | Helper for sequencing across binary operations.
genBinop :: Expr -> Expr -> TR (C.Exp, C.Exp, [C.BlockItem])
genBinop lhs rhs = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  return (lhsVal, rhsVal, lhsStms ++ rhsStms)

genYield :: TR [C.BlockItem]
genYield = do
  next <- nextCase
  return [citems|
    $id:actg->$id:pc = $int:next;
    return;
    case $int:next:;
    |]

-- | The unit value, the singleton inhabitant of the type Unit.
unit :: C.Exp
unit = [cexp|0|]

-- | Undefined "value", the fake value used for expressions of type Void.
undef :: C.Exp
undef = [cexp|0xdeadbeef|]

-- | Generate definitions for SSM type definitions.
genTypeDef :: TypeDef -> C.Definition
genTypeDef = todo
