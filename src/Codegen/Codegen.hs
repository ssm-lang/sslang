-- | Translate SSM program to C compilation unit.
--
-- Each procedure in a program is turned into three components:
--
-- 1) A struct (the activation record)
-- 2) An initialization function (the enter function)
-- 3) A step function, which corresponds to the actual procedure body

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

module Codegen.Codegen where

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           Control.Monad.Except           ( ExceptT(..)
                                                , MonadError(..)
                                                , runExceptT
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , State
                                                , evalState
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )

import           Codegen.Identifiers
import           Common.Identifiers             ( fromId )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
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
type VarId = L.VarId
type Binder = L.Binder

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
  { fnName   :: VarId
  , fnArgs   :: [(Binder, Type)]
  , fnRetTy  :: Type
  , fnLocs   :: [(VarId, Type)]
  , fnBody   :: Expr
  , caseNum  :: Int
  , maxWaits :: Int
  , tmpNum   :: Int
  }

data CompileError = CompileErrorMsg String | CompileErrorOther

-- | Translation monad.
newtype TR a = TR (ExceptT CompileError (State TRState) a)
  deriving Functor                    via (ExceptT CompileError (State TRState))
  deriving Applicative                via (ExceptT CompileError (State TRState))
  deriving Monad                      via (ExceptT CompileError (State TRState))
  deriving (MonadError CompileError)  via (ExceptT CompileError (State TRState))
  deriving (MonadState TRState)       via (ExceptT CompileError (State TRState))

instance MonadFail TR where
  fail = throwError . CompileErrorMsg

-- | Run a TR computation on a procedure.
runTR
  :: VarId                  -- ^ Name of procedure
  -> [(Binder, Type)]       -- ^ Names and types of arguments to procedure
  -> Type                   -- ^ Name and type of return parameter of procedure
  -> Expr                   -- ^ Body of procedure
  -> TR a                   -- ^ Translation monad to run
  -> Either CompileError a  -- ^ Compilation may fail
runTR name args ret body (TR tra) = evalState (runExceptT tra) $ TRState
  { fnName   = name
  , fnArgs   = args
  , fnRetTy  = ret
  , fnBody   = body
  , caseNum  = 0
  , fnLocs   = []
  , maxWaits = 0
  , tmpNum   = 0
  }

-- | Read and increment the number of cases in a procedure, i.e., caseNum++.
nextCase :: TR Int
nextCase = do
  n <- gets caseNum
  modify $ \st -> st { caseNum = n + 1 }
  return n

-- | Register a local variable for which an sv should be allocated.
addLocal :: (VarId, Type) -> TR ()
addLocal l = modify $ \st -> st { fnLocs = l : fnLocs st }

-- | Register a name to be waited on
addWait :: Int -> TR ()
addWait n = modify $ \st -> st { maxWaits = n `max` maxWaits st }

-- | Read and increment the number of temporaries allocated, i.e., tmpNum++
nextTmp :: Type -> TR VarId
nextTmp ty = do
  t <- fromId . tmp_ <$> gets tmpNum -- TODO: get rid of fromId
  modify $ \st -> st { tmpNum = tmpNum st + 1 }
  addLocal (t, ty)
  return t

isLocalVar :: VarId -> TR Bool
isLocalVar name = do
  args <- gets fnArgs
  locs <- gets fnLocs
  return $ isJust (lookup (Just name) args) || isJust (lookup name locs)

{-------- Type compilation --------}

-- | Wrap a 'C.Type' with pointers, according to some 'L.Type'.
ptrs_ :: L.Type -> C.Type -> C.Type
ptrs_ (L.TBuiltin (L.Ref _)) t = [cty|$ty:t *|]
ptrs_ _                      t = t
-- ^ TODO: this does not handle nested pointers

typeId :: Type -> CIdent
typeId (L.TCon     t ) = fromId t
typeId (L.TBuiltin bt) = builtinId bt

builtinId :: L.Builtin Type -> CIdent
builtinId L.Unit        = todo
builtinId L.Void        = todo
builtinId (L.Arrow _ _) = todo
builtinId (L.Tuple _  ) = todo
builtinId (L.Ref   t  ) = sv_ $ typeId t -- NOTE: this does not add pointers

genType :: Type -> C.Type
genType typ = ptrs_ typ $ ctype $ typeId typ

genInit :: Type -> C.Exp
genInit ty = [cexp|$id:(initialize_ $ typeId $ fromJust $ L.deref ty)|]

genAssign :: Type -> C.Exp
genAssign ty = [cexp|$id:(assign_ $ typeId $ fromJust $ L.deref ty)|]

genLater :: Type -> C.Exp
genLater ty = [cexp|$id:(later_ $ typeId $ fromJust $ L.deref ty)|]

genArg :: Int -> (Binder, Type) -> (CIdent, C.Type)
genArg i = bimap (maybe (arg_ i) fromId) genType

genArgs :: [(Binder, Type)] -> [(CIdent, C.Type)]
genArgs = zipWith genArg [0 ..]

genLocal :: (VarId, Type) -> (CIdent, C.Type)
genLocal = bimap fromId genType

genLocals :: [(VarId, Type)] -> [(CIdent, C.Type)]
genLocals = map genLocal

genTrigs :: Int -> [(CIdent, C.Type)]
genTrigs trigs = zip (map trig_ [1 .. trigs]) (repeat trigger_t)

-- | The constant unit value, the singleton inhabitant of the type Unit.
unit :: C.Exp
unit = [cexp|0|]

-- | Undefined "value", the fake value used for expressions of type Void.
undef :: C.Exp
undef = [cexp|0xdeadbeef|]

{-------- Compilation --------}

-- | Include statements in the generated file
includes :: [C.Definition]
includes = [cunit|$esc:("#include \"ssm-platform.h\"")|]

-- | Setup the entry point of the program.
genInitProgram :: Program -> [C.Definition]
genInitProgram L.Program { L.programEntry = entry } = [cunit|
    int $id:initialize_program(void) {
       $id:activate($id:(enter_ entry)
        (&$id:top_parent, $id:root_priority, $id:root_depth));
      return 0;
    }
  |]

{- | Generate definitions for an SSM 'Procedure'. -}
genTop :: (L.VarId, Expr) -> Either CompileError [C.Definition]
genTop (name, l@(L.Lambda _ _ ty)) =
  runTR (fromId name) (zip argIds argTys) retTy body $ do
    (stepDecl , stepDefn ) <- genStep
    (enterDecl, enterDefn) <- genEnter
    structDefn             <- genStruct
    return [structDefn, enterDecl, stepDecl, enterDefn, stepDefn]
 where
  (argIds, body ) = L.collectLambda l
  (argTys, retTy) = L.dearrow ty
genTop (_, L.Lit _ _) = todo
genTop (_, _        ) = nope

{- | Generate struct definition for an SSM 'Procedure'.

This is where local variables, triggers, and parameter values are stored.
-}
genStruct :: TR C.Definition
genStruct = do
  name  <- gets fnName
  args  <- gets fnArgs
  locs  <- gets fnLocs
  trigs <- gets maxWaits

  return [cedecl|
    typedef struct {
      $ty:act_t $id:act_member;

      $sdecls:(map structField $ genArgs args)
      $sdecls:(map structField $ genLocals locs)
      $sdecls:(map structField $ genTrigs trigs)

    } $id:(act_ name);
  |]
 where
  structField :: (CIdent, C.Type) -> C.FieldGroup
  structField (n, t) = [csdecl|$ty:t $id:n;|]

{- | Generate the enter function for an SSM 'Procedure' and its signature.

Its struct is allocated and initialized (partially; local variables' values are
left uninitialized).
-}
genEnter :: TR (C.Definition, C.Definition)
genEnter = do
  actname <- gets fnName
  args    <- gets fnArgs
  trigs   <- gets maxWaits
  let act  = [cty|typename $id:act'|]
      act' = act_ actname -- hack to use this typename as expr in macros

      declParam (n, t) = [cparam|$ty:t $id:n|]
      initParam (n, _) = [[cstm|$id:acts->$id:n = $id:n;|]]
      initTrig (trigId, _) = [cstm|$id:acts->$id:trigId.act = $id:actg;|]

      params =
        [ [cparam|$ty:act_t *$id:caller|]
          , [cparam|$ty:priority_t $id:priority|]
          , [cparam|$ty:depth_t $id:depth|]
          ]
          ++ map declParam (genArgs args)
  return
    ( [cedecl|$ty:act_t *$id:(enter_ actname)($params:params);|]
    , [cedecl|
        $ty:act_t *$id:(enter_ actname)($params:params) {
          $ty:act_t *$id:actg = $id:enter(sizeof($ty:act),
                                          $id:(step_ actname),
                                          $id:caller,
                                          $id:priority,
                                          $id:depth);

          $ty:act *$id:acts = $id:container_of($id:actg, $id:act', $id:act_member);

          /* Assign parameters */
          $stms:(concatMap initParam $ genArgs args)

          /* Initialize triggers */
          $stms:(map initTrig $ genTrigs trigs)

          return $id:actg;
        }
      |]
    )

{- | Generate the step function for an SSM 'Procedure'.

This function just defines the function definition and switch statement that
wraps the statements of the procedure. The heavy lifting is performed by
'genExpr'.(L.Var n (L.TBuiltin (L.Arrow _ _))) =
-}
genStep :: TR (C.Definition, C.Definition)
genStep = do
  actName   <- gets fnName
  actBody   <- gets fnBody
  firstCase <- nextCase
  (_, stms) <- genExpr actBody -- Toss away return value
  let act  = [cty|typename $id:actt|]
      actt = act_ actName -- hack to use this typename as expr in macros

      -- | Dequeue any outstanding event on a reference
      -- TODO: move this responsibility to Drop
      -- dequeue :: (L.VarId, Type) -> C.BlockItem
      -- dequeue (n, _) = [citem|$id:unsched_event(&$id:acts->$id:n.sv);|]
  return
    ( [cedecl|void $id:(step_ actName)($ty:act_t *$id:actg);|]
    , [cedecl|
        void $id:(step_ actName)($ty:act_t *$id:actg) {
          $ty:act *$id:acts =
            $id:container_of($id:actg, $id:actt, $id:act_member);

          switch ($id:actg->$id:pc) {
          case $int:firstCase:;
            $items:stms

          default:
            break;
          }
        $id:leave_cleanup:;
          $id:leave($id:actg, sizeof($ty:act));
        }
      |]
    )

-- | Helper to generate yield point in step function.
genYield :: TR [C.BlockItem]
genYield = do
  next <- nextCase
  return [citems|
    $id:actg->$id:pc = $int:next;
    return;
    case $int:next:;
    |]

-- TODO: we currently don't do anything to differentiate between when
-- a Var is a top-level reference vs when it is a local variable. Here
-- we're just inferring it from how it's used, i.e., on the LHS of an app vs on
-- its own.
genExpr :: Expr -> TR (C.Exp, [C.BlockItem])
genExpr (L.Var n (L.TBuiltin (L.Arrow _ _))) =
  -- Any identifiers of L.Arrow type must refer to a (global) function, so we
  -- just return a handle to its enter function.
  return ([cexp|$id:(enter_ n)|], [])
genExpr (L.Var n _) = do
  isLocal <- isLocalVar n
  if isLocal then return ([cexp|$id:acts->$id:n|], []) else todo
genExpr (L.Data _ _             ) = nope
genExpr (L.Lit  l t             ) = genLiteral l t
genExpr (L.Let [(Just n, d)] b _) = do
  addLocal (n, L.typeExpr d)
  (defVal, defStms) <- genExpr d
  let defInit = [citems| $id:acts->$id:n = $exp:defVal; |]
  (bodyVal, bodyStms) <- genExpr b
  return (bodyVal, defStms ++ defInit ++ bodyStms)
genExpr (L.Let [(Nothing, d)] b _) = do
  (_      , defStms ) <- genExpr d -- Throw away value
  (bodyVal, bodyStms) <- genExpr b
  return (bodyVal, defStms ++ bodyStms)
genExpr L.Let{}          = error "Cannot handle mutually recursive bindings"
genExpr a@(L.App _ _ ty) = do
  let (fn, args) = L.collectApp a
  (fnEnter : argVals, evalStms) <- unzip <$> mapM genExpr (fn : args)
  tmpName                       <- nextTmp ty
  yield                         <- genYield
  let tmp = [cexp|$id:acts->$id:tmpName|]
      enterArgs =
        [[cexp|$id:actg|], [cexp|$id:actg->$id:priority|]]
          ++ argVals
          ++ [[cexp|&$exp:tmp|]]
      call = [citems|$id:activate($exp:fnEnter($args:enterArgs));|]
  return (tmp, concat evalStms ++ call ++ yield)
genExpr L.Match{}       = nope
genExpr L.Lambda{}      = error "Cannot handle lambdas"
genExpr (L.Prim p es t) = genPrim p es t

genPrim :: Primitive -> [Expr] -> Type -> TR (C.Exp, [C.BlockItem])
genPrim L.New [e, _] refType = do
  (val, stms) <- genExpr e
  tmp         <- nextTmp refType
  let alloc = [citems|
          $id:acts->$id:tmp = malloc(sizeof(*$id:acts->$id:tmp));
          $exp:(genInit refType)($id:acts->$id:tmp);
          *$id:acts->$id:tmp.$id:value = $exp:val;
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
  let assignBlock = [citems|
          $items:lhsStms
          $items:rhsStms
          $exp:(genAssign $ L.typeExpr lhs)
            ($exp:lhsVal, $id:actg->$id:priority, $exp:rhsVal);
        |]
  return (unit, assignBlock)
genPrim L.After [time, lhs, rhs] _ = do
  (timeVal, timeStms) <- genExpr time
  (lhsVal , lhsStms ) <- genExpr lhs
  (rhsVal , rhsStms ) <- genExpr rhs
  -- TODO: handle event type
  let laterBlock = [citems|
          $items:timeStms
          $items:lhsStms
          $items:rhsStms
          $exp:(genLater $ L.typeExpr lhs)
            ($exp:lhsVal, $exp:rhsVal, $id:now() + $exp:timeVal);
        |]
  return (unit, laterBlock)
genPrim L.Fork procs _ = do
  yield <- genYield
  let
    numChildren = length procs
    forkArgs    = genForkArgs
      numChildren
      ([cexp|$id:actg->$id:priority|], [cexp|$id:actg->$id:depth|])
    checkNewDepth = [citems|
                        if ($id:actg->$id:depth < $exp:(depthSub numChildren))
                          $id:throw($id:exhausted_priority);
                      |]

    genActivate
      :: ((C.Exp, C.Exp), Expr) -> TR (C.Exp, [C.BlockItem], C.BlockItem)
    genActivate ((prioArg, depthArg), a@(L.App _ _ ty)) = do
      let (fn, args) = L.collectApp a
      (fnEnter : argVals, evalStms) <- unzip <$> mapM genExpr (fn : args)
      tmpName                       <- nextTmp ty
      let tmp = [cexp|$id:acts->$id:tmpName|]
          enterArgs =
            [[cexp|$id:actg|], prioArg, depthArg]
              ++ argVals
              ++ [[cexp|&$exp:tmp|]]
      return
        ( tmp
        , concat evalStms
        , [citem|$id:activate($exp:fnEnter($args:enterArgs));|]
        )
    -- For now, we only support forking expressions that have a top-level
    -- application (i.e., no thunks), whose left operand is a var.
    genActivate _ = nope

  (_rets, evals, activates) <- unzip3 <$> mapM genActivate (zip forkArgs procs)
  return (todo, checkNewDepth ++ concat evals ++ activates ++ yield)
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
  -- Assign to return argument and jump to leave
  let retBlock = [citems|
                    $id:acts->$id:ret_val = $exp:val;
                    goto $id:leave_cleanup;
                 |]
  return (undef, stms ++ retBlock)
genPrim (L.PrimOp op) es t = genPrimOp op es t
genPrim _ _ _ = error "Unsupported Primitive or wrong number of arguments"

-- | Generate C value for SSM literal.
genLiteral :: Literal -> Type -> TR (C.Exp, [C.BlockItem])
genLiteral (L.LitIntegral i    ) _ = return ([cexp|$int:i|], [])
genLiteral (L.LitBool     True ) _ = return ([cexp|true|], [])
genLiteral (L.LitBool     False) _ = return ([cexp|false|], [])

-- | Generate C expression for SSM primitive operation.
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

-- | Compute priority and depth arguments for a fork of width 'numChildren'.
genForkArgs :: Int -> (C.Exp, C.Exp) -> [(C.Exp, C.Exp)]
genForkArgs numChildren (currentPrio, currentDepth) =
  [ let p = [cexp|$exp:currentPrio + ($int:(i-1) * (1 << $exp:d))|]
        d = [cexp|$exp:currentDepth - $exp:(depthSub numChildren)|]
    in  (p, d)
  | i <- [1 .. numChildren]
  ]

-- | How much the depth should be decreased when forking 'numChildren'.
depthSub :: Int -> C.Exp
depthSub numChildren = [cexp|$int:ds|]
  where ds = ceiling $ logBase (2 :: Double) $ fromIntegral numChildren :: Int
