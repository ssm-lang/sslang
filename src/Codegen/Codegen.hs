{- | Translate SSM program to C compilation unit.

What is expected of the IR:

- Well-formed: All primitive functions are applied to the right number of
  arguments.

- Fully-applied: All applications are fully-applied; the only time a term of
  type a -> b appears anywhere is on the left-hand side of an application.

- Defunctionalized: No lambdas/closures; the only kinds of terms with an arrow
  type are variables.

- Name mangled: All variable identifiers are unique.

-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
module Codegen.Codegen where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( fromId )

import qualified IR.IR                         as I
import qualified IR.Types.Flat                 as I
import qualified IR.Types.TypeSystem           as I

import           Codegen.Identifiers
import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C


import           Control.Comonad                ( Comonad(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Maybe                     ( isJust )

-- | Possible, but temporarily punted for the sake of expediency.
todo :: a
todo = error "Not yet implemented"

-- | Impossible without a discussion about implementation strategy.
nope :: a
nope = error "Not yet supported"

{- Some convenience type aliases -}
type Type = I.Type
type Program = I.Program Type
type Expr = I.Expr Type
type PrimOp = I.PrimOp
type Primitive = I.Primitive
type Literal = I.Literal
type TypeDef = I.TypeDef Type
type VarId = I.VarId
type Binder = I.Binder

{- | State maintained while compiling a top-level SSM function.

The information here is populated while generating the step function, so that
should be computed first, before this information is used to generate the act
struct and enter definitions.
-}
data GenFnState = GenFnState
  { fnName     :: VarId             -- ^ Function name
  , fnParams   :: [(Binder, Type)]  -- ^ Function parameters
  , fnRetTy    :: Type              -- ^ Function return type
  , fnBody     :: Expr              -- ^ Function body
  , fnLocs     :: [(VarId, Type)]   -- ^ Function local variables
  , fnMaxWaits :: Int               -- ^ Number of triggers needed for waiting
  , fnCases    :: Int               -- ^ Yield point counter
  , fnTmps     :: Int               -- ^ Temporary variable name counter
  }

{- | Translation monad for procedures, with derived typeclass instances.

We declare 'GenFn' as a newtype so that we can implement 'MonadFail' for it,
allowing us to use monadic pattern matching.
-}
newtype GenFn a = GenFn (StateT GenFnState Compiler.Pass a)
  deriving Functor                      via (StateT GenFnState Compiler.Pass)
  deriving Applicative                  via (StateT GenFnState Compiler.Pass)
  deriving Monad                        via (StateT GenFnState Compiler.Pass)
  deriving MonadFail                    via (StateT GenFnState Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT GenFnState Compiler.Pass)
  deriving (MonadState GenFnState)      via (StateT GenFnState Compiler.Pass)

-- | Run a GenFn computation on a procedure.
runGenFn
  :: VarId                  -- ^ Name of procedure
  -> [(Binder, Type)]       -- ^ Names and types of parameters to procedure
  -> Type                   -- ^ Name and type of return parameter of procedure
  -> Expr                   -- ^ Body of procedure
  -> GenFn a                -- ^ Translation monad to run
  -> Compiler.Pass a        -- ^ Pass on errors to caller
runGenFn name params ret body (GenFn tra) = evalStateT tra $ GenFnState
  { fnName     = name
  , fnParams   = params
  , fnRetTy    = ret
  , fnBody     = body
  , fnLocs     = []
  , fnMaxWaits = 0
  , fnCases    = 0
  , fnTmps     = 0
  }

-- | Read and increment the number of cases in a procedure, i.e., fnCases++.
nextCase :: GenFn Int
nextCase = do
  n <- gets fnCases
  modify $ \st -> st { fnCases = n + 1 }
  return n

-- | Register a local variable, to be declared in activation record.
addLocal :: (VarId, Type) -> GenFn ()
addLocal l = modify $ \st -> st { fnLocs = l : fnLocs st }

-- | Register number of wait statements track of number of triggers needed.
maxWait :: Int -> GenFn ()
maxWait n = modify $ \st -> st { fnMaxWaits = n `max` fnMaxWaits st }

-- | Allocate a temp variable of given 'Type', registered as a local variable.
nextTmp :: Type -> GenFn VarId
nextTmp ty = do
  t <- fromId . tmp_ <$> gets fnTmps
  modify $ \st -> st { fnTmps = fnTmps st + 1 }
  addLocal (t, ty)
  return t

{- | Determines whether the given 'VarId' can be found in the activation record.

Returns true iff 'name' appears as local variable or as a parameter.
-}
isLocalVar :: VarId -> GenFn Bool
isLocalVar name = do
  params <- gets fnParams
  locs   <- gets fnLocs
  return $ isJust (lookup (Just name) params) || isJust (lookup name locs)

{-------- Type compilation --------}

-- | Wrap a 'C.Type' with pointers, according to some 'I.Type'.
ptrs_ :: I.Type -> C.Type -> C.Type
ptrs_ (I.TBuiltin (I.Ref _)) t = [cty|$ty:t *|]
ptrs_ _                      t = t
-- ^ TODO: this does not handle stacked pointers

-- | Obtains the C type name corresponding to an SSM type.
typeId :: Type -> CIdent
typeId (I.TCon     t ) = fromId t
typeId (I.TBuiltin bt) = builtinId bt

-- | Obtains the C type name corresponding to an SSM built-in type.
builtinId :: I.Builtin Type -> CIdent
builtinId I.Unit         = "unit"
builtinId I.Void         = todo
builtinId (I.Arrow _ _ ) = todo
builtinId (I.Tuple    _) = todo
builtinId (I.Integral s) = int_ s
builtinId (I.Ref      t) = sv_ $ typeId t -- NOTE: this does not add pointers

-- | Translate an SSM 'Type' to a 'C.Type'.
genType :: Type -> C.Type
genType typ = ptrs_ typ $ ctype $ typeId typ

-- | Obtain the initialize method for a given SSM scheduled variable 'Type'.
genInit :: Type -> Maybe C.Exp
genInit ty = cexpr . initialize_ . typeId <$> I.deref ty

-- | Obtain the assign method for a given SSM scheduled variable 'Type'.
genAssign :: Type -> Maybe C.Exp
genAssign ty = cexpr . assign_ . typeId <$> I.deref ty

-- | Obtain the later method for a given SSM scheduled variable 'Type'.
genLater :: Type -> Maybe C.Exp
genLater ty = cexpr . later_ . typeId <$> I.deref ty

-- | Translate a list of SSM parameters to C parameters.
genParams :: [(Binder, Type)] -> [(CIdent, C.Type)]
genParams = zipWith genArg [0 ..]
  where genArg i = bimap (maybe (arg_ i) fromId) genType

-- | Translate a list of SSM local declarations to C declarations.
genLocals :: [(VarId, Type)] -> [(CIdent, C.Type)]
genLocals = map $ bimap fromId genType

-- | Generate declarations for @numTrigs@ triggers.
genTrigs :: Int -> [(CIdent, C.Type)]
genTrigs numTrigs = zip (map trig_ [1 .. numTrigs]) (repeat trigger_t)

-- | The constant unit value, the singleton inhabitant of the type Unit.
unit :: C.Exp
unit = [cexp|0|]

-- | Undefined "value", the fake value used for expressions of type Void.
undef :: C.Exp
undef = [cexp|0xdeadbeef|]

{-------- Compilation --------}

-- | Generate a C compilation from an SSM program.
genProgram :: Program -> Compiler.Pass [C.Definition]
genProgram I.Program { I.programDefs = defs } = do -- p@I.Program
  (cdecls, cdefs) <- bimap concat concat . unzip <$> mapM genTop defs
  return $ includes ++ cdecls ++ cdefs  -- ++ genInitProgram p

-- | Include statements in the generated C file.
includes :: [C.Definition]
includes = [cunit|
$esc:("#include \"ssm.h\"")
$esc:("#define ssm_initialize_unit(v) ssm_initialize_event(v)")
$esc:("#define ssm_later_unit(l, d, r) ssm_later_event(l, d)")
$esc:("#define ssm_assign_unit(l, p, r) ssm_assign_event(l, p)")
typedef typename ssm_event_t ssm_unit_t;
typedef char unit;
|]

-- | Setup the entry point of the program.
genInitProgram :: Program -> [C.Definition]
genInitProgram I.Program { I.programEntry = entry } = [cunit|
    int $id:initialize_program(void) {
       $id:activate($id:(enter_ entry)(&$id:top_parent, $id:root_priority, $id:root_depth));
      return 0;
    }
  |]

{- | Generate C declarations and definitions for a top-level SSM function.

Each top-level function in a program is turned into three components:

(1) a struct (the activation record);
(2) an initialization function (the enter function); and
(3) a step function, which corresponds to the actual procedure body.

Items (2) and (3) include both declarations and definitions.
-}
genTop :: (VarId, Expr) -> Compiler.Pass ([C.Definition], [C.Definition])
genTop (name, l@(I.Lambda _ _ ty)) =
  runGenFn (fromId name) (zip argIds argTys) retTy body $ do
    (stepDecl , stepDefn ) <- genStep
    (enterDecl, enterDefn) <- genEnter
    structDefn             <- genStruct
    return ([structDefn, enterDecl, stepDecl], [enterDefn, stepDefn])
 where
  (argIds, body ) = I.collectLambda l
  (argTys, retTy) = I.collectArrow ty
genTop (_, I.Lit _ _) = todo
genTop (_, _        ) = nope

{- | Generate struct definition for an SSM 'Procedure'.

This is where local variables, triggers, and parameter values are stored.
-}
genStruct :: GenFn C.Definition
genStruct = do
  name   <- gets fnName
  params <- gets fnParams
  retTy  <- gets fnRetTy
  locs   <- gets fnLocs
  trigs  <- gets fnMaxWaits

  return [cedecl|
    typedef struct {
      $ty:act_t $id:act_member;

      $sdecls:(map structField $ genParams params)
      $ty:(genType retTy) *$id:ret_val;
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
genEnter :: GenFn (C.Definition, C.Definition)
genEnter = do
  actname <- gets fnName
  params  <- gets fnParams
  retTy   <- gets fnRetTy
  trigs   <- gets fnMaxWaits
  let act  = [cty|typename $id:act'|]
      act' = act_ actname -- hack to use this typename as expr in macros

      declParam (n, t) = [cparam|$ty:t $id:n|]
      initParam (n, _) = [[cstm|$id:acts->$id:n = $id:n;|]]
      initTrig (trigId, _) = [cstm|$id:acts->$id:trigId.act = $id:actg;|]

      enterParams =
        [ [cparam|$ty:act_t *$id:caller|]
          , [cparam|$ty:priority_t $id:priority|]
          , [cparam|$ty:depth_t $id:depth|]
          ]
          ++ map declParam (genParams params)
          ++ [[cparam|$ty:(genType retTy) *$id:ret_val|]]
  return
    ( [cedecl|$ty:act_t *$id:(enter_ actname)($params:enterParams);|]
    , [cedecl|
        $ty:act_t *$id:(enter_ actname)($params:enterParams) {
          $ty:act_t *$id:actg = $id:enter(sizeof($ty:act),
                                          $id:(step_ actname),
                                          $id:caller,
                                          $id:priority,
                                          $id:depth);

          $ty:act *$id:acts = $id:container_of($id:actg, $id:act', $id:act_member);

          /* Assign parameters */
          $stms:(concatMap initParam $ genParams params)

          /* Set return value */
          $id:acts->$id:ret_val = $id:ret_val;

          /* Initialize triggers */
          $stms:(map initTrig $ genTrigs trigs)

          return $id:actg;
        }
      |]
    )

{- | Generate the step function for an SSM 'Procedure'.

This function just defines the function definition and switch statement that
wraps the statements of the procedure. The heavy lifting is performed by
'genExpr'.(I.Var n (I.TBuiltin (I.Arrow _ _))) =
-}
genStep :: GenFn (C.Definition, C.Definition)
genStep = do
  actName   <- gets fnName
  actBody   <- gets fnBody
  firstCase <- nextCase
  (_, stms) <- genExpr actBody -- Toss away return value
  let act  = [cty|typename $id:actt|]
      actt = act_ actName -- hack to use this typename as expr in macros
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
genYield :: GenFn [C.BlockItem]
genYield = do
  next <- nextCase
  return [citems|
    $id:actg->$id:pc = $int:next;
    return;
    case $int:next:;
    |]

{- | Translate an SSM expression into a C expression and statements.

SSM IR is a side-effectful expression language, with two implications when
translating to C:

(1) every expression has a value (even if it is an uninhabited type), so this
    must be reflected in C; and
(2) some of the side effects in SSM cannot be implemented in C using expressions
    alone.

These two implications roughly translate to the @C.Exp@ and @[C.BlockItem]@ in
@genExpr@'s return type. When we translate an SSM expression @e@:

  (val, stms) <- genExpr e

@val@ represents the C expression that corresponds to the value of @e@ upon
evaluation, while @stms@ represents the list of preceding statements that
compute @val@.

A further consideration upon point (2) is that SSM expressions may yield control
at any point. Thus, the C expression returned by @genExpr@ must accommodate the
step function suspending and resuming. For instance, consider the following SSM
IR expression:

  (let x = 3 in x) + (wait r; 6)

The @x@ in the let-binding in the left operand cannot just be a local variable
in the step function, because it would be "uninitialized" by the yield in the
right operand:

    // let x = 3 in x
    // stms:
    int x = 3;
    // exp: x

    // (wait r; 6)
    // stms:
    ssm_sensitize(r);
    actg->pc = N;
    return;
  case N:
    ssm_desensitize(r);
    // exp: 6

    // After the return, x is no longer initialized, so the following is
    // undefined behavior:
    x + 6

To ensure this is cannot happen, we conservatively declare @x@ as a local
variable in the activation record, so that its value is preserved between
yields, even if this is not usually necessary. We leave it to later compiler
passes to optimize this.
-}
genExpr :: Expr -> GenFn (C.Exp, [C.BlockItem])
genExpr (I.Var n (I.TBuiltin (I.Arrow _ _))) =
  -- NOTE: Any identifiers of I.Arrow type must refer to a (global) function, so
  -- we just return a handle to its enter function.
  return ([cexp|$id:(enter_ n)|], [])
genExpr (I.Var n _) = do
  -- isLocal <- isLocalVar n -- TODO: check for global vs local variable
  return ([cexp|$id:acts->$id:n|], [])
genExpr (I.Data _ _             ) = nope
genExpr (I.Lit  l t             ) = genLiteral l t
genExpr (I.Let [(Just n, d)] b _) = do
  addLocal (n, extract d)
  (defVal, defStms) <- genExpr d
  let defInit = [citems| $id:acts->$id:n = $exp:defVal; |]
  (bodyVal, bodyStms) <- genExpr b
  return (bodyVal, defStms ++ defInit ++ bodyStms)
genExpr (I.Let [(Nothing, d)] b _) = do
  (_      , defStms ) <- genExpr d -- Throw away value
  (bodyVal, bodyStms) <- genExpr b
  return (bodyVal, defStms ++ bodyStms)
genExpr I.Let{}          = fail "Cannot handle mutually recursive bindings"
genExpr a@(I.App _ _ ty) = do
  let (fn, args) = I.collectApp a
  (fnEnter : argVals, evalStms) <- unzip <$> mapM genExpr (fn : args)
  tmpName                       <- nextTmp ty
  yield                         <- genYield
  let tmp = [cexp|$id:acts->$id:tmpName|]
      enterArgs =
        [ [cexp|$id:actg|]
          , [cexp|$id:actg->$id:priority|]
          , [cexp|$id:actg->$id:depth|]
          ]
          ++ argVals
          ++ [[cexp|&$exp:tmp|]]
      call = [citems|$id:activate($exp:fnEnter($args:enterArgs));|]
  return (tmp, concat evalStms ++ call ++ yield)
genExpr I.Match{}       = nope
genExpr I.Lambda{}      = fail "Cannot handle lambdas"
genExpr (I.Prim p es t) = genPrim p es t

-- | Generate code for SSM primitive; see 'genExpr' for extended discussion.
genPrim :: Primitive -> [Expr] -> Type -> GenFn (C.Exp, [C.BlockItem])
genPrim I.New [e] refType = do -- TODO: New with perceus should take two args
  (val, stms)     <- genExpr e
  tmp             <- nextTmp refType
  Just initialize <- return $ genInit refType
  -- TODO: reference counting
  let alloc =
        [citems|
          $id:acts->$id:tmp = $id:mem_alloc(sizeof(*$id:acts->$id:tmp));
          $exp:initialize($id:acts->$id:tmp);
        |]
          ++ if refType == I.ref I.unit
               then []
               else [citems|*$id:acts->$id:tmp.$id:value = $exp:val;|]
  return ([cexp|$id:acts->$id:tmp|], stms ++ alloc)
genPrim I.Dup [e] _ = do
  -- TODO: reference counting
  (_val, _stms) <- genExpr e
  todo
genPrim I.Drop [e] _ = do
  (val, stms) <- genExpr e
  -- TODO: reference counting
  let free = [citems|
          $id:unsched_event(&$exp:val->$id:sv);
          $id:mem_free($exp:val);
        |]
  return (unit, stms ++ free)
genPrim I.Reuse [e] _ = do
  -- TODO: reference counting
  (_val, _stms) <- genExpr e
  todo
genPrim I.Deref [a] ty = do
  (val, stms) <- genExpr a
  -- SSM Refs are all SVs, so we access the value field in order to dereference.
  -- For event types, there is no value, so we just return the 'unit' value.
  return (if ty == I.unit then unit else [cexp|$exp:val->$id:value|], stms)

genPrim I.Assign [lhs, rhs] _ = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  Just assign       <- return $ genAssign $ extract lhs
  let assignBlock = [citems|
          $items:lhsStms
          $items:rhsStms
          $exp:assign($exp:lhsVal, $id:actg->$id:priority, $exp:rhsVal);
        |]
  return (unit, assignBlock)
genPrim I.After [time, lhs, rhs] _ = do
  (timeVal, timeStms) <- first unmarshal <$> genExpr time
  (lhsVal , lhsStms ) <- genExpr lhs
  (rhsVal , rhsStms ) <- genExpr rhs
  Just later          <- return $ genLater $ extract lhs
  let laterBlock = [citems|
          $items:timeStms
          $items:lhsStms
          $items:rhsStms
          $exp:later($exp:lhsVal, $id:now() + $exp:timeVal, $exp:rhsVal);
        |]
  return (unit, laterBlock)
genPrim I.Par procs _ = do
  yield <- genYield
  let
    numChildren = length procs
    parArgs     = genParArgs
      numChildren
      ([cexp|$id:actg->$id:priority|], [cexp|$id:actg->$id:depth|])
    checkNewDepth = [citems|
                        if ($id:actg->$id:depth < $exp:(depthSub numChildren))
                          $id:throw($id:exhausted_priority);
                      |]

    genActivate
      :: ((C.Exp, C.Exp), Expr) -> GenFn (C.Exp, [C.BlockItem], C.BlockItem)
    genActivate ((prioArg, depthArg), a@(I.App _ _ ty)) = do
      let (fn, args) = I.collectApp a
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

  (_rets, evals, activates) <- unzip3 <$> mapM genActivate (zip parArgs procs)
  return (todo, checkNewDepth ++ concat evals ++ activates ++ yield)
genPrim I.Wait vars _ = do
  (varVals, varStms) <- unzip <$> mapM genExpr vars
  maxWait $ length varVals
  yield <- genYield
  let trigs = zip [1 :: Int ..] varVals
      sens (i, var) =
        [citem|$id:sensitize(&$exp:var->$id:sv, &$id:acts->$id:(trig_ i));|]
      desens (i, _) = [citem|$id:desensitize(&$id:acts->$id:(trig_ i));|]
  return (unit, concat varStms ++ map sens trigs ++ yield ++ map desens trigs)
genPrim I.Loop [b] _ = do
  (_, bodyStms) <- genExpr b
  return (unit, [citems|for (;;) { $items:bodyStms }|])
genPrim I.Break  []  _ = return (undef, [citems|break;|])
genPrim I.Return [e] _ = do
  (val, stms) <- genExpr e
  -- Assign to return argument and jump to leave
  let retBlock = [citems|
                    *$id:acts->$id:ret_val = $exp:val;
                    goto $id:leave_cleanup;
                 |]
  return (undef, stms ++ retBlock)
genPrim (I.PrimOp op) es t = genPrimOp op es t
genPrim _ _ _ = fail "Unsupported Primitive or wrong number of arguments"



-- | Generate C value for SSM literal.
genLiteral :: Literal -> Type -> GenFn (C.Exp, [C.BlockItem])
genLiteral (I.LitIntegral i    ) _ = return (marshal [cexp|$int:i|], [])
genLiteral (I.LitBool     True ) _ = return (marshal [cexp|true|], [])
genLiteral (I.LitBool     False) _ = return (marshal [cexp|false|], [])
genLiteral I.LitEvent            _ = return ([cexp|1|], [])

-- | Generate C expression for SSM primitive operation.
genPrimOp :: PrimOp -> [Expr] -> Type -> GenFn (C.Exp, [C.BlockItem])
genPrimOp I.PrimAdd [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  -- all integers are 31 bits + 1 tag bit, so zero tag bit on one argument,
  -- add together, and the result will be sum with a tag bit of 1.
  return ([cexp|((((typename uint32_t) $exp:rhsVal) & 0xFFFFFFFE) + ((typename uint32_t) $exp:lhsVal))|], stms)
genPrimOp I.PrimSub [lhs, rhs] _ = do
  -- all integers are 31 bits + 1 tag bit, so zero tag bit on subtrahend,
  -- subtract, and the result will be difference with a tag bit of 1.                   
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|(((typename uint32_t) $exp:lhsVal) - (((typename uint32_t) $exp:rhsVal) & 0xFFFFFFFE))|], stms)
genPrimOp I.PrimMul [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal * $exp:rhsVal|], stms)
genPrimOp I.PrimDiv [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal / $exp:rhsVal|], stms)
genPrimOp I.PrimMod [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal % $exp:rhsVal|], stms)
genPrimOp I.PrimNeg [opr] _ = do
  (val, stms) <- first unmarshal <$> genExpr opr
  return (marshal [cexp|- $exp:val|], stms)
genPrimOp I.PrimBitAnd [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal & $exp:rhsVal|], stms)
genPrimOp I.PrimBitOr [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal | $exp:rhsVal|], stms)
genPrimOp I.PrimBitNot [opr] _ = do
  (val, stms) <- genExpr opr
  -- all integers are 31 bits + 1 tag bit, so val XOR (~1)
  -- flips the 31 bits and keeps the tag bit 1. 
  return ([cexp|($exp:val ^ 0xFFFFFFFE)|], stms)
genPrimOp I.PrimEq [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal == $exp:rhsVal|], stms)
genPrimOp I.PrimNot [opr] _ = do
  (val, stms) <- first unmarshal <$> genExpr opr
  return (marshal [cexp|! $exp:val|], stms)
genPrimOp I.PrimGt [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal < $exp:rhsVal|], stms)
genPrimOp I.PrimGe [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal <= $exp:rhsVal|], stms)
genPrimOp I.PrimLt [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal > $exp:rhsVal|], stms)
genPrimOp I.PrimLe [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- genBinop lhs rhs
  return ([cexp|$exp:lhsVal >= $exp:rhsVal|], stms)
genPrimOp _ _ _ = fail "Unsupported PrimOp or wrong number of arguments"

-- | Helper for sequencing across binary operations.
genBinop :: Expr -> Expr -> GenFn ((C.Exp, C.Exp), [C.BlockItem])
genBinop lhs rhs = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  return ((lhsVal, rhsVal), lhsStms ++ rhsStms)

-- | Marshal a C expression evaluating to a 32 bit int
marshal :: C.Exp -> C.Exp
marshal e = [cexp|((((typename uint32_t) $exp:e) << 1) | 0x1)|]

-- | Unmarshal a C expression evaluating to a SSLANG 31 bit 
unmarshal :: C.Exp -> C.Exp
unmarshal e = [cexp|(((typename uint32_t) $exp:e) >> 1)|]

-- | Compute priority and depth arguments for a par fork of width 'numChildren'.
genParArgs :: Int -> (C.Exp, C.Exp) -> [(C.Exp, C.Exp)]
genParArgs numChildren (currentPrio, currentDepth) =

  [ let p = [cexp|$exp:currentPrio + ($int:(i-1) * (1 << $exp:d))|]
        d = [cexp|$exp:currentDepth - $exp:(depthSub numChildren)|]
    in  (p, d)
  | i <- [1 .. numChildren]
  ]

-- | How much the depth should be decreased when par forking 'numChildren'.
depthSub :: Int -> C.Exp
depthSub numChildren = [cexp|$int:ds|]
  where ds = ceiling $ logBase (2 :: Double) $ fromIntegral numChildren :: Int
