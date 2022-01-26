{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Translate SSM program to C compilation unit.
--
-- What is expected of the IR:
--
-- - Well-formed: All primitive functions are applied to the right number of
--  arguments.
--
-- - Fully-applied: All applications are fully-applied; the only time a term of
--  type @a -> b@ appears anywhere is on the left-hand side of an application.
--
-- - Defunctionalized: No lambdas/closures; the only kinds of terms with an arrow
--  type are variables.
--
-- - Name mangled: All variable identifiers are unique.
module Codegen.Codegen where

import           Codegen.LibSSM
import           Codegen.TypeDef                ( TypeDefInfo
                                                , dconType
                                                , genTypeDef
                                                , intInit
                                                , isPointer
                                                ,
    -- , tag
                                                  typeSize
                                                )

import qualified IR.IR                         as I
import qualified IR.Types.Flat                 as I
import qualified IR.Types.TypeSystem           as I

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( fromId
                                                , ident
                                                )
import           Control.Comonad                ( Comonad(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import           Prelude                 hiding ( drop )

-- | Possible, but temporarily punted for the sake of expediency.
todo :: a
todo = error "Not yet implemented"

-- | Impossible without a discussion about implementation strategy.
nope :: a
nope = error "Not yet supported"

{- | State maintained while compiling a top-level SSM function.

The information here is populated while generating the step function, so that
should be computed first, before this information is used to generate the act
struct and enter definitions.
-}
data GenFnState = GenFnState
  { fnName     :: I.VarId               -- ^ Function name
  , fnParams   :: [(I.Binder, I.Type)]  -- ^ Function parameters
  , fnRetTy    :: I.Type                -- ^ Function return type
  , fnBody     :: I.Expr I.Type         -- ^ Function body
  , fnLocs     :: [(I.VarId, I.Type)]   -- ^ Function local variables
  , fnMaxWaits :: Int                   -- ^ Number of triggers needed
  , fnCases    :: Int                   -- ^ Yield point counter
  , fnTmps     :: Int                   -- ^ Temporary variable name counter
  , adtInfo    :: TypeDefInfo           -- ^ ADT information
  }

{- | Translation monad for procedures, with derived typeclass instances.

We declare 'GenFn' as a newtype so that we can implement 'MonadFail' for it,
allowing us to use monadic pattern matching.
-}
newtype GenFn a = GenFn (StateT GenFnState Compiler.Pass a)
  deriving (Functor) via (StateT GenFnState Compiler.Pass)
  deriving (Applicative) via (StateT GenFnState Compiler.Pass)
  deriving (Monad) via (StateT GenFnState Compiler.Pass)
  deriving (MonadFail) via (StateT GenFnState Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT GenFnState Compiler.Pass)
  deriving (MonadState GenFnState) via (StateT GenFnState Compiler.Pass)

-- | Run a 'GenFn' computation on a procedure.
runGenFn
  :: I.VarId              -- ^ Name of procedure
  -> [(I.Binder, I.Type)] -- ^ Names and types of parameters to procedure
  -> I.Type               -- ^ Name and type of return parameter of procedure
  -> I.Expr I.Type        -- ^ Body of procedure
  -> TypeDefInfo          -- ^ ADT information
  -> GenFn a              -- ^ Translation monad to run
  -> Compiler.Pass a      -- ^ Pass on errors to caller
runGenFn name params ret body adtinfo (GenFn tra) = evalStateT tra $ GenFnState
  { fnName     = name
  , fnParams   = params
  , fnRetTy    = ret
  , fnBody     = body
  , fnLocs     = []
  , adtInfo    = adtinfo
  , fnMaxWaits = 0
  , fnCases    = 0
  , fnTmps     = 0
  }

-- | Read and increment the number of cases in a procedure, i.e., @fnCases++@.
nextCase :: GenFn Int
nextCase = do
  n <- gets fnCases
  modify $ \st -> st { fnCases = n + 1 }
  return n

-- | Register a local variable, to be declared in activation record.
addLocal :: (I.VarId, I.Type) -> GenFn ()
addLocal l = modify $ \st -> st { fnLocs = l : fnLocs st }

-- | Register number of wait statements track of number of triggers needed.
maxWait :: Int -> GenFn ()
maxWait n = modify $ \st -> st { fnMaxWaits = n `max` fnMaxWaits st }

-- | Allocate a temp variable of given type, registered as a local variable.
nextTmp :: I.Type -> GenFn I.VarId
nextTmp ty = do
  t <- fromId . tmp_ <$> gets fnTmps
  modify $ \st -> st { fnTmps = fnTmps st + 1 }
  addLocal (t, ty)
  return t

-- | Generate local variable in activation record for temporary storage.
genTmp :: I.Type -> GenFn C.Exp
genTmp ty = acts_ . fromId <$> nextTmp ty

{- | Whether the given variable can be found in the activation record.

Returns true iff it appears as local variable or as a parameter.
-}
isLocalVar :: I.VarId -> GenFn Bool
isLocalVar name = do
  params <- gets fnParams
  locs   <- gets fnLocs
  return $ isJust (lookup (Just name) params) || isJust (lookup name locs)

{-------- Type compilation --------}
--
-- -- | Wrap a 'C.Type' with pointers, according to some 'I.Type'.
-- ptrs_ :: I.Type -> C.Type -> C.Type
-- ptrs_ (I.TBuiltin (I.Ref _)) t = [cty|$ty:t *|]
-- ptrs_ _ t = t
-- -- ^ TODO: this does not handle stacked pointers
--
-- -- | Obtains the C type name corresponding to an SSM type.
-- typeId :: I.Type -> CIdent
-- typeId (I.TCon t) = fromId t
-- typeId (I.TBuiltin bt) = builtinId bt
--
-- -- | Obtains the C type name corresponding to an SSM built-in type.
-- builtinId :: I.Builtin I.Type -> CIdent
-- builtinId I.Unit = "unit"
-- builtinId I.Void = todo
-- builtinId (I.Arrow _ _) = todo
-- builtinId (I.Tuple _) = todo
-- builtinId (I.Integral s) = int_ s
-- builtinId (I.Ref t) = sv_ $ typeId t -- NOTE: this does not add pointers
--
-- -- | Translate an SSM type to a 'C.Type'.
-- genType :: I.Type -> C.Type
-- genType typ = ptrs_ typ $ ctype $ typeId typ
--
-- -- | Obtain the initialize method for a given SSM scheduled variable type.
-- genInit :: I.Type -> Maybe C.Exp
-- genInit ty = cexpr . initialize_ . typeId <$> I.deref ty
--
-- -- | Obtain the assign method for a given SSM scheduled variable type.
-- genAssign :: I.Type -> Maybe C.Exp
-- genAssign ty = cexpr . assign_ . typeId <$> I.deref ty
--
-- -- | Obtain the later method for a given SSM scheduled variable type.
-- genLater :: I.Type -> Maybe C.Exp
-- genLater ty = cexpr . later_ . typeId <$> I.deref ty

-- | Translate a list of SSM parameters to C parameters.
genParams :: [(I.Binder, I.Type)] -> [(CIdent, C.Type)]
genParams = zipWith genArg [0 ..]
  where genArg i = bimap (maybe (arg_ i) fromId) (const value_t)

-- | Translate a list of SSM local declarations to C declarations.
genLocals :: [(I.VarId, I.Type)] -> [(CIdent, C.Type)]
genLocals = map $ bimap fromId (const value_t)

-- | Generate declarations for @numTrigs@ triggers.
genTrigs :: Int -> [(CIdent, C.Type)]
genTrigs numTrigs = zip (map trig_ [1 .. numTrigs]) (repeat trigger_t)

-- | The constant unit value, the singleton inhabitant of the type Unit.
unit :: C.Exp
unit = [cexp|0|]

-- | Fake undefined value used for expressions of type Void.
undef :: C.Exp
undef = [cexp|0xdeadbeef|]

{-------- Compilation --------}

-- | Generate a C compilation from an SSM program.
genProgram :: I.Program I.Type -> Compiler.Pass [C.Definition]
genProgram I.Program { I.programDefs = defs, I.typeDefs = typedefs } =
  let genAdt = (\acc adt -> acc <> genTypeDef adt)
  in  let (adts, adtsInfo) = foldl genAdt ([], mempty) typedefs
      in      --  if null typedefs
          --  then error "where are all the ADT definitions???? "
          --  else
          do
                                -- p@I.Program
            (cdecls, cdefs) <-
              bimap concat concat . unzip <$> mapM (genTop adtsInfo) defs
            return $ includes ++ adts ++ cdecls ++ cdefs -- ++ genInitProgram p

-- | Include statements in the generated C file.
includes :: [C.Definition]
includes = [cunit|
$esc:("#include \"ssm.h\"")
typedef char unit;
|]
-- $esc:("#define ssm_initialize_unit(v) ssm_initialize_event(v)")
-- $esc:("#define ssm_later_unit(l, d, r) ssm_later_event(l, d)")
-- $esc:("#define ssm_assign_unit(l, p, r) ssm_assign_event(l, p)")
-- typedef typename ssm_event_t ssm_unit_t;

-- | Setup the entry point of the program.
genInitProgram :: I.Program I.Type -> [C.Definition]
genInitProgram I.Program { I.programEntry = entry } = [cunit|
    int $id:initialize_program(void) {
       $exp:(activate enter_entry);
      return 0;
    }
  |]
 where
  enter_entry = [cexp|$id:(enter_ entry)(&$exp:top_parent,
                                          $exp:root_priority,
                                          $exp:root_depth)|]

-- | Generate C declarations and definitions for a top-level SSM function.
--
-- Each top-level function in a program is turned into three components:
--
-- 1. a struct (the activation record);
-- 2. an initialization function (the enter function); and
-- 3. a step function, which corresponds to the actual procedure body.
--
-- Items 2 and 3 include both declarations and definitions.
genTop
  :: TypeDefInfo
  -> (I.VarId, I.Expr I.Type)
  -> Compiler.Pass ([C.Definition], [C.Definition])
genTop info (name, l@(I.Lambda _ _ ty)) =
  runGenFn (fromId name) (zip argIds argTys) retTy body info $ do
    (stepDecl , stepDefn ) <- genStep
    (enterDecl, enterDefn) <- genEnter
    structDefn             <- genStruct
    return ([structDefn, enterDecl, stepDecl], [enterDefn, stepDefn])
 where
  (argIds, body ) = I.collectLambda l
  (argTys, retTy) = I.collectArrow ty
genTop _ (_, I.Lit _ _) = todo
genTop _ (_, _        ) = nope

-- | Generate struct definition for an SSM procedure.
--
-- This is where local variables, triggers, and parameter values are stored.
genStruct :: GenFn C.Definition
genStruct = do
  name   <- gets fnName
  params <- gets fnParams
  -- retTy  <- gets fnRetTy
  locs   <- gets fnLocs
  trigs  <- gets fnMaxWaits

  return [cedecl|
    typedef struct {
      $ty:act_t $id:act_member;

      $sdecls:(map structField $ genParams params)
      $ty:value_t *$id:ret_val;
      $sdecls:(map structField $ genLocals locs)
      $sdecls:(map structField $ genTrigs trigs)

    } $id:(act_typename name);
  |]
 where
  structField :: (CIdent, C.Type) -> C.FieldGroup
  structField (n, t) = [csdecl|$ty:t $id:n;|]

-- | Generate the enter function for an SSM procedure and its signature.
--
-- Its struct is allocated and initialized (partially; local variables' values are
-- left uninitialized).
genEnter :: GenFn (C.Definition, C.Definition)
genEnter = do
  actName <- gets fnName
  params  <- gets fnParams
  -- retTy   <- gets fnRetTy
  trigs   <- gets fnMaxWaits
  let act = act_ actName
      declParam (n, t) = [cparam|$ty:t $id:n|]
      initParam (n, _) = [[cstm|$id:acts->$id:n = $id:n;|]]
      initTrig (trigId, _) = [cstm|$id:acts->$id:trigId.act = $id:actg;|]

      enterParams =
        [ [cparam|$ty:act_t *$id:enter_caller|]
          , [cparam|$ty:priority_t $id:enter_priority|]
          , [cparam|$ty:depth_t $id:enter_depth|]
          ]
          ++ map declParam (genParams params)
          ++ [[cparam|$ty:value_t *$id:ret_val|]]

      alloc_act = enter [cexp|sizeof($ty:act)|]
                        [cexp|$id:(step_ actName)|]
                        [cexp|$id:enter_caller|]
                        [cexp|$id:enter_priority|]
                        [cexp|$id:enter_depth|]
      get_acts = to_act (cexpr actg) actName
  return
    ( [cedecl|$ty:act_t *$id:(enter_ actName)($params:enterParams);|]
    , [cedecl|
        $ty:act_t *$id:(enter_ actName)($params:enterParams) {
          $ty:act_t *$id:actg = $exp:alloc_act;
          $ty:act *$id:acts = $exp:get_acts;

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

-- | Generate the step function for an SSM procedure.
--
-- This function just defines the function definition and switch statement that
-- wraps the statements of the procedure. The heavy lifting is performed by
-- 'genExpr'.
genStep :: GenFn (C.Definition, C.Definition)
genStep = do
  actName   <- gets fnName
  actBody   <- gets fnBody
  firstCase <- nextCase
  (_, stms) <- genExpr actBody -- Toss away return value
  let act      = act_ actName
      get_acts = to_act (cexpr actg) actName
      do_leave = leave (cexpr actg) (csizeof act)
  return
    ( [cedecl|void $id:(step_ actName)($ty:act_t *$id:actg);|]
    , [cedecl|
        void $id:(step_ actName)($ty:act_t *$id:actg) {
          $ty:act *$id:acts = $exp:get_acts;

          switch ($id:actg->$id:act_pc) {
          case $int:firstCase:;
            $items:stms

          default:
            break;
          }
        $id:leave_label:
          $exp:do_leave;
        }
      |]
    )

-- | Helper to generate yield point in step function.
genYield :: GenFn [C.BlockItem]
genYield = do
  next <- nextCase
  return [citems|
    $id:actg->$id:act_pc = $int:next;
    return;
    case $int:next:;
    |]

-- | Translate an SSM expression into a C expression and statements.
--
-- SSM IR is a side-effectful expression language, with two implications when
-- translating to C:
--
-- 1.  every expression has a value (even if it is an uninhabited type), so this
--    must be reflected in C; and
-- 2.  some of the side effects in SSM cannot be implemented in C using expressions
--    alone.
--
-- These two implications roughly translate to the @C.Exp@ and @[C.BlockItem]@ in
-- @genExpr@'s return type. When we translate an SSM expression @e@:
--
-- > (val, stms) <- genExpr e
--
-- @val@ represents the C expression that corresponds to the value of @e@ upon
-- evaluation, while @stms@ represents the list of preceding statements that
-- compute @val@.
--
-- A further consideration upon point 2 is that SSM expressions may yield control
-- at any point. Thus, the C expression returned by @genExpr@ must accommodate the
-- step function suspending and resuming. For instance, consider the following SSM
-- IR expression:
--
-- > (let x = 3 in x) + (wait r; 6)
--
-- The @x@ in the let-binding in the left operand cannot just be a local variable
-- in the step function, because it would be "uninitialized" by the yield in the
-- right operand:
--
-- >   // let x = 3 in x
-- >   // stms:
-- >   int x = 3;
-- >   // exp: x
-- >
-- >   // (wait r; 6)
-- >   // stms:
-- >   ssm_sensitize(r);
-- >   actg->pc = N;
-- >   return;
-- > case N:
-- >   ssm_desensitize(r);
-- >   // exp: 6
-- >
-- >   // After the return, x is no longer initialized, so the following is
-- >   // undefined behavior:
-- >   x + 6
--
-- To ensure this is cannot happen, we conservatively declare @x@ as a local
-- variable in the activation record, so that its value is preserved between
-- yields, even if this is not usually necessary. We leave it to later compiler
-- passes to optimize this.
genExpr :: I.Expr I.Type -> GenFn (C.Exp, [C.BlockItem])
genExpr (I.Var n (I.TBuiltin (I.Arrow _ _))) =
  -- NOTE: Any identifiers of I.Arrow type must refer to a (global) function, so
  -- we just return a handle to its enter function.
  return ([cexp|$id:(enter_ n)|], [])
genExpr (I.Var n _) = do
  -- isLocal <- isLocalVar n -- TODO: check for global vs local variable
  return ([cexp|$id:acts->$id:n|], [])
genExpr (I.Data tg _) = do
  info <- gets adtInfo
  let Just initExpr = M.lookup tg (intInit info) in return (initExpr, [])
genExpr (I.Lit l t              ) = genLiteral l t
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
  case fn of
    (I.Var _ _) -> do
      tmpName <- nextTmp ty
      yield   <- genYield
      let tmp = [cexp|$id:acts->$id:tmpName|]
          enterArgs =
            [ [cexp|$id:actg|]
              , [cexp|$id:actg->$id:act_priority|]
              , [cexp|$id:actg->$id:act_depth|]
              ]
              ++ argVals
              ++ [[cexp|&$exp:tmp|]]
          call = [citems|$exp:(activate $ fnEnter `ccall` enterArgs);|]
      return (tmp, concat evalStms ++ call ++ yield)
    (I.Data tg dty) -> do
      info <- gets adtInfo
      case M.lookup tg (isPointer info) of
        Nothing    -> fail "Couldn't find tag"
        Just False -> fail "Cannot handle integer types with fields yet"
        Just True  -> do
          tmpName <- nextTmp dty
          let tmp = [cexp|$id:acts->$id:tmpName|]
          case M.lookup tg (dconType info) of
            Nothing -> do
              let theMap = show (M.toList (dconType info))
              fail
                (  "couldn't find "
                ++ ident tg
                ++ "in the lookup table!\n"
                ++ theMap
                )
            --Just typ -> do
            Just _ -> do

              -- let (Just typ) = M.lookup tg (dconType info)
            --  let (Just sz)  = M.lookup typ (typeSize info)
              let alloc      = undefined -- [[citem|$exp:tmp = $id:ssm_new($int:sz,$id:tg);|]]
              let initField  = undefined -- (\y i -> undefined) -- [citem| $exp:tmp->$id:payload[$uint:i] = $exp:y;|]
              let initFields = zipWith initField argVals [0 :: Int, 1 ..]
              return (undefined tmp, concat evalStms ++ alloc ++ initFields)
    _ -> fail $ "Cannot apply this expression: " ++ show fn
genExpr I.Match{}       = nope
genExpr I.Lambda{}      = fail "Cannot handle lambdas"
genExpr (I.Prim p es t) = genPrim p es t

-- | Generate code for SSM primitive; see 'genExpr' for extended discussion.
genPrim
  :: I.Primitive -> [I.Expr I.Type] -> I.Type -> GenFn (C.Exp, [C.BlockItem])
genPrim I.New [e] refType = do
  (val, stms) <- genExpr e
  tmp         <- genTmp refType
  return (tmp, stms ++ [citems|$exp:tmp = $exp:(new_sv val);|])
genPrim I.Dup [e] _ = do
  (val, stms) <- genExpr e
  return (unit, stms ++ [citems|$exp:(dup val);|])
genPrim I.Drop [e] _ = do
  (val, stms) <- genExpr e
  return (unit, stms ++ [citems|$exp:(drop val);|])
genPrim I.Reuse [_] _ = do
  -- TODO: delet this
  todo
genPrim I.Deref [a] ty = do
  (val, stms) <- genExpr a
  tmp         <- genTmp ty
  return (tmp, stms ++ [citems|$exp:tmp = $exp:(deref val);|])
genPrim I.Assign [lhs, rhs] _ = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  let prio = [cexp|$id:actg->$id:act_priority|]
      assignBlock = [citems|
          $items:lhsStms
          $items:rhsStms
          $exp:(assign lhsVal prio rhsVal);
        |]
  return (unit, assignBlock)
genPrim I.After [time, lhs, rhs] _ = do
  (timeVal, timeStms) <- first unmarshal <$> genExpr time
  (lhsVal , lhsStms ) <- genExpr lhs
  (rhsVal , rhsStms ) <- genExpr rhs
  let when = [cexp|$exp:now() + $exp:timeVal|]
      laterBlock = [citems|
          $items:timeStms
          $items:lhsStms
          $items:rhsStms
          $exp:(later lhsVal when rhsVal);
        |]
  return (unit, laterBlock)
genPrim I.Par procs _ = do
  yield <- genYield
  let
    numChildren = length procs
    parArgs     = genParArgs
      numChildren
      ([cexp|$id:actg->$id:act_priority|], [cexp|$id:actg->$id:act_depth|])
    checkNewDepth = [citems|
                        if ($id:actg->$id:act_depth < $exp:(depthSub numChildren))
                          $exp:(throw EXHAUSTED_PRIORITY);
                      |]

    genActivate
      :: ((C.Exp, C.Exp), I.Expr I.Type)
      -> GenFn (C.Exp, [C.BlockItem], C.BlockItem)
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
        , [citem|$exp:(activate $ fnEnter `ccall` enterArgs);|]
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
  let trigs = zip varVals $ map mkTrig [1 :: Int ..]
      mkTrig i = [cexp|&$exp:(acts_ $ trig_ i)|]
      sens (var, trig) = [citem|$exp:(sensitize var trig);|]
      desens (_, trig) = [citem|$exp:(desensitize trig);|]
  return (unit, concat varStms ++ map sens trigs ++ yield ++ map desens trigs)
genPrim I.Loop [b] _ = do
  (_, bodyStms) <- genExpr b
  return (unit, [citems|for (;;) { $items:bodyStms }|])
genPrim I.Break  []  _ = return (undef, [citems|break;|])
genPrim I.Return [e] _ = do
  (val, stms) <- genExpr e
  -- Assign to return argument and jump to leave
  let retBlock = [citems|
                    *$exp:(acts_ ret_val) = $exp:val;
                    goto $id:leave_label;
                 |]
  return (undef, stms ++ retBlock)
genPrim (I.PrimOp op) es t = genPrimOp op es t
genPrim _ _ _ = fail "Unsupported Primitive or wrong number of arguments"

-- | Generate C value for SSM literal.
genLiteral :: I.Literal -> I.Type -> GenFn (C.Exp, [C.BlockItem])
genLiteral (I.LitIntegral i    ) _ = return (marshal [cexp|$int:i|], [])
genLiteral (I.LitBool     True ) _ = return (marshal [cexp|true|], [])
genLiteral (I.LitBool     False) _ = return (marshal [cexp|false|], [])
genLiteral I.LitEvent            _ = return (marshal [cexp|1|], [])

-- | Generate C expression for SSM primitive operation.
genPrimOp
  :: I.PrimOp -> [I.Expr I.Type] -> I.Type -> GenFn (C.Exp, [C.BlockItem])
genPrimOp I.PrimAdd [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal + $exp:rhsVal|], stms)
  -- TODO: optimization:
  --  All integers are 31 bits + 1 tag bit, so zero tag bit on one argument,
  -- add together, and the result will be sum with a tag bit of 1.
  -- let val = word_to_val
  --           [cexp|$exp:(val_to_word lhsVal) + ($exp:(val_to_word rhsVal) & ~1)|]
  -- return (val, stms)
genPrimOp I.PrimSub [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal - $exp:rhsVal|], stms)
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
  ((lhsVal, rhsVal), stms) <- 
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal & $exp:rhsVal|], stms)
genPrimOp I.PrimBitOr [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <- 
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal | $exp:rhsVal|], stms)
genPrimOp I.PrimBitNot [opr] _ = do
  (val, stms) <- first unmarshal <$> genExpr opr
  -- TODO: optimization:
  -- all integers are 31 bits + 1 tag bit, so val XOR (~1) flips the 31 bits and
  -- keeps the tag bit 1.
  return (marshal [cexp|($exp:val ^ (~1))|], stms)
genPrimOp I.PrimEq [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal == $exp:rhsVal|], stms)
genPrimOp I.PrimNot [opr] _ = do
  (val, stms) <- first unmarshal <$> genExpr opr
  return (marshal [cexp|! $exp:val|], stms)
genPrimOp I.PrimGt [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal < $exp:rhsVal|], stms)
genPrimOp I.PrimGe [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal <= $exp:rhsVal|], stms)
genPrimOp I.PrimLt [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal > $exp:rhsVal|], stms)
genPrimOp I.PrimLe [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal >= $exp:rhsVal|], stms)
genPrimOp _ _ _ = fail "Unsupported PrimOp or wrong number of arguments"

-- | Helper for sequencing across binary operations.
genBinop
  :: I.Expr I.Type -> I.Expr I.Type -> GenFn ((C.Exp, C.Exp), [C.BlockItem])
genBinop lhs rhs = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  return ((lhsVal, rhsVal), lhsStms ++ rhsStms)

-- | Compute priority and depth arguments for a par fork of given width.
genParArgs :: Int -> (C.Exp, C.Exp) -> [(C.Exp, C.Exp)]
genParArgs width (currentPrio, currentDepth) =
  [ let p = [cexp|$exp:currentPrio + ($int:(i-1) * (1 << $exp:d))|]
        d = [cexp|$exp:currentDepth - $exp:(depthSub width)|]
    in  (p, d)
  | i <- [1 .. width]
  ]

-- | How much the depth should be decreased when par forking given width.
depthSub :: Int -> C.Exp
depthSub width = [cexp|$int:ds|]
  where ds = ceiling $ logBase (2 :: Double) $ fromIntegral width :: Int
