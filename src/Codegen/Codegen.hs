{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- | Translate SSM program to C compilation unit.

What is expected of the IR:

Well-formed: All primitive functions are applied to the right number of
arguments.

Pure par expression: All par-expressions' operands are applications that have no
side effects.

Defunctionalized: No lambdas; the only terms with an arrow type are variables
or applications.

Name mangled: All variable identifiers are unique.
-}
module Codegen.Codegen where

import Codegen.LibSSM
import Codegen.Typegen (
  DConInfo (..),
  TConInfo (..),
  TypegenInfo (..),
  genTypes,
 )

import qualified IR.IR as I
import qualified IR.Types as I

import Language.C.Quote.GCC
import qualified Language.C.Syntax as C

import qualified Common.Compiler as Compiler
import Common.Identifiers (fromId, fromString)

import Control.Monad (foldM, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Lazy (
  MonadState,
  StateT (..),
  evalStateT,
  gets,
  modify,
 )
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Prelude hiding (drop)

import GHC.Stack (HasCallStack)


-- | Possible, but temporarily punted for the sake of expediency.
todo :: HasCallStack => a
todo = error "Not yet implemented"


-- | Impossible without a discussion about implementation strategy.
nope :: HasCallStack => a
nope = error "Not yet supported"


-- | Hack to allow us to splice string literals into C AST
newtype EscExp = EscExp String


instance ToExp EscExp where
  toExp (EscExp e) loc = C.EscExp e loc


{- | State maintained while compiling a top-level SSM function.

The information here is populated while generating the step function, so that
should be computed first, before this information is used to generate the act
struct and enter definitions.
-}
data GenFnState = GenFnState
  { fnName :: I.VarId
  -- ^ Function name
  , fnParams :: [I.Binder I.Type]
  -- ^ Function parameters
  , fnRetTy :: I.Type
  -- ^ Function return type
  , fnBody :: I.Expr I.Type
  -- ^ Function body
  , fnLocals :: M.Map I.VarId I.Type
  -- ^ Function local variables
  , fnVars :: M.Map I.VarId C.Exp
  -- ^ How to resolve variables
  , fnMaxWaits :: Int
  -- ^ Number of triggers needed
  , fnCases :: Int
  -- ^ Yield point counter
  , fnFresh :: Int
  -- ^ Temporary variable name counter
  , fnTypeInfo :: TypegenInfo
  -- ^ (User-defined) type information
  }


{- | Translation monad for procedures, with derived typeclass instances.

We declare 'GenFn' as a newtype so that we can implement 'MonadFail' for it,
allowing us to use monadic pattern matching.
-}
newtype GenFn a = GenFn (StateT GenFnState Compiler.Pass a)
  deriving (Functor) via StateT GenFnState Compiler.Pass
  deriving (Applicative) via StateT GenFnState Compiler.Pass
  deriving (Monad) via StateT GenFnState Compiler.Pass
  deriving (MonadFail) via StateT GenFnState Compiler.Pass
  deriving (MonadError Compiler.Error) via StateT GenFnState Compiler.Pass
  deriving (MonadState GenFnState) via StateT GenFnState Compiler.Pass
  deriving (Compiler.MonadWriter [Compiler.Warning]) via StateT GenFnState Compiler.Pass


-- | Run a 'GenFn' computation on a procedure.
runGenFn
  :: I.VarId
  -- ^ Name of procedure
  -> [I.Binder I.Type]
  -- ^ Names and types of parameters to procedure
  -> I.Expr I.Type
  -- ^ Body of procedure
  -> TypegenInfo
  -- ^ Type information
  -> [(I.VarId, I.Type)]
  -- ^ Other global identifiers
  -> GenFn a
  -- ^ Translation monad to run
  -> Compiler.Pass a
  -- ^ Pass on errors to caller
runGenFn name params body typeInfo globals (GenFn tra) =
  evalStateT tra $
    GenFnState
      { fnName = name
      , fnParams = params
      , fnRetTy = I.extract body
      , fnBody = body
      , fnLocals = M.empty
      , fnVars =
          M.fromList $ mapMaybe resolveParam params ++ map genGlobal globals
      , fnTypeInfo = typeInfo
      , fnMaxWaits = 0
      , fnCases = 0
      , fnFresh = 0
      }
 where
  resolveParam :: I.Binder I.Type -> Maybe (I.VarId, C.Exp)
  resolveParam (I.BindVar v _) = Just (v, acts_ $ fromId v)
  resolveParam _ = Nothing

  genGlobal :: (I.VarId, I.Type) -> (I.VarId, C.Exp)
  genGlobal (v, I.Arrow _ _) = (v, static_value $ closure_ v)
  genGlobal _ = todo


-- | Lookup some information associated with a type constructor.
getsTCon :: (TConInfo -> a) -> I.TConId -> GenFn a
getsTCon f i = do
  Just a <- fmap f . (`tconInfo` i) <$> gets fnTypeInfo
  return a


-- | Lookup some information associated with a data constructor.
getsDCon :: (DConInfo -> a) -> I.DConId -> GenFn a
getsDCon f i = do
  Just a <- fmap f . (`dconInfo` i) <$> gets fnTypeInfo
  return a


-- | Read and increment the number of cases in a procedure, i.e., @fnCases++@.
nextCase :: GenFn Int
nextCase = do
  n <- gets fnCases
  modify $ \st -> st{fnCases = n + 1}
  return n


-- | Obtain fresh integer in the 'GenFn' monad
getFresh :: GenFn Int
getFresh = do
  n <- gets fnFresh
  modify $ \st -> st{fnFresh = n + 1}
  return n


-- | Bind a variable to a C expression.
addBinding :: I.Binder I.Type -> C.Exp -> GenFn ()
addBinding (I.BindVar v _) e =
  modify $ \st -> st{fnVars = M.insert v e $ fnVars st}
addBinding _ _ = return ()


-- | Register a new local variable, to be declared in activation record.
addLocal :: I.VarId -> I.Type -> GenFn I.VarId
addLocal v t = do
  fnl <- gets fnLocals
  v' <-
    if v `M.member` fnl
      then do
        ctr <- getFresh
        return $ v <> fromString (show ctr)
      else return v
  modify $ \st -> st{fnLocals = M.insert v' t fnl}
  return v'


-- | Bind a variable to a C expression only while computing the given monad.
withBindings :: [(I.Binder I.Type, C.Exp)] -> GenFn a -> GenFn a
withBindings bs m = do
  fnv <- gets fnVars
  mapM_ (uncurry addBinding) bs
  a <- m
  modify $ \st -> st{fnVars = fnv}
  return a


-- | Register a local variable and bind its C expression during a monad.
withNewLocal :: (I.VarId, I.Type) -> GenFn a -> GenFn a
withNewLocal (v, t) m = do
  v' <- addLocal v t
  withBindings [(I.BindVar v t, acts_ $ fromId v')] m


-- | Register number of wait statements track of number of triggers needed.
maxWait :: Int -> GenFn ()
maxWait n = modify $ \st -> st{fnMaxWaits = n `max` fnMaxWaits st}


-- | Generate a fresh label.
freshLabel :: GenFn CIdent
freshLabel = fromId . label_ <$> getFresh


-- | Generate anonymous local variable in activation record for storage.
genTmp :: I.Type -> GenFn C.Exp
genTmp ty = do
  v <- fromId . tmp_ <$> getFresh
  v' <- addLocal v ty
  return $ acts_ $ fromId v'


-- | Translate a list of SSM parameters to C parameters.
genParams :: [I.Binder I.Type] -> [(CIdent, C.Type)]
genParams = zipWith genArg [0 ..]
 where
  genArg _ (I.BindVar v _) = (fromId v, value_t)
  genArg i _ = (arg_ i, value_t)


-- | Translate a list of SSM local declarations to C declarations.
genLocals :: [(I.VarId, I.Type)] -> [(CIdent, C.Type)]
genLocals = map $ bimap fromId (const value_t)


-- | Generate declarations for @numTrigs@ triggers.
genTrigs :: Int -> [(CIdent, C.Type)]
genTrigs numTrigs = zip (map trig_ [1 .. numTrigs]) (repeat trigger_t)


-- | The constant unit value, the singleton inhabitant of the type Unit.
unit :: C.Exp
unit = marshal [cexp|0|]


-- | Fake undefined value used for expressions of type Void.
undef :: C.Exp
undef = marshal [cexp|0xdeadbeef|]


{-------- Compilation --------}

{- | Generate a C compilation from an SSM program.

 Each top-level function in a program is turned into three components:

 1. a struct (the activation record);
 2. an initialization function (the enter function); and
 3. a step function, which corresponds to the actual procedure body.

 Items 2 and 3 include both declarations and definitions.
-}
genProgram :: I.Program I.Type -> Compiler.Pass [C.Definition]
genProgram p = do
  (tdefs, tinfo) <- genTypes $ I.typeDefs p
  (cdecls, cdefns) <- cUnpack <$> mapM (genTop tinfo) (I.programDefs p)
  externs <- mapM genExtern $ I.externDecls p
  return $
    includes
      ++ tdefs
      ++ externs
      ++ cescs
      ++ cdecls
      ++ cdefns
      ++ genInitProgram (I.programEntry p)
 where
  genTop
    :: TypegenInfo
    -> (I.Binder I.Type, I.Expr I.Type)
    -> Compiler.Pass ([C.Definition], [C.Definition])
  genTop tinfo (I.BindVar name _, l@I.Lambda{}) =
    runGenFn (fromId name) argIds body tinfo tops $ do
      (stepDecl, stepDefn) <- genStep
      (enterDecl, enterDefn) <- genEnter
      (closureDecl, closureDefn) <- genStaticClosure
      structDefn <- genStruct
      return
        ( [structDefn, enterDecl, closureDecl, stepDecl]
        , [enterDefn, closureDefn, stepDefn]
        )
   where
    tops = mapMaybe extractBindVar $ I.programDefs p
    extractBindVar (I.binderToVar -> Just v, e) = Just (v, I.extract e)
    extractBindVar _ = Nothing
    (argIds, body) = I.unfoldLambda l
  genTop _ (_, I.Lit _ _) = todo
  genTop _ (_, _) = nope

  cUnpack = bimap concat concat . unzip
  cescs = [cunit|$esc:(I.cDefs p)|]


-- | Include statements in the generated C file.
includes :: [C.Definition]
includes =
  [cunit|
$esc:("#include \"ssm.h\"")
typedef char unit;
|]


-- | Setup the entry point of the program.
genInitProgram :: I.VarId -> [C.Definition]
genInitProgram = const []


-- genInitProgram entry = [cunit|
--     $ty:act_t *$id:stdout_handler_enter($ty:act_t *parent,
--                                         $ty:priority_t priority,
--                                         $ty:depth_t depth,
--                                         $ty:value_t *argv,
--                                         $ty:value_t *ret);
--
--     void $id:stdin_handler_spawn($ty:sv_t *ssm_stdin);
--     void $id:stdin_handler_kill(void);
--
--     void $id:program_init(void) {
--       $ty:value_t ssm_stdin = $exp:std_sv;
--       $ty:value_t ssm_stdout = $exp:std_sv;
--
--       $ty:value_t std_argv[2] = { ssm_stdin, ssm_stdout };
--
--       $exp:(activate enter_stdout);
--       $exp:(activate enter_entry);
--
--       $id:stdin_handler_spawn($exp:(to_sv $ cexpr "ssm_stdin"));
--     }
--
--     void $id:program_exit(void) {
--       $id:stdin_handler_kill();
--     }
--   |]
--  where
--   std_sv                    = new_sv $ marshal [cexp|0|]
--
--   parArgs                   = genParArgs 2 (root_priority, root_depth)
--   (stdoutPrio, stdoutDepth) = head parArgs
--   (entryPrio , entryDepth ) = parArgs !! 1
--   enter_stdout = [cexp|$id:stdout_handler_enter(&$exp:top_parent,
--                                                 $exp:stdoutPrio,
--                                                 $exp:stdoutDepth,
--                                                 &ssm_stdout,
--                                                 NULL)|]
--
--   enter_entry = [cexp|$id:(enter_ entry)(&$exp:top_parent,
--                                          $exp:entryPrio,
--                                          $exp:entryDepth,
--                                          std_argv,
--                                          NULL)|]

genExtern :: (I.VarId, I.Type) -> Compiler.Pass C.Definition
genExtern (v, t) =
  return
    [cedecl|
    extern $ty:value_t $id:v($params:xparams);
  |]
 where
  argNum = length $ fst $ I.unfoldArrow t
  xparams = replicate argNum [cparam|$ty:value_t|]


{- | Generate struct definition for an SSM procedure.

 This is where local variables, triggers, and parameter values are stored.
-}
genStruct :: GenFn C.Definition
genStruct = do
  name <- gets fnName
  params <- gets fnParams
  -- retTy  <- gets fnRetTy
  locs <- M.toList <$> gets fnLocals
  trigs <- gets fnMaxWaits

  return
    [cedecl|
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


{- | Generate the enter function for an SSM procedure and its signature.

 Its struct is allocated and initialized (partially; local variables' values are
 left uninitialized).
-}
genEnter :: GenFn (C.Definition, C.Definition)
genEnter = do
  actName <- gets fnName
  params <- gets fnParams
  trigs <- gets fnMaxWaits
  let act = act_ actName
      enterParams =
        [ [cparam|$ty:act_t *$id:enter_caller|]
        , [cparam|$ty:priority_t $id:enter_priority|]
        , [cparam|$ty:depth_t $id:enter_depth|]
        , [cparam|$ty:value_t *$id:argv|]
        , [cparam|$ty:value_t *$id:ret_val|]
        ]
      alloc_act =
        enter
          [cexp|sizeof($ty:act)|]
          [cexp|$id:(step_ actName)|]
          [cexp|$id:enter_caller|]
          [cexp|$id:enter_priority|]
          [cexp|$id:enter_depth|]
      get_acts = to_act (cexpr actg) actName

      initParam :: (CIdent, b) -> Int -> C.Stm
      initParam (n, _) i = [cstm|$id:acts->$id:n = $id:argv[$int:i];|]

      initTrig :: (CIdent, b) -> C.Stm
      initTrig (trigId, _) = [cstm|$id:acts->$id:trigId.act = $id:actg;|]
  return
    ( [cedecl|$ty:act_t *$id:(enter_ actName)($params:enterParams);|]
    , [cedecl|
        $ty:act_t *$id:(enter_ actName)($params:enterParams) {
          $ty:act_t *$id:actg = $exp:alloc_act;
          $ty:act *$id:acts = $exp:get_acts;

          /* Assign parameters */
          $stms:(zipWith initParam (genParams params) [0..])

          /* Set return value */
          $id:acts->$id:ret_val = $id:ret_val;

          /* Initialize triggers */
          $stms:(map initTrig $ genTrigs trigs)

          return $id:actg;
        }
      |]
    )


-- | Generate static closure for top-level function
genStaticClosure :: GenFn (C.Definition, C.Definition)
genStaticClosure = do
  actName <- gets fnName
  argc <- length <$> gets fnParams
  let closure_name = closure_ actName
      enter_f = [cexp|$id:(enter_ actName)|]
  return
    ( [cedecl|extern $ty:closure1_t $id:closure_name;|]
    , [cedecl|$ty:closure1_t $id:closure_name = $init:(static_closure enter_f argc);|]
    )


{- | Generate the step function for an SSM procedure.

 This function just defines the function definition and switch statement that
 wraps the statements of the procedure. The heavy lifting is performed by
 'genExpr'.
-}
genStep :: GenFn (C.Definition, C.Definition)
genStep = do
  actName <- gets fnName
  actBody <- gets fnBody
  firstCase <- nextCase
  (ret_expr, stms) <- genExpr actBody -- Toss away return value
  let act = act_ actName
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
        if ($id:acts->$id:ret_val)
          *$id:acts->$id:ret_val = $exp:ret_expr;
        $exp:do_leave;
        }
      |]
    )


-- | Helper to generate yield point in step function.
genYield :: GenFn [C.BlockItem]
genYield = do
  next <- nextCase
  return
    [citems|
    $id:actg->$id:act_pc = $int:next;
    return;
    case $int:next:;
    |]


{- | Translate an SSM expression into a C expression and statements.

 SSM IR is a side-effectful expression language, with two implications when
 translating to C:

 1.  every expression has a value (even if it is an uninhabited type), so this
    must be reflected in C; and
 2.  some of the side effects in SSM cannot be implemented in C using expressions
    alone.

 These two implications roughly translate to the @C.Exp@ and @[C.BlockItem]@ in
 @genExpr@'s return type. When we translate an SSM expression @e@:

 > (val, stms) <- genExpr e

 @val@ represents the C expression that corresponds to the value of @e@ upon
 evaluation, while @stms@ represents the list of preceding statements that
 compute @val@.

 A further consideration upon point 2 is that SSM expressions may yield control
 at any point. Thus, the C expression returned by @genExpr@ must accommodate the
 step function suspending and resuming. For instance, consider the following SSM
 IR expression:

 > (let x = 3 in x) + (wait r; 6)

 The @x@ in the let-binding in the left operand cannot just be a local variable
 in the step function, because it would be "uninitialized" by the yield in the
 right operand:

 >   // let x = 3 in x
 >   // stms:
 >   int x = 3;
 >   // exp: x
 >
 >   // (wait r; 6)
 >   // stms:
 >   ssm_sensitize(r);
 >   actg->pc = N;
 >   return;
 > case N:
 >   ssm_desensitize(r);
 >   // exp: 6
 >
 >   // After the return, x is no longer initialized, so the following is
 >   // undefined behavior:
 >   x + 6

 To ensure this is cannot happen, we conservatively declare @x@ as a local
 variable in the activation record, so that its value is preserved between
 yields, even if this is not usually necessary. We leave it to later compiler
 passes to optimize this.
-}
genExpr :: I.Expr I.Type -> GenFn (C.Exp, [C.BlockItem])
genExpr (I.Var n _) = do
  mv <- M.lookup n <$> gets fnVars
  v <- maybe err return mv
  return (v, [])
 where
  err = Compiler.unexpected $ "Codegen: Could not find I.Var named " <> show n
genExpr (I.Data dcon _) = do
  e <- getsDCon dconConstruct dcon
  return (e, [])
genExpr (I.Lit l ty) = do
  tmp <- genTmp ty
  return (tmp, [citems|$exp:tmp = $exp:(genLiteral l);|])
genExpr (I.Let [(I.BindVar n _, d)] b _) = do
  (defVal, defStms) <- genExpr d
  withNewLocal (n, I.extract d) $ do
    -- Look up n because withNewLocal may have mangled its name.
    Just n' <- M.lookup n <$> gets fnVars
    let defInit = [citems|$exp:n' = $exp:defVal;|]
    (bodyVal, bodyStms) <- genExpr b
    return (bodyVal, defStms ++ defInit ++ bodyStms)
genExpr (I.Let [(I.BindAnon _, d)] b _) = do
  (_, defStms) <- genExpr d -- Throw away value
  (bodyVal, bodyStms) <- genExpr b
  return (bodyVal, defStms ++ bodyStms)
genExpr I.Let{} = fail "Cannot handle mutually recursive bindings"
genExpr e@(I.App _ _ ty) = do
  let (fn, args) = second (map fst) $ I.unfoldApp e
  -- args must be non-empty because a is an App
  case fn of
    -- I.Var _ _ -> do
    (I.Prim I.Dup [I.Var _ _] _) -> do
      (fnExp, fnStms) <- genExpr fn
      foldM apply (fnExp, fnStms) args
     where
      apply (f, stms) a = do
        (aVal, aStms) <- genExpr a -- first evaluate argument
        ret <- genTmp ty -- allocate return value address
        yield <- genYield -- application might necessitate yield
        let current = [cexp|$id:actg|]
            prio = [cexp|$exp:current->$id:act_priority|]
            depth = [cexp|$exp:current->$id:act_depth|]
            retp = [cexp|&$exp:ret|]
            appStms =
              [citems|
              $exp:(closure_apply f aVal current prio depth retp);
              if ($exp:(has_children current)) {
                $items:yield;
              }
              $exp:(drop f);
            |]
        return (ret, stms ++ aStms ++ appStms)
    (I.Data dcon dty) -> do
      (argVals, evalStms) <- unzip <$> mapM genExpr args
      onHeap <- getsDCon dconOnHeap dcon
      unless onHeap $ do
        fail $ "Cannot handle packed fields yet, for: " ++ show dcon
      construct <- getsDCon dconConstruct dcon
      destruct <- getsDCon dconDestruct dcon
      tmp <- genTmp dty
      let alloc = [[citem|$exp:tmp = $exp:construct;|]]
          initField y i = [citem|$exp:(destruct i tmp) = $exp:y;|]
          initFields = zipWith initField argVals [0 ..]
      return (tmp, concat evalStms ++ alloc ++ initFields)
    _ -> fail $ "Cannot apply this expression: " ++ show fn
genExpr (I.Match s as t) = do
  -- To implement a match expression, we need to generate a C switch statement
  --
  -- which switches on the tag of the scrutinee @s@.
  -- However, since we're already using a switch statement for jumping to
  -- program counters, we cannot simply nest the switch statement, since we will
  -- risk the inner switch statement (for the match expression) shadowing the
  -- cases of the outer switch statement (for the program counters). In
  -- particular, we will be unable to yield inside of match expression arm with
  -- this naive compilation scheme.
  --
  -- So, in order to keep the C statements "flat", we use a basic block-style
  -- scheme, where the inner switch statement only jumps (using @goto@) to
  -- blocks corresponding to each arm of the match expression; this ensures that
  -- we never need to yield in the inner switch statement, thus side-stepping
  -- C's limitation. At the end of each block, we jump to a join statement that
  -- follows the blocks generated for each arm.
  (sExp, sStms) <- genExpr s
  scrut <- genTmp $ I.extract s
  val <- genTmp t
  joinLabel <- freshLabel

  let assignScrut = [citems|$exp:scrut = $exp:sExp;|]
      tag = adt_tag scrut -- TODO: look this up using typeScrut
      switch cases = [citems|switch ($exp:tag) { $items:cases }|]
      assignVal e = [citems|$exp:val = $exp:e;|]
      joinStm = [citems|$id:joinLabel:;|]

      genArm :: (I.Alt I.Type, I.Expr I.Type) -> GenFn ([C.BlockItem], [C.BlockItem])
      genArm (alt, arm) = do
        armLabel <- freshLabel
        (altLabel, armBlk) <- withAltScope armLabel alt $ do
          (armExp, armStms) <- genExpr arm
          return $ armStms ++ assignVal armExp
        let armCase = [citems|$item:altLabel goto $id:armLabel;|]
        return (armCase, armBlk)
      mkBlk :: CIdent -> [C.BlockItem] -> [C.BlockItem]
      mkBlk label blk =
        [citems|$id:label:;|] ++ blk ++ [citems|goto $id:joinLabel;|]
      withAltScope
        :: CIdent
        -> I.Alt I.Type
        -> GenFn [C.BlockItem]
        -> GenFn (C.BlockItem, [C.BlockItem])
      withAltScope label a@(I.AltData dcon _ _) m = do
        destruct <- getsDCon dconDestruct dcon
        cas <- getsDCon dconCase dcon
        -- NOTE: we assume here that this AltData is flat, i.e., the number of
        -- fields in the AltData is the same as what we expect for this data
        -- constructor (which we obtain from dconDestruct).
        let fieldBinds = I.altBinders a `zip` map (`destruct` scrut) [0 ..]
        blk <- withBindings fieldBinds m
        return ([citem|case $exp:cas:;|], mkBlk label blk)
      withAltScope label (I.AltLit l _) m = do
        blk <- m
        return ([citem|case $exp:(genLiteralRaw l):;|], mkBlk label blk)
      withAltScope label (I.AltBinder b) m = do
        blk <- withBindings [(b, scrut)] m
        addBinding b scrut
        return ([citem|default:;|], mkBlk label blk)

  (cases, blks) <- bimap concat concat . unzip <$> mapM genArm as
  return (val, sStms ++ assignScrut ++ switch cases ++ blks ++ joinStm)
genExpr I.Lambda{} = fail "Cannot handle lambdas"
genExpr (I.Prim p es t) = genPrim p es t
genExpr (I.Exception _ t) = do
  tmp <- genTmp t
  return (tmp, [citems|$exp:(throw INTERNAL_ERROR);|]) -- unit instead of temp?


-- | Generate code for SSM primitive; see 'genExpr' for extended discussion.
genPrim
  :: I.Primitive -> [I.Expr I.Type] -> I.Type -> GenFn (C.Exp, [C.BlockItem])
genPrim I.New [e] refType = do
  (val, stms) <- genExpr e
  tmp <- genTmp refType
  return (tmp, stms ++ [citems|$exp:tmp = $exp:(new_sv val); $exp:(drop val);|])
genPrim I.Dup [e] _ = do
  (val, stms) <- genExpr e
  return (val, stms ++ [citems|$exp:(dup val);|])
genPrim I.Drop [e, r] _ = do
  (val, stms) <- genExpr e
  (ref, stms') <- genExpr r -- NOTE: this should never really be side-effectful!
  return (val, stms ++ stms' ++ [citems|$exp:(drop ref);|])
genPrim I.Deref [a] ty = do
  (val, stms) <- genExpr a
  tmp <- genTmp ty
  return (tmp, stms ++ [citems|$exp:tmp = $exp:(deref val); $exp:(drop val);|])
genPrim I.Assign [lhs, rhs] _ = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  let prio = [cexp|$id:actg->$id:act_priority|]
      assignBlock =
        [citems|
          $items:lhsStms
          $items:rhsStms
          $exp:(assign lhsVal prio rhsVal);
          $exp:(drop rhsVal);
          $exp:(drop lhsVal);
        |]
  return (unit, assignBlock)
genPrim I.After [time, lhs, rhs] _ = do
  (timeVal, timeStms) <- genExpr time
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  let when = [cexp|$exp:now() + $exp:(unmarshal timeVal)|]
      laterBlock =
        [citems|
          $items:timeStms
          $items:lhsStms
          $items:rhsStms
          $exp:(later lhsVal when rhsVal);
          $exp:(drop timeVal);
          $exp:(drop rhsVal);
          $exp:(drop lhsVal);
        |]
  return (unit, laterBlock)
genPrim I.Par procs _ = do
  let numChildren = length procs
      parArgs =
        genParArgs
          numChildren
          ([cexp|$id:actg->$id:act_priority|], [cexp|$id:actg->$id:act_depth|])

      checkNewDepth =
        [citems|
          if ($id:actg->$id:act_depth < $exp:(depthSub numChildren))
            $exp:(throw EXHAUSTED_PRIORITY);
        |]

      -- FIXME: ideally, par expressions must all be parallel applications that
      -- don't yield/block. However, that relies on a liftPar pass that isn't
      -- implemented just yet.
      -- So, this is currently broken in that side effects inside the arguments
      -- of function calls will be evaluated sequentially, which is wrong.
      apply
        :: (I.Expr I.Type, (C.Exp, C.Exp))
        -> GenFn (C.Exp, [C.BlockItem], [C.BlockItem])
      apply (I.App fn arg ty, (prio, depth)) = do
        (fnExp, fnStms) <- genExpr fn
        (argExp, argStms) <- genExpr arg
        ret <- genTmp ty
        let current = [cexp|$id:actg|]
            retp = [cexp|&$exp:ret|]
            appStms =
              [citems|
              $exp:(closure_apply fnExp argExp current prio depth retp);
              $exp:(drop fnExp);
            |]
        return (ret, fnStms ++ argStms, appStms)
      apply (e, _) = do
        fail $ "Cannot compile par with non-application expression: " ++ show e

  (_rets, befores, activates) <- unzip3 <$> mapM apply (zip procs parArgs)
  yield <- genYield
  let parRetVal = unit -- TODO: return tuple of values
  return
    (parRetVal, checkNewDepth ++ concat befores ++ concat activates ++ yield)
genPrim I.Wait vars _ = do
  (varVals, varStms) <- unzip <$> mapM genExpr vars
  maxWait $ length varVals
  yield <- genYield
  let trigs = zip varVals $ map mkTrig [1 :: Int ..]
      mkTrig i = [cexp|&$exp:(acts_ $ trig_ i)|]
      sens (var, trig) = [citem|$exp:(sensitize var trig);|]
      desens (var, trig) = [citems|$exp:(desensitize trig); $exp:(drop var);|]
  return
    (unit, concat varStms ++ map sens trigs ++ yield ++ concatMap desens trigs)
genPrim I.Loop [b] _ = do
  (_, bodyStms) <- genExpr b
  return (unit, [citems|for (;;) { $items:bodyStms }|])
genPrim I.Break [] _ = return (undef, [citems|break;|])
genPrim I.Now [] t = do
  tmp <- genTmp t
  return (tmp, [citems|$exp:tmp = $exp:(new_time $ ccall now []);|])
genPrim I.Last [r] t = do
  (r', stms) <- genExpr r
  tmp <- genTmp t
  return (tmp, stms ++ [citems|$exp:tmp = $exp:(new_time $ sv_last_updated r');|])
genPrim (I.CQuote e) [] _ = return ([cexp|$exp:(EscExp e)|], [])
genPrim (I.CCall s) es _ = do
  (argExps, argStms) <- second concat . unzip <$> mapM genExpr es
  -- TODO: obtain return value from call
  return (unit, argStms ++ [citems|$id:s($args:argExps);|])
genPrim (I.FfiCall s) es ty = do
  (argExps, argStms) <- second concat . unzip <$> mapM genExpr es
  ret <- genTmp ty
  let doDrop arg = [citem|$exp:(drop arg);|]
  return
    ( ret
    , argStms
        ++ [citems|$exp:ret = $id:s($args:argExps);|]
        ++ map doDrop argExps
    )
genPrim (I.PrimOp op) es t = do
  (opVal, opStms) <- genPrimOp op es t
  tmp <- genTmp t
  return (tmp, opStms ++ [citems|$exp:tmp = $exp:opVal;|])
genPrim _ _ _ = fail "Unsupported Primitive or wrong number of arguments"


-- | Generate C value for SSM literal, marshalled.
genLiteral :: I.Literal -> C.Exp
genLiteral = marshal . genLiteralRaw


-- | Generate C value for SSM literal, unmarshalled.
genLiteralRaw :: I.Literal -> C.Exp
genLiteralRaw (I.LitIntegral i) = [cexp|$int:i|]
genLiteralRaw I.LitEvent = [cexp|1|]


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
genPrimOp I.PrimNeq [lhs, rhs] _ = do
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal unmarshal) <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal != $exp:rhsVal|], stms)
genPrimOp I.PrimNot [opr] _ = do
  (val, stms) <- first unmarshal <$> genExpr opr
  return (marshal [cexp|! $exp:val|], stms)
genPrimOp I.PrimGt [lhs, rhs] _ = do
  let unmarshal' = cast_to_signed Size32 . (`shl` cint 1) . unmarshal
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal' unmarshal') <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal > $exp:rhsVal|], stms)
genPrimOp I.PrimGe [lhs, rhs] _ = do
  let unmarshal' = cast_to_signed Size32 . (`shl` cint 1) . unmarshal
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal' unmarshal') <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal >= $exp:rhsVal|], stms)
genPrimOp I.PrimLt [lhs, rhs] _ = do
  let unmarshal' = cast_to_signed Size32 . (`shl` cint 1) . unmarshal
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal' unmarshal') <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal < $exp:rhsVal|], stms)
genPrimOp I.PrimLe [lhs, rhs] _ = do
  let unmarshal' = cast_to_signed Size32 . (`shl` cint 1) . unmarshal
  ((lhsVal, rhsVal), stms) <-
    first (bimap unmarshal' unmarshal') <$> genBinop lhs rhs
  return (marshal [cexp|$exp:lhsVal <= $exp:rhsVal|], stms)
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
     in (p, d)
  | i <- [1 .. width]
  ]


-- | How much the depth should be decreased when par forking given width.
depthSub :: Int -> C.Exp
depthSub width = [cexp|$int:ds|]
 where
  ds = ceiling $ logBase (2 :: Double) $ fromIntegral width :: Int
