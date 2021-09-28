-- | Translate SSM program to C compilation unit.
--
-- Each procedure in a program is turned into three components:
--
-- 1) A struct (the activation record)
-- 2) An initialization function (the enter function)
-- 3) A step function, which corresponds to the actual procedure body

{-# LANGUAGE QuasiQuotes #-}

module Codegen.Codegen where

import           Control.Monad.State.Lazy       ( State
                                                , evalState
                                                , gets
                                                , modify
                                                )

import           Data.Either                    ( rights )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           Data.List                      ( sortOn )

import           Codegen.Identifiers

import qualified IR.Mono                       as L
import qualified Types.Mono                    as L

-- TODOs: remove hard-coded identifiers.

-- | Given a 'Program', returns a tuple containing the compiled program and
-- a list of all `include` statements.
-- compile_ :: IR.Program -> ([C.Definition], [C.Definition])
-- compile_ program = (compUnit, includes)
--  where
--   -- | The file to generate, minus include statements
--   compUnit :: [C.Definition]
--   compUnit = concat
--     [globals, declarePeripherals program, preamble, decls, defns, initProg]

--   -- | Global reference declarations
--   globals :: [C.Definition]
--   globals = genGlobals (globalReferences program)

--   initProg :: [C.Definition]
--   initProg = genInitProgram program

--   -- | Preamble, macros etc
--   preamble :: [C.Definition]
--   preamble = genPreamble

--   -- | declarations and definitions, prototypes etc
--   decls, defns :: [C.Definition]
--   (decls, defns) =
--     concat2 $ unzip $ map genProcedure $ Map.elems (funs program)

--   -- | Utility function to distribute @concat@ over a tuple
--   concat2 :: ([[a]], [[b]]) -> ([a], [b])
--   concat2 (x, y) = (concat x, concat y)

{- | State maintained while compiling a 'Procedure'.

The information here is populated while generating the step function, so that
should be computed first, before this information is used to generate the act
struct and enter definitions.
-}
data TRState = TRState
  { fnName   :: L.VarId
  , fnArgs   :: [(L.VarId, L.Type)]
  , fnBody   :: L.MIExpr
  , caseNum  :: Int
  , locals   :: [(L.VarId, L.Type)]
  , maxWaits :: Int
  }

-- | Translation monad.
type TR a = State TRState a

-- | Run a TR computation on a procedure.
runTR :: L.VarId -> [(L.VarId, L.Type)] -> L.MIExpr -> TR a -> a
runTR name args body tra = evalState tra $ TRState { fnName   = name
                                                   , fnArgs   = args
                                                   , fnBody   = body
                                                   , caseNum  = 0
                                                   , locals   = []
                                                   , maxWaits = 0
                                                   }

-- | Read and increment the number of cases in a procedure, i.e., `ncase++`.
nextCase :: TR Int
nextCase = do
  n <- gets caseNum
  modify $ \st -> st { caseNum = n + 1 }
  return n

-- | Register a local variable for which an sv should be allocated.
addLocal :: (L.VarId, L.Type) -> TR ()
addLocal l = modify $ \st -> st { locals = l : locals st }

-- | Register a name to be waited on
addWait :: Int -> TR ()
addWait n = modify $ \st -> st { maxWaits = n `max` maxWaits st }


{-------- Compilation unit generation --------}

-- | Include statements in the generated file
includes :: [C.Definition]
includes = [cunit|
$esc:("#include \"ssm-platform.h\"")
|]

-- | Setup the entry point of the program.
genInitProgram :: L.MIProgram -> [C.Definition]
genInitProgram _ = [cunit|
  int $id:initialize_program(void) {
    $id:activate($id:(enter_ "main")(&$id:top_parent, $id:root_priority, $id:root_depth));
    return 0;
  }
|]


{-------- Function generation --------}

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
trigOf :: L.VarId -> CIdent
trigOf v = "__trig_" ++ v

{- | Generate definitions for an SSM 'Procedure'.

The fst element of the returned tuple contains the struct definition and
function prototype declarations, while the snd element contains the function
definitions.
-}
genTop :: (L.VarId, L.MIExpr) -> [C.Definition]
genTop (name, L.Lambda arg argTy body) = runTR name [(arg, argTy)] body $ do
  (stepDecl , stepDefn ) <- genStep
  (enterDecl, enterDefn) <- genEnter
  structDefn             <- genStruct
  return [structDefn, enterDecl, stepDecl, enterDefn, stepDefn]
genTop (name, L.Lit l) = error "TODO: literals"
genTop (name, _      ) = error "TODO: arbitrary top-level expressions"

-- | Return the C variant of the base type of a SSM type
typeIdent :: L.Type -> CIdent
typeIdent (L.TCon t    ) = t
typeIdent (L.TArrow _ _) = undefined
typeIdent (L.TSV t     ) = sv_ $ typeIdent t

unwrapSV :: L.Type -> L.Type
unwrapSV (L.TSV t) = t
unwrapSV _         = error "not SV"


ctype :: CIdent -> C.Type
ctype t = [cty|typename $id:t|]

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

      $sdecls:(map param args)
      $sdecls:(map local ls)
      $sdecls:(map trig [1..ts])

    } $id:(act_ name);
  |]
 where
  -- SVs are passed by reference, everything else passed by value
  param :: (L.VarId, L.Type) -> C.FieldGroup
  param (n, t@(L.TSV _)) = [csdecl|$ty:(ctype $ typeIdent t) *$id:n;|]
  param (n, t          ) = [csdecl|$ty:(ctype $ typeIdent t) $id:n;|]

  -- Declare an SV for each local variable
  local :: (L.VarId, L.Type) -> C.FieldGroup
  local (n, t) = [csdecl|$ty:(ctype $ sv_ $ typeIdent t) $id:n;|]

  trig :: Int -> C.FieldGroup
  trig i = [csdecl|$ty:trigger_t $id:(trigOf $ show i);|]


{- | Generate the enter function for an SSM 'Procedure'.

Its struct is allocated and initialized (partially; local variables' values are
left uninitialized).
-}
genEnter :: TR (C.Definition, C.Definition)
genEnter = do
  actname <- gets fnName
  args    <- gets fnArgs
  ts      <- gets maxWaits
  ls      <- gets locals
  let act   = [cty|typename $id:act'|]
      act'  = act_ actname -- hack to use this typename as expr in macros
      enter = enter_ actname
      step  = step_ actname
      params =
        [cparams|$ty:act_t *caller, $ty:priority_t priority, $ty:depth_t depth|]
          ++ map param args
  return
    ( [cedecl|$ty:act_t *$id:enter($params:params);|]
    , [cedecl|
        $ty:act_t *$id:enter($params:params) {
          $ty:act_t *$id:actg = $id:act_enter(sizeof($ty:act), $id:step, caller, priority, depth);
          $ty:act *$id:acts = container_of($id:actg, $id:act', act);

          /* Initialize and assign parameters */
          $stms:(concatMap initParam args)

          /* Initialize locals */
          $stms:(concat $ map initLocal ls)

          /* Initialize triggers */
          $stms:(map initTrig [1..ts])

          return $id:actg;
        }
      |]
    )
 where
  -- SVs are passed by reference, everything else passed by value
  param :: (L.VarId, L.Type) -> C.Param
  param (n, t@(L.TSV _)) = [cparam|$ty:(ctype $ typeIdent t) *$id:n|]
  param (n, t          ) = [cparam|$ty:(ctype $ typeIdent t) $id:n|]

  -- | Initialize act struct from parameters
  initParam :: (L.VarId, L.Type) -> [C.Stm]
  initParam (n, _) = [[cstm|acts->$id:n = $id:n;|]]

  -- | Initialize locals
  initLocal :: (L.VarId, L.Type) -> [C.Stm]
  initLocal (n, t) = [[cstm| $id:(initialize_ $ typeIdent t)(&acts->$id:n);|]]

  -- | Initialize a trigger
  initTrig :: Int -> C.Stm
  initTrig i = [cstm| $id:acts->$id:(trigOf $ show i).act = $id:actg;|]

{- | Generate the step function for an SSM 'Procedure'.

This function just defines the function definition and switch statement that
wraps the statements of the procedure. The heavy lifting is performed by
'genCase'.
-}
genStep :: TR (C.Definition, C.Definition)
genStep = do
  actName   <- gets fnName
  actBody   <- gets fnBody
  _         <- nextCase -- Toss away 0th case
  (_, stms) <- genExpr actBody -- Toss away return value
  ls        <- gets locals
  let act  = [cty|typename $id:actt|]
      actt = act_ actName -- hack to use this typename as expr in macros
      step = step_ actName

      -- | Dequeue any outstanding event on a reference
      dequeue :: (L.VarId, L.Type) -> C.Stm
      dequeue (n, _) = [cstm|$id:unsched_event(&$id:acts->$id:n.sv);|]
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
          $stms:(map dequeue ls)
          $id:act_leave($id:actg, sizeof($ty:act));
        }
      |]
    )

genExpr :: L.MIExpr -> TR (C.Exp, [C.BlockItem])
genExpr (L.Var n _             ) = return ([cexp|&$id:acts->$id:n|], [])
genExpr (L.Lit (L.Literal _s _)) = error "todo"
genExpr (L.Let [(n, v)] b      ) = do
  addLocal (n, L.exprType v)
  (defVal, defStms) <- genExpr v
  let defBlock = [citem|
    {
      $items:defStms
      $id:(initialize_ $ typeIdent $ L.exprType v)(&acts->$id:n);
      $id:acts->$id:n.$id:value = $exp:defVal;
    }
    |]
  (bodyVal, bodyStms) <- genExpr b
  return (bodyVal, defBlock : bodyStms)
genExpr (L.Let _ _       ) = error "Cannot handle mutually recursive bindings"
genExpr (L.PrimApp f es t) = genPrim f es t
genExpr (L.Match _ _ _ _ ) = error "Not implemented"
genExpr (L.Lambda  _ _  _) = error "Not implemented"
genExpr (L.App _ _       ) = error "Not implemented"

genPrim :: L.PrimFun -> [L.MIExpr] -> L.Type -> TR (C.Exp, [C.BlockItem])
genPrim L.Loop [b] _ = do
  (_, bodyStms) <- genExpr (L.App b (L.Lit undefined)) -- TODO: need to apply unit literal
  return (undef, [citems|for (;;) { $items:bodyStms }|])
genPrim L.Break  []  _ = return (undef, [citems|break;|])
genPrim L.Return []  _ = return (undef, [citems|return;|])
genPrim L.Fork   _   _ = error "todo"
genPrim L.Wait   vs   _ = do
  (varVals, varStms) <- unzip <$> mapM genExpr vs
  nextPc <- nextCase
  addWait $ length varVals
  let trigs = zip [1::Int ..] varVals
      sens (i, var) = [citem|$id:sensitize($exp:var, &$id:acts->$id:(trig_ i));|]
      desens (i, _) = [citem|$id:desensitize(&$id:acts->$id:(trig_ i));|]
      waitBlock =
        [citem|
        {
          $items:(concat varStms)
          $items:(map sens trigs)
          $id:actg->$id:pc = $int:nextPc;
          return;
          case $int:nextPc: ;
          $items:(map desens trigs)
        }
        |]
  return (unit, [waitBlock])
genPrim L.Deref [e] _ = do
  (val, stms) <- genExpr e
  return ([cexp|*$exp:val|], stms)
genPrim L.Assign [lhs, rhs] _ = do
  (lhsVal, lhsStms) <- genExpr lhs
  (rhsVal, rhsStms) <- genExpr rhs
  -- TODO: handle event type
  let lhsTy = typeIdent $ unwrapSV $ L.exprType lhs
      assignBlock = [citem|
        {
          $items:lhsStms
          $items:rhsStms
          $id:(assign_ lhsTy)($exp:lhsVal, $exp:rhsVal);
        }
      |]
  return (unit, [assignBlock])

genPrim L.Later [lhs, rhs, time] _ = do
  (timeVal, timeStms) <- genExpr time
  (lhsVal , lhsStms ) <- genExpr lhs
  (rhsVal , rhsStms ) <- genExpr rhs
  -- TODO: handle event type
  let lhsTy = typeIdent $ unwrapSV $ L.exprType lhs
      laterBlock = [citem|
        {
          $items:timeStms
          $items:lhsStms
          $items:rhsStms
          $id:(later_ lhsTy)($exp:lhsVal, $exp:rhsVal, $id:now() + $exp:timeVal);
        }
      |]
  return (unit, [laterBlock])
genPrim (L.Arith _) _ _ = error "todo"
genPrim (L.Ffi   _) _ _ = error "unimplemented"
genPrim _           _ _ = error "Unexpected prim fun"

unit :: C.Exp
unit = [cexp|0|]

undef :: C.Exp
undef = [cexp|0xdeadbeef|]

  {-
     OLD
{- | Generate the list of statements from each 'Stm' in an SSM 'Procedure'.

Note that this compilation scheme might not work if the language were to
support return statements. This could be fixed by generating a break, and
moving the leave call to outside of the switch statement in 'genStep'.
-}
genCase :: L.Expr -> TR [C.Stm]
genCase (NewRef n t v) = do
  locs <- gets locals
  let lvar = identName n
      lhs  = [cexp|&$id:acts->$id:lvar|]
      rhs  = genExp locs v
  addLocal $ makeDynamicRef n (mkReference t)
  case baseType t of
    TEvent -> return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority);|]]
    _ ->
      return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority, $exp:rhs);|]]
genCase (SetRef r e) = do
  locs <- gets locals
  let lvar = refIdent r
      t    = refType r
      lhs  = refPtr r locs
      rhs  = genExp locs e
  case baseType t of
    TEvent -> return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority);|]]
    _ ->
      return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority, $exp:rhs);|]]
genCase (SetLocal n t e) = do
  locs <- gets locals
  let lvar = identName n
      lhs  = [cexp|&$id:acts->$id:lvar|]
      rhs  = genExp locs e
  return [[cstm| $id:acts->$id:(identName n) = $exp:rhs;|]]
genCase (If c t e) = do
  locs <- gets locals
  let cnd = genExp locs c
  thn <- concat <$> mapM genCase t
  els <- concat <$> mapM genCase e
  return [[cstm| if ($exp:cnd) { $stms:thn } else { $stms:els }|]]
genCase (While c b) = do
  locs <- gets locals
  let cnd = genExp locs c
  bod <- concat <$> mapM genCase b
  return [[cstm| while ($exp:cnd) { $id:debug_microtick(); $stms:bod } |]]
genCase (After d r v) = do
  locs <- gets locals
  let lvar = refIdent r
      t    = refType r
      del  = genTimeDelay locs d
      lhs  = refPtr r locs
      rhs  = genExp locs v
  -- Note that the semantics of 'After' and 'later_' differ---the former
  -- expects a relative time, whereas the latter takes an absolute time.
  -- Thus we add now() in the code we generate.
  case baseType t of
    TEvent -> return [[cstm| $id:(later_ t)($exp:lhs, $id:now() + $exp:del);|]]
    _      -> return
      [[cstm| $id:(later_ t)($exp:lhs, $id:now() + $exp:del, $exp:rhs);|]]
genCase (Wait ts) = do
  caseNum <- nextCase
  maxWaits $ length ts
  locs <- gets locals
  let trigs = zip [1 ..] $ map (`refSV` locs) ts
  return
    $  map getTrace      ts
    ++ map sensitizeTrig trigs
    ++ [ [cstm| $id:actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
    ++ fmap desensitizeTrig trigs
 where
  sensitizeTrig :: (Int, C.Exp) -> C.Stm
  sensitizeTrig (i, trig) =
    [cstm|$id:sensitize($exp:trig, &$id:acts->$id:(trig_ i));|]

  desensitizeTrig :: (Int, C.Exp) -> C.Stm
  desensitizeTrig (i, _) = [cstm|$id:desensitize(&$id:acts->$id:(trig_ i));|]

  getTrace :: Reference -> C.Stm
  getTrace r = [cstm|$id:debug_trace($string:event);|]
    where event = show $ T.ActSensitize $ refName r
genCase (Fork cs) = do
  locs    <- gets locals
  caseNum <- nextCase
  let
    genCall :: Int -> (Ident, [Either SSMExp Reference]) -> C.Stm
    genCall i (r, as) =
      [cstm|$id:fork($id:(enter_ (identName r))($args:enterArgs));|]
     where
      enterArgs =
        [ [cexp|actg|]
          , [cexp|actg->priority + $int:i * (1 << $exp:newDepth)|]
          , newDepth
          ]
          ++ map genArg as
      genArg :: Either SSMExp Reference -> C.Exp
      genArg (Left  e) = genExp locs e
      genArg (Right r) = refPtr r locs

    newDepth :: C.Exp
    newDepth = [cexp|actg->depth - $int:depthSub|]

    depthSub :: Int
    depthSub =
      (ceiling $ logBase (2 :: Double) $ fromIntegral $ length cs) :: Int

    checkNewDepth :: C.Stm
    checkNewDepth = [cstm|
      if ($id:actg->depth < $int:depthSub)
         $id:throw($exp:exhausted_priority); |]

    genTrace :: (Ident, [Either SSMExp Reference]) -> C.Stm
    genTrace (r, _) = [cstm|$id:debug_trace($string:event);|]
      where event = show $ T.ActActivate $ identName r

  return
    $  checkNewDepth
    :  map genTrace cs
    ++ zipWith genCall [0 :: Int ..] cs
    ++ [ [cstm| $id:actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
genCase Skip = return []

  {-
-- | Generate C expression from 'SSMExp' and a list of local variables.
-- genExp :: [Reference] -> SSMExp -> C.Exp
-- genExp _  (Var t n              )
--   | baseType t == TEvent = [cexp|0|]
--   | otherwise            = [cexp|acts->$id:(identName n)|]
-- genExp _  (Lit _ (LInt32  i    )) = [cexp|(typename i32) $int:i|]
-- genExp _  (Lit _ (LUInt8  i    )) = [cexp|(typename u8) $int:i|]
-- genExp _  (Lit _ (LUInt32 i    )) = [cexp|(typename u32) $int:i|]
-- genExp _  (Lit _ (LInt64  i    )) = [cexp|(typename i64) $int:i|]
-- genExp _  (Lit _ (LUInt64 i    )) = [cexp|(typename u64) $int:i|]
-- genExp _  (Lit _ (LBool   True )) = [cexp|true|]
-- genExp _  (Lit _ (LBool   False)) = [cexp|false|]
-- genExp _  (Lit _ (LEvent       )) = [cexp|0|]
-- genExp ls (UOpE _ e op)           = case op of
--   Neg -> [cexp|- $exp:(genExp ls e)|]
--   Not -> [cexp|! $exp:(genExp ls e)|]
-- genExp ls (UOpR t r op) = case op of
--   Changed -> [cexp|$id:event_on($exp:(refSV r ls))|]
--   Deref   -> case t of
--     TEvent -> [cexp|0|]
--     _      -> [cexp|$exp:(refVal r ls)|]
-- -- | Circumvent optimizations that take advantage of C's undefined signed
-- -- integer wraparound behavior. FIXME: remove this hack, which is probably not
-- -- robust anyway if C is aggressive about inlining.
-- genExp ls (BOp ty e1 e2 op)
--   | ty == TInt32 && op == OPlus = [cexp|_add($exp:c1, $exp:c2)|]
--   | otherwise                   = gen op
--  where
--   (c1, c2) = (genExp ls e1, genExp ls e2)
--   gen OPlus  = [cexp|$exp:c1 + $exp:c2|]
--   gen OMinus = [cexp|$exp:c1 - $exp:c2|]
--   gen OTimes = [cexp|$exp:c1 * $exp:c2|]
--   gen ODiv   = [cexp|$exp:c1 / $exp:c2|]
--   gen ORem   = [cexp|$exp:c1 % $exp:c2|]
--   gen OMin   = [cexp|$exp:c1 < $exp:c2 ? $exp:c1 : $exp:c2|]
--   gen OMax   = [cexp|$exp:c1 < $exp:c2 ? $exp:c2 : $exp:c1|]
--   gen OLT    = [cexp|$exp:c1 < $exp:c2|]
--   gen OEQ    = [cexp|$exp:c1 == $exp:c2|]
--   gen OAnd   = [cexp|$exp:c1 && $exp:c2|]
--   gen OOr    = [cexp|$exp:c1 || $exp:c2|]

-- genTimeDelay :: [Reference] -> SSMTime -> C.Exp
-- genTimeDelay ls (SSMTime d) = [cexp|$exp:(genExp ls d)|]
-}
  -}
