{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Bindings to the ssm-runtime library.
module Codegen.LibSSM where

import Common.Identifiers (
  DConId,
  Identifiable (..),
  Identifier (..),
  VarId (..),
  fromId,
 )
import Data.String (IsString (..))
import Language.C.Quote (
  Id (Id),
  ToIdent (..),
 )
import Language.C.Quote.GCC (
  cexp,
  cinit,
  cty,
 )
import qualified Language.C.Syntax as C


-- Allow snake_case for c literals
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- | Identifiers in C.
newtype CIdent = CIdent Identifier
  deriving (Eq)
  deriving (Ord)
  deriving (Show) via Identifier
  deriving (IsString) via Identifier
  deriving (Identifiable) via Identifier
  deriving (ToIdent) via Identifier
  deriving (Semigroup) via Identifier
  deriving (Monoid) via Identifier


-- | Construct a type name from a C identifier.
ctype :: CIdent -> C.Type
ctype i = [cty|typename $id:i|]


-- | Construct an expression from a C identifier.
cexpr :: CIdent -> C.Exp
cexpr i = [cexp|$id:i|]


-- | Construct an expression of the size of a C type.
csizeof :: C.Type -> C.Exp
csizeof t = [cexp|sizeof($ty:t)|]


-- | Construct an integer literal in C.
cint :: Int -> C.Exp
cint i = [cexp|$int:i|]


ccall :: C.Exp -> [C.Exp] -> C.Exp
fn `ccall` args = [cexp|$exp:fn($args:args)|]


amp :: C.Exp -> C.Exp
amp e = [cexp|&$exp:e|]


star :: C.Exp -> C.Exp
star e = [cexp|*$exp:e|]


-- | Natively supported sizes in C.
data CSize
  = Size8
  | Size16
  | Size32
  | Size64
  deriving (Eq, Ord)


-- | Convert a 'CSize' into an integer.
size_to_int :: CSize -> Int
size_to_int Size8 = 8
size_to_int Size16 = 16
size_to_int Size32 = 32
size_to_int Size64 = 64


-- | Convert a 'CSize' into some string identifier of the size.
size_to_string :: IsString s => CSize -> s
size_to_string = fromString . show . size_to_int


{---- from ssm.h {{{ ----}

-- | @enum ssm_error@, an enumeration of possible runtime errors.
data SSMError
  = -- | Reserved for unforeseen, non-user-facing errors.
    INTERNAL_ERROR
  | -- | Tried to insert into full ready queue.
    EXHAUSTED_ACT_QUEUE
  | -- | Tried to insert into full event queue.
    EXHAUSTED_EVENT_QUEUE
  | -- | Could not allocate more memory.
    EXHAUSTED_MEMORY
  | -- | Tried to exceed available recursion depth.
    EXHAUSTED_PRIORITY
  | -- | Not yet ready to perform the requested action.
    NOT_READY
  | -- | Specified invalid time.
    INVALID_TIME
  | -- | Invalid memory layout.
    INVALID_MEMORY
  deriving (Show)


instance ToIdent SSMError where
  toIdent e = Id $ "SSM_" ++ show e


-- | @SSM_THROW@, throw a runtime error.
throw :: SSMError -> C.Exp
throw e = [cexp|SSM_THROW($id:e)|]


-- TODO: throw

-- | @ssm_word_t@, the largest supported machine word size.
word_t :: C.Type
word_t = [cty|typename ssm_word_t|]


-- | @ssm_value_t@, runtime representation for sslang values.
value_t :: C.Type
value_t = [cty|typename ssm_value_t|]


-- | Name of the @packed_val@ field in a 'value_t'.
packed_val :: CIdent
packed_val = "packed_val"


-- | Name of the @heap_ptr@ field in a 'value_t'.
heap_ptr :: CIdent
heap_ptr = "heap_ptr"


-- TODO: skip ssm mm

-- | @ssm_marshal@, construct a 'value_t' out of a 31-bit integral value.
marshal :: C.Exp -> C.Exp
marshal v = [cexp|ssm_marshal($exp:(cast_to_unsigned Size32 v))|]


-- | @ssm_unmarshal@, extract 31-bit integral value out of a 'value_t'.
unmarshal :: C.Exp -> C.Exp
unmarshal v = cast_to_unsigned Size32 [cexp|ssm_unmarshal($exp:v)|]


-- | @ssm_on_heap@, whether a 'value_t' points to something on the heap.
on_heap :: C.Exp -> C.Exp
on_heap v = [cexp|ssm_on_heap($exp:v)|]


-- | @ssm_dup@, increment the reference count of a value.
dup :: C.Exp -> C.Exp
dup v = [cexp|ssm_dup($exp:v)|]


-- | @ssm_dups@, increment the reference count on a vector of values.
dups :: C.Exp -> C.Exp -> C.Exp
dups c v = [cexp|ssm_dups($exp:c, $exp:v)|]


-- | @ssm_drop@, drop the reference count of a value and free if necessary.
drop :: C.Exp -> C.Exp
drop v = [cexp|ssm_drop($exp:v)|]


-- | @ssm_time_t@, 64-bit time type.
time_t :: C.Type
time_t = [cty|typename ssm_time_t|]


-- | @struct ssm_time@, the heap-representation of a 'time_t'.
time_obj_t :: C.Type
time_obj_t = [cty|struct ssm_time|]


-- | @SSM_NEVER@, a 'time_t' that is never reached.
never :: C.Exp
never = [cexp|SSM_NEVER|]


-- TODO: time units

-- | @ssm_now@, the current time.
now :: C.Exp
now = [cexp|ssm_now|]


-- | @ssm_new_time@, allocate a 'time_obj_t' on the heap.
new_time :: C.Exp -> C.Exp
new_time t = [cexp|ssm_new_time($exp:t)|]


-- | @ssm_time_read@, read the time in a 'time_obj_t'.
read_time :: C.Exp -> C.Exp
read_time to = [cexp|ssm_time_read($exp:to)|]


-- | @ssm_priority_t@, thread priority.
priority_t :: C.Type
priority_t = [cty|typename ssm_priority_t|]


-- | @ssm_depth_t@, thread depth.
depth_t :: C.Type
depth_t = [cty|typename ssm_depth_t|]


-- | @SSM_ROOT_PRIORITY@, the depth of the root process.
root_priority :: C.Exp
root_priority = [cexp|SSM_ROOT_PRIORITY|]


-- | @SSM_ROOT_DEPTH@, the depth of the root process.
root_depth :: C.Exp
root_depth = [cexp|SSM_ROOT_DEPTH|]


-- | @ssm_act_t@, the generic activation record type.
act_t :: C.Type
act_t = [cty|typename ssm_act_t|]


-- | Name of the program counter field in an 'act_t'.
act_pc :: CIdent
act_pc = "pc"


-- | Name of the caller field in an 'act_t'.
act_caller :: CIdent
act_caller = "caller"


-- | Name of the depth field in an 'act_t'.
act_depth :: CIdent
act_depth = "depth"


-- | Name of the priority field in an 'act_t'.
act_priority :: CIdent
act_priority = "priority"


-- | @ssm_trigger_t@, nodes in the linked list of triggers.
trigger_t :: C.Type
trigger_t = [cty|typename ssm_trigger_t|]


-- | @ssm_enter@, allocate and initialize activation record.
enter :: C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp
enter size step parent prio dep =
  [cexp|ssm_enter($exp:size, $exp:step, $exp:parent, $exp:prio, $exp:dep)|]


-- | @ssm_leave@, deallocate an activation record before leaving.
leave :: C.Exp -> C.Exp -> C.Exp
leave act size = [cexp|ssm_leave($exp:act, $exp:size)|]


-- | @ssm_has_children@, returns non-zero if @act@ has at least one child.
has_children :: C.Exp -> C.Exp
has_children act = [cexp|ssm_has_children($exp:act)|]


-- | @ssm_activate@, schedule an activation record on the ready queue.
activate :: C.Exp -> C.Exp
activate act = [cexp|ssm_activate($exp:act)|]


-- | @ssm_top_parent@, Name of top level parent activation record
top_parent :: C.Exp
top_parent = [cexp|ssm_top_parent|]


-- | @ssm_sv_t@, polymorphic scheduled variables.
sv_t :: C.Type
sv_t = [cty|typename ssm_sv_t|]


-- | @ssm_new_sv@, allocate a new 'sv_t' on the heap.
new_sv :: C.Exp -> C.Exp
new_sv v = [cexp|ssm_new_sv($exp:v)|]


-- | @ssm_to_sv@, retrieve the 'sv_t' pointer pointed to by a 'value_t'.
to_sv :: C.Exp -> C.Exp
to_sv v = [cexp|ssm_to_sv($exp:v)|]


-- | @ssm_deref@, read the value of an 'sv_t' pointed to by a 'value_t'.
deref :: C.Exp -> C.Exp
deref v = [cexp|ssm_deref($exp:v)|]


-- | @ssm_assign@, assign to a scheduled variable.
assign :: C.Exp -> C.Exp -> C.Exp -> C.Exp
assign var prio val = [cexp|ssm_assign($exp:var, $exp:prio, $exp:val)|]


-- | @ssm_later@, schedule a delayed assignment to a scheduled variable.
later :: C.Exp -> C.Exp -> C.Exp -> C.Exp
later var when val = [cexp|ssm_later($exp:var, $exp:when, $exp:val)|]


-- TODO: unsafe assign and later

-- | @ssm_sensitize@, sensitize a trigger to a variable.
sensitize :: C.Exp -> C.Exp -> C.Exp
sensitize var trig = [cexp|ssm_sensitize($exp:var, $exp:trig)|]


-- | @ssm_sensitize@, sensitize a trigger to a variable.
desensitize :: C.Exp -> C.Exp
desensitize trig = [cexp|ssm_desensitize($exp:trig)|]


-- | @ssm_new_adt@, allocate a new ADT object on the heap.
new_adt :: Int -> DConId -> C.Exp
new_adt field_count tag = [cexp|ssm_new_adt($uint:field_count, $id:tag)|]


-- | @ssm_adt_field@, access the @i@th field of an ADT object. Assignable.
adt_field :: C.Exp -> Int -> C.Exp
adt_field v i = [cexp|ssm_adt_field($exp:v, $uint:i)|]


-- | @ssm_tag@, extract the tag of an ADT value.
adt_tag :: C.Exp -> C.Exp
adt_tag v = [cexp|ssm_tag($exp:v)|]


-- | @ssm_closure1_t@, the (template) type of a closure with a single argument.
closure1_t :: C.Type
closure1_t = [cty|struct ssm_closure1|]


{- | Inintializer for a "static" closure that contains no arguments.

 FIXME: An ugly hack that shouldn't exist because John didn't have the
 foresight to provide an interface to define static closures.
-}
static_closure :: C.Exp -> Int -> C.Initializer
static_closure f argc =
  [cinit|{
    .mm = {
      .ref_count = 1,
      .kind = SSM_CLOSURE_K,
      .info = {
        .vector = {
          .count = 0,
          .cap = $int:argc,
        },
      },
    },
    .f = $exp:f,
    .argv = {{0}}, // https://stackoverflow.com/q/13746033/10497710
  }|]


-- | Promote a static object to an @ssm_value_t@ (warning: hacky!).
static_value :: CIdent -> C.Exp
static_value name = [cexp|($ty:value_t) { .heap_ptr = &$id:name.mm }|]


-- | @ssm_new_closure@, allocate a new closure object on the heap.
new_closure :: CIdent -> Int -> C.Exp
new_closure f n = [cexp|ssm_new_closure(&$id:f, $int:n)|]


-- | @ssm_closure_push@, add a new argument to a closure.
closure_push :: C.Exp -> C.Exp -> C.Exp
closure_push f a = [cexp|ssm_closure_push($exp:f, $exp:a)|]


-- | @ssm_closure_pop@, remove an argument from a closure.
closure_pop :: C.Exp -> C.Exp
closure_pop f = [cexp|ssm_closure_pop($exp:f)|]


-- | @ssm_closure_apply@, apply a closure to an argument.
closure_apply :: C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp
closure_apply f a act prio depth ret =
  [cexp|ssm_closure_apply($exp:f, $exp:a, $exp:act, $exp:prio, $exp:depth, $exp:ret)|]


-- | @ssm_closure_apply@, apply a closure to an argument, consuming the closure.
closure_apply_final
  :: C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp
closure_apply_final f a act prio depth ret =
  [cexp|ssm_closure_apply_final($exp:f, $exp:a, $exp:act, $exp:prio, $exp:depth, $exp:ret)|]


-- | @ssm_closure_free@, free a closure (without performing reference counting).
closure_free :: C.Exp -> C.Exp
closure_free f = [cexp|ssm_closure_free($exp:f)|]


-- | Name of the pseudonymous macro from the Linux kernel.
container_of :: CIdent
container_of = "container_of"


-- | Name of program initialization hook, called to set up program with runtime.
program_init :: CIdent
program_init = "ssm_program_init"


-- | Name of program destruction hook, called before gracefully exiting program.
program_exit :: CIdent
program_exit = "ssm_program_exit"


{- | Name of stdout handler enter function, used to bind stdout for POSIX platforms.
 NOTE: this is a hack
-}
stdout_handler_enter :: CIdent
stdout_handler_enter = "__enter_stdout_handler"


{- | Name of stdin handler spawner, used to bind stdin for POSIX platforms.
 NOTE: this is a hack
-}
stdin_handler_spawn :: CIdent
stdin_handler_spawn = "__spawn_stdin_handler"


{- | Name of stdin handler killer, used to destroy handler thread on POSIX.
 NOTE: this is a hack
-}
stdin_handler_kill :: CIdent
stdin_handler_kill = "__kill_stdin_handler"


{---- from ssm.h }}} ----}

{---- Naming conventions {{{ ----}

-- | Obtain the name of a process activation record struct.
act_typename :: VarId -> CIdent
act_typename name = "act_" <> fromId name <> "_t"


-- | Identifier for act member in act struct.
act_member :: CIdent
act_member = "act"


-- | Obtain the type of a process activation record.
act_ :: VarId -> C.Type
act_ name = [cty|typename $id:(act_typename name)|]


-- | Obtain the process-specific activation record from a generic one.
to_act :: C.Exp -> VarId -> C.Exp
to_act act name =
  [cexp|$id:container_of($exp:act, $id:(act_typename name), $id:act_member)|]


-- | Obtain the name for the enter function of a routine.
enter_ :: VarId -> CIdent
enter_ name = "__enter_" <> fromId name


-- | Obtain the name for the static closure of a routine.
closure_ :: VarId -> CIdent
closure_ name = "__closure_" <> fromId name


-- | Obtain the name of the step function of a routine.
step_ :: VarId -> CIdent
step_ name = "__step_" <> fromId name


-- | Obtain the name of each trigger for a routine.
trig_ :: Int -> CIdent
trig_ i = "__trig_" <> fromString (show i)


-- | Obtain the name of a temporary variable.
tmp_ :: Int -> CIdent
tmp_ i = "__tmp_" <> fromString (show i)


-- | Obtain the name of a label.
label_ :: Int -> CIdent
label_ i = "__label_" <> fromString (show i)


-- | Obtain the name of an argument variable.
arg_ :: Int -> CIdent
arg_ i = "__arg_" <> fromString (show i)


-- | Obtain the name of an argument variable.
argv :: CIdent
argv = "__argv"


-- | Name of return argument.
ret_val :: CIdent
ret_val = "__return_val"


-- | Identifier for generic (inner) struct act.
actg :: CIdent
actg = "actg"


-- | Identifier for specialized (outer) struct act.
acts :: CIdent
acts = "acts"


-- | Access activation record member.
acts_ :: CIdent -> C.Exp
acts_ i = [cexp|$id:acts->$id:i|]


-- | Name of the caller argument of an enter call.
enter_caller :: CIdent
enter_caller = "caller"


-- | Name of the priority argument of an enter call.
enter_priority :: CIdent
enter_priority = "priority"


-- | Name of the depth argument of an enter call.
enter_depth :: CIdent
enter_depth = "depth"


-- | Label to jump to terminate execution.
leave_label :: CIdent
leave_label = "__leave_step"


{---- Naming conventions }}} ----}

{---- Quasiquoting helpers {{{ ----}

-- | Cast to a signed integer of a particular size.
cast_to_signed :: CSize -> C.Exp -> C.Exp
cast_to_signed = cast_to_int True


-- | Cast to an unsigned integer of a particular size.
cast_to_unsigned :: CSize -> C.Exp -> C.Exp
cast_to_unsigned = cast_to_int False


-- | Cast to an integer of a particular size and signedness.
cast_to_int :: Bool -> CSize -> C.Exp -> C.Exp
cast_to_int signed size e = [cexp|(typename $id:int_t) $exp:e|]
 where
  int_t :: CIdent
  int_t = (if signed then "int" else "uint") <> size_to_string size <> "_t"


-- | Shift left by the specified amount.
shl :: C.Exp -> C.Exp -> C.Exp
shl l r = [cexp|$exp:l << $exp:r|]


-- | Shift right by the specified amount.
shr :: C.Exp -> C.Exp -> C.Exp
shr l r = [cexp|$exp:l >> $exp:r|]

{---- Quasiquoting helpers }}} ----}
