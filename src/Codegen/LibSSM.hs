{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Bindings to the ssm-runtime library.
module Codegen.LibSSM where

import           Common.Identifiers             ( Identifiable(..)
                                                , Identifier(..)
                                                , VarId(..)
                                                , fromId
                                                )
import           Data.String                    ( IsString(..) )
import           Language.C.Quote               ( Id(Id)
                                                , ToIdent(..)
                                                )
import           Language.C.Quote.GCC           ( cexp
                                                , cty
                                                )
import qualified Language.C.Syntax             as C

-- Allow snake_case for c literals
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | Identifiers in C.
newtype CIdent = CIdent Identifier
  deriving (Eq)
  deriving (Ord)
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

ccall :: C.Exp -> [C.Exp] -> C.Exp
fn `ccall` args = [cexp|$exp:fn($args:args)|]

amp :: C.Exp -> C.Exp
amp e = [cexp|&$exp:e|]

star :: C.Exp -> C.Exp
star e = [cexp|*$exp:e|]

{---- from ssm.h {{{ ----}

-- | @enum ssm_error@, an enumeration of possible runtime errors.
data SSMError
  = INTERNAL_ERROR        -- ^ Reserved for unforeseen, non-user-facing errors.
  | EXHAUSTED_ACT_QUEUE   -- ^ Tried to insert into full ready queue.
  | EXHAUSTED_EVENT_QUEUE -- ^ Tried to insert into full event queue.
  | EXHAUSTED_MEMORY      -- ^ Could not allocate more memory.
  | EXHAUSTED_PRIORITY    -- ^ Tried to exceed available recursion depth.
  | NOT_READY             -- ^ Not yet ready to perform the requested action.
  | INVALID_TIME          -- ^ Specified invalid time.
  | INVALID_MEMORY        -- ^ Invalid memory layout.
  deriving Show

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
marshal v = [cexp|ssm_marshal($exp:v)|]

-- | @ssm_unmarshal@, extract 31-bit integral value out of a 'value_t'.
unmarshal :: C.Exp -> C.Exp
unmarshal v = [cexp|ssm_unmarshal($exp:v)|]

-- | @ssm_on_heap@, whether a 'value_t' points to something on the heap.
on_heap :: C.Exp -> C.Exp
on_heap v = [cexp|ssm_on_heap($exp:v)|]

-- | @ssm_dup@, increment the reference count of a value.
dup :: C.Exp -> C.Exp
dup v = [cexp|ssm_dup($exp:v)|]

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
new_adt :: Int -> Int -> C.Exp
new_adt val_count tag = [cexp|ssm_new_adt($uint:val_count, $uint:tag)|]

-- | @ssm_adt_field@, access the @i@th field of an ADT object. Assignable.
adt_field :: C.Exp -> Int -> C.Exp
adt_field v i = [cexp|ssm_adt_field($exp:v, $uint:i)|]

-- | @ssm_tag@, extract the tag of an ADT value.
adt_tag :: C.Exp -> C.Exp
adt_tag v = [cexp|ssm_tag($exp:v)|]

-- | Name of the pseudonymous macro from the Linux kernel.
container_of :: CIdent
container_of = "container_of"

-- | Name of top level program initialization function
initialize_program :: CIdent
initialize_program = "ssm_program_initialize"

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

-- | Obtain the name of the step function of a routine.
step_ :: VarId -> CIdent
step_ name = "step_" <> fromId name

-- | Obtain the name for the enter function of a routine.
enter_ :: VarId -> CIdent
enter_ name = "enter_" <> fromId name

-- | Obtain the name of each trigger for a routine.
trig_ :: Int -> CIdent
trig_ i = "__trig_" <> fromString (show i)

-- | Obtain the name of a temporary variable.
tmp_ :: Int -> CIdent
tmp_ i = "__tmp_" <> fromString (show i)

-- | Obtain the name of an argument variable.
arg_ :: Int -> CIdent
arg_ i = "__arg_" <> fromString (show i)

-- | Name of return argument.
ret_val :: CIdent
ret_val = "__return_val"

-- | Identifier for generic (inner) struct act.
actg :: CIdent
actg = "actg"

-- | Identifier for specialized (outer) struct act.
acts :: CIdent
acts = "acts"

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

leave_label :: CIdent
leave_label = "__leave_step"

{---- Naming conventions }}} ----}

  {-
{----- libssm Types -----}
-- | Name of the program counter field in an 'act_t'.
pc :: CIdent
pc = "pc"

-- | Name of the caller field in an 'act_t'.
caller :: CIdent
caller = "caller"

-- | Name of the depth field in an 'act_t'.
depth :: CIdent
depth = "depth"

-- | Name of the priority field in an 'act_t'.
priority :: CIdent
priority = "priority"

-- | The type of the scheduled variable base class.
sv_t :: C.Type
sv_t = [cty|struct sv|]

-- | The word size as type uint32_t
word_t :: C.Type
word_t = [cty|typename uint32_t|]

-- | The conventional name of the payload field in a type-specialized SV.
value :: CIdent
value = "value"

{- libssm Symbols -}

-- | Name of the pseudonymous macro from the Linux kernel.
container_of :: CIdent
container_of = "container_of"

-- | Name of top level program initialization function
initialize_program :: CIdent
initialize_program = "ssm_program_initialize"

-- | Name of the top level static input switch initialization function
initialize_static_input_device :: CIdent
initialize_static_input_device = "initialize_static_input_switch"

-- | Name of the top level static output initialization function
initialize_static_output_device :: CIdent
initialize_static_output_device = "bind_static_output_device"

-- | Name of top level return step-function
top_return :: CIdent
top_return = "top_return"

-- | Name of top level parent activation record
top_parent :: CIdent
top_parent = "ssm_top_parent"

-- | Name of routine that forks procedures
activate :: CIdent
activate = "ssm_activate"

-- | Name of routine that initialized an activation record
enter :: CIdent
enter = "ssm_enter"

-- | Name of routine that deallocates an activation record
leave :: CIdent
leave = "ssm_leave"

-- | Name of routine that checks if a reference has been written to
event_on :: CIdent
event_on = "ssm_event_on"

-- | Name of routine that sensitizes a procedure
sensitize :: CIdent
sensitize = "ssm_sensitize"

-- | Name of routine that desensitizes a procedure
desensitize :: CIdent
desensitize = "ssm_desensitize"

-- | Name of routine that dequeues an event from the event queue
unsched_event :: CIdent
unsched_event = "ssm_unschedule"

-- | Name of routine that returns the current value of now.
now :: CIdent
now = "ssm_now"

-- | Name of routine that returns the current value of now.
never :: CIdent
never = "SSM_NEVER"

-- | Priority of the root SSM routine.
root_priority :: CIdent
root_priority = "SSM_ROOT_PRIORITY"

-- | Depth of the root SSM routine.
root_depth :: CIdent
root_depth = "SSM_ROOT_DEPTH"

-- | Name of macro that throws an error.
throw :: CIdent
throw = "SSM_THROW"

-- | Exhausted priority
exhausted_priority :: CIdent
exhausted_priority = "SSM_EXHAUSTED_PRIORITY"

-- | Insert debug trace
debug_trace :: CIdent
debug_trace = "SSM_DEBUG_TRACE"

-- | Used for inserting
debug_microtick :: CIdent
debug_microtick = "SSM_DEBUG_MICROTICK"

{---- Activation record identifiers ----}

-- | Obtain the name of the activation record struct for a routine.
act_ :: VarId -> CIdent
act_ routineName = "act_" <> fromId routineName <> "_t"

-- | Obtain the name of the step function of a routine.
step_ :: VarId -> CIdent
step_ routineName = "step_" <> fromId routineName

-- | Obtain the name for the enter function of a routine.
enter_ :: VarId -> CIdent
enter_ routineName = "enter_" <> fromId routineName

-- | Obtain the name of each trigger for a routine.
trig_ :: Int -> CIdent
trig_ i = "__trig_" <> fromString (show i)

-- | Obtain the name of a temporary variable.
tmp_ :: Int -> CIdent
tmp_ i = "__tmp_" <> fromString (show i)

-- | Obtain the name of an argument variable.
arg_ :: Int -> CIdent
arg_ i = "__arg_" <> fromString (show i)

-- | Label in the step function where cleanup is performed before leaving.
leave_cleanup :: CIdent
leave_cleanup = "__leave_cleanup"

-- | Identifier for specialized (outer) struct act.
acts :: CIdent
acts = "acts"

-- | Identifier for generic (inner) struct act.
actg :: CIdent
actg = "actg"

-- | Identifier for act member in act struct.
act_member :: CIdent
act_member = "act"

-- | Name of return argument.
ret_val :: CIdent
ret_val = "__return_val"

{---- Type identifiers ----}

-- | Name of the scheduled variable type for an SSM Type.
sv_ :: Identifiable a => a -> CIdent
sv_ ty = "ssm_" <> fromId ty <> "_t"

-- | Name of the initialize method for an SSM Type.
initialize_ :: Identifiable a => a -> CIdent
initialize_ ty = "ssm_initialize_" <> fromId ty

-- | Name of the assign method for an SSM Type.
assign_ :: Identifiable a => a -> CIdent
assign_ ty = "ssm_assign_" <> fromId ty

-- | Name of the later method for an SSM Type.
later_ :: Identifiable a => a -> CIdent
later_ ty = "ssm_later_" <> fromId ty

-- | Name of the generic ssm_sv_t embedded inside of a scheduled variable type.
sv :: CIdent
sv = "sv"

-- | Name of C integer type.
int_ :: Int -> CIdent
int_ s = "i" <> fromString (show s)

{----- Memory management -----}

-- | Allocate memory.
mem_alloc :: CIdent
mem_alloc = "malloc"

-- | Free memory.
mem_free :: CIdent
mem_free = "free"

-- | Call to allocate memory for an ADT
ssm_new :: CIdent
ssm_new = "ssm_new"

{----- Algebraic Data Types -----}

-- | Type of a generic ssm value (either pointer or integer)
ssm_value_t :: C.Type
ssm_value_t = [cty|typename ssm_value_t|]

-- | Name of an ssm values's pointer field
heap_ptr :: CIdent
heap_ptr = "heap_ptr"

-- | Name of an ssm values's integer field
packed_val :: CIdent
packed_val = "packed_val"

-- | Type of a generic ADT on the heap
ssm_object :: C.Type
ssm_object = [cty|struct ssm_object_t|]

-- | Type of an ADT's memory management header
ssm_mm_md :: C.Type
ssm_mm_md = [cty|struct ssm_mm|]

-- | Name of an ADT's memory management header
mm :: CIdent
mm = "mm"

-- | Name of an ADT's payload
payload :: CIdent
payload = "payload"

-- | Construct an ssm_value_t from an ssm_object*
ssm_from_obj :: C.Exp -> C.Exp
ssm_from_obj o = [cexp|($ty:ssm_value_t) { .$id:packed_val = &($exp:o)->$id:mm }|]

-- | Construct an ssm_value_t from an integer
ssm_from_int :: C.Exp -> C.Exp
ssm_from_int v = [cexp|($ty:ssm_value_t) { .$id:packed_val = $exp:v }|]

-- | Convert an ssm_value_t to an ssm_object*
-- This kind of conversion is needed to access fields in an ADT
ssm_to_obj :: C.Exp -> C.Exp
ssm_to_obj v = [cexp| ($id:container_of(($exp:v).$id:heap_ptr, ssm_object, $id:mm )) |]
-}
