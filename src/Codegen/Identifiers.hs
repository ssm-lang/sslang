-- | Identifiers in generated C code and helper functions for constructing them.
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Identifiers where

import           Language.C.Quote               ( ToIdent(..) )
import           Language.C.Quote.GCC           ( cty )
import qualified Language.C.Syntax             as C

import           Common.Identifiers             ( Identifiable(..)
                                                , Identifier(..)
                                                , VarId(..)
                                                , fromId
                                                )

import           Data.String                    ( IsString(..) )

-- | Use snake_case for c literals
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- | Identifiers in C.
newtype CIdent = CIdent Identifier
  deriving Eq
  deriving IsString via Identifier
  deriving Identifiable via Identifier
  deriving ToIdent via Identifier
  deriving Semigroup via Identifier
  deriving Monoid via Identifier

-- | Construct a type name from a C identifier.
ctype :: CIdent -> C.Type
ctype i = [cty|typename $id:i|]

{----- libssm Types -----}

-- | C type that represents model time
time_t :: C.Type
time_t = [cty|typename ssm_time_t|]

-- | C type that represents process priorities
priority_t :: C.Type
priority_t = [cty|typename ssm_priority_t|]

-- | C type that represents process depths
depth_t :: C.Type
depth_t = [cty|typename ssm_depth_t|]

-- | C type that represents triggers, aka processes that are sensitized on variables
trigger_t :: C.Type
trigger_t = [cty|struct ssm_trigger|]

-- | C type that represents the step function of a process
stepf_t :: C.Type
stepf_t = [cty|typename ssm_stepf_t|]

-- | C type that represents booleans
bool_t :: C.Type
bool_t = [cty|typename bool|]

-- | The type of the activation record base class.
act_t :: C.Type
act_t = [cty|struct ssm_act|]

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

-- | The conventional name of the payload field in a type-specialized SV.
value :: CIdent
value = "value"

{- libssm Symbols -}

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

root_priority :: CIdent
root_priority = "SSM_ROOT_PRIORITY"

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

tmp_ :: Int -> CIdent
tmp_ i = "__tmp_" <> fromString (show i)

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

-- | Name of the scheduled variable type for an SSM `Type`.
sv_ :: Identifiable a => a -> CIdent
sv_ ty = "ssm_" <> fromId ty <> "_t"

-- | Name of the initialize method for an SSM `Type`.
initialize_ :: Identifiable a => a -> CIdent
initialize_ ty = "ssm_initialize_" <> fromId ty

-- | Name of the assign method for an SSM `Type`.
assign_ :: Identifiable a => a -> CIdent
assign_ ty = "ssm_assign_" <> fromId ty

-- | Name of the later method for an SSM `Type`.
later_ :: Identifiable a => a -> CIdent
later_ ty = "ssm_later_" <> fromId ty

-- | Name of the generic ssm_sv_t embedded inside of a scheduled variable type.
sv :: CIdent
sv = "sv"
