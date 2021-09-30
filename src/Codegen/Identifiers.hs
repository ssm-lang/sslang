-- | Identifiers in generated C code and helper functions for constructing them.
{-# LANGUAGE QuasiQuotes #-}
module Codegen.Identifiers
  ( -- * Type aliases
    CIdent

      -- * Identifiers recognized by the C runtime system.
  , initialize_program
  , initialize_static_input_device
  , initialize_static_output_device
  , top_return
  , top_parent
  , activate
  , act_enter
  , act_leave
  , event_on
  , sensitize
  , desensitize
  , unsched_event
  , throw
  , exhausted_priority
  , now
  , never

      -- * Type names recognized by the the C runtime system.
  , time_t
  , trigger_t
  , priority_t
  , depth_t
  , stepf_t
  , act_t
  , sv_t
      {- | These are (a subset of the) types that the runtime system includes and uses
      internally. These are just the ones that the code generator needs to talk about. -}
  , bool_t

      -- * Constructing Identifiers from strings
      -- | These functions create identifiers from some known [pre|suf]fix.
  , act_
  , step_
  , enter_
  , trig_
  , pc
  , depth
  , priority

      -- * Constructing SSM time macros from SSMTimeUnit
  --, units_

      -- * Constructing Identifiers from types
      {- | Some identifiers need to be prefixed or suffixed with some type information,
      such as @later_int@, @later_bool@ etc. We create identifiers like these and others
      by using these functions. -}
  , sv_
  , initialize_
  , assign_
  , later_
  , value
  , root_priority
  , root_depth

    -- * Debug-/trace-specific macros
  , debug_microtick
  , debug_trace

    -- * Accessing references
    {- | These functions help the code generator compile a reference into a
    C-expression that references the same reference. -}
  -- , refPtr
  -- , refVal
  -- , refSV
  ) where

import           Language.C.Quote.GCC           ( cty )
import qualified Language.C.Syntax             as C

import           Common.Identifiers             ( VarId
                                                , ident
                                                )

-- | Use snake_case for c literals
{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Type alias for C identifiers.
type CIdent = String

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
act_enter :: CIdent
act_enter = "ssm_enter"

-- | Name of routine that deallocates an activation record
act_leave :: CIdent
act_leave = "ssm_leave"

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

-- | Name of macro that throws an error.
throw :: CIdent
throw = "SSM_THROW"

-- | Exhausted priority
exhausted_priority :: CIdent
exhausted_priority = "SSM_EXHAUSTED_PRIORITY"

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

{---- Activation record identifiers ----}

-- | The type of the activation record base class.
act_t :: C.Type
act_t = [cty|struct ssm_act|]

-- | Obtain the name of the activation record struct for a routine.
act_ :: CIdent -> CIdent
act_ routineName = "act_" ++ routineName ++ "_t"

-- | Obtain the name of the step function of a routine.
step_ :: CIdent -> CIdent
step_ routineName = "step_" ++ routineName

-- | Obtain the name for the enter function of a routine.
enter_ :: CIdent -> CIdent
enter_ routineName = "enter_" ++ routineName

-- | Obtain the name of each trigger for a routine.
trig_ :: Int -> CIdent
trig_ i = "trig" ++ show i

pc :: CIdent
pc = "pc"

depth :: CIdent
depth = "depth"

priority :: CIdent
priority = "priority"

{---- Type identifiers ----}

-- | The type of the scheduled variable base class.
sv_t :: C.Type
sv_t = [cty|struct sv|]

-- | Obtain the name of the scheduled variable type for an SSM `Type`.
sv_ :: CIdent -> CIdent
sv_ ty = "ssm_" ++ ty ++ "_t"

-- | Obtain the name of the initialize method for an SSM `Type`.
initialize_ :: CIdent -> CIdent
initialize_ ty = "ssm_initialize_" ++ ty

-- | Obtain the name of the assign method for an SSM `Type`.
assign_ :: CIdent -> CIdent
assign_ ty = "ssm_assign_" ++ ty

-- | Obtain the name of the later method for an SSM `Type`.
later_ :: CIdent -> CIdent
later_ ty = "ssm_later_" ++ ty

-- | Name of the field in an SV that holds the payload.
value :: CIdent
value = "value"

debug_microtick :: CIdent
debug_microtick = "SSM_DEBUG_MICROTICK"

debug_trace :: CIdent
debug_trace = "SSM_DEBUG_TRACE"

root_priority :: CIdent
root_priority = "SSM_ROOT_PRIORITY"

root_depth :: CIdent
root_depth = "SSM_ROOT_DEPTH"
