#ifndef _SSM_H
#define _SSM_H

/** \mainpage A Runtime Library for the Sparse Synchronous Model

\section intro_sec Introduction

The operation of this library was first described in

> Stephen A. Edwards and John Hui.
> The Sparse Synchronous Model.
> In Forum on Specification and Design Languages (FDL),
> Kiel, Germany, September 2020.
> http://www.cs.columbia.edu/~sedwards/papers/edwards2020sparse.pdf

\section usage_sec Usage

See [the detailed documentation](@ref all)

\section example Example


    #include "ssm.h"

    typedef struct {
      SSM_ACT_FIELDS;
      ssm_event_t second;
    } main_act_t;

    ssm_stepf_t main_step;

    main_act_t *enter_main(struct ssm_act *parent,
                           ssm_priority_t priority,
                           ssm_depth_t depth) {
      main_act_t * act =
        (main_act_t *) ssm_enter(sizeof(main_act_t),
                                 main_step, parent, priority, depth);
      ssm_initialize_event(&act->second);
    }

*/

#include <stdint.h>   /* For uint16_t, UINT64_MAX etc. */
#include <stdbool.h>  /* For bool, true, false */
#include <stdlib.h>   /* For size_t */
#include <assert.h>
#include <stddef.h>   /* For offsetof */

/** \defgroup all The SSM Runtime
 * \addtogroup all
 * @{
 */

#ifndef SSM_ACT_MALLOC
/** Allocation function for activation records.
 *
 * Given a number of bytes, return a void pointer to the base
 * of newly allocated space or 0 if no additional space is available */
#define SSM_ACT_MALLOC(size) malloc(size)
#endif

#ifndef SSM_ACT_FREE
/** Free function for activation records.
 *
 * The first argument is a pointer to the base of the activation record
 * being freed; the second is the size in bytes of the record being freed.
 */
#define SSM_ACT_FREE(ptr, size) free(ptr)
#endif

/** Underlying exception handler; can be overridden by each platform
 *
 * ssm_throw is declared as a weak symbol, meaning it will be left a null
 * pointer if the linker does not find a definition for this symbol in any
 * object file.
 */
void ssm_throw(int reason, const char *file, int line, const char *func);

/** Invoked when a process must terminate, e.g., when memory or queue space is
 * exhausted. Not expected to return.
 *
 * Argument passed is an ssm_error_t indicating why the failure occurred.
 * Default behavior is to exit with reason as the exit code, but can be
 * overridden by defining ssm_throw.
 */
#define SSM_THROW(reason) ssm_throw(reason, __FILE__, __LINE__, __func__)

/** Error codes, indicating reason for failure.
 *
 * Platforms may extend the list of errors using SSM_PLATFORM_ERROR like this:
 *
 *      enum {
 *        SSM_CUSTOM_ERROR1 = SSM_PLATFORM_ERROR,
 *        // etc.
 *      };
 */
enum ssm_error_t {
  /** Reserved for unforeseen, non-user-facing errors. */
  SSM_INTERNAL_ERROR = 1,
  /** Tried to insert into full activation record queue. */
  SSM_EXHAUSTED_ACT_QUEUE,
  /** Tried to insert into full event queue. */
  SSM_EXHAUSTED_EVENT_QUEUE,
  /** Could not allocate more memory. */
  SSM_EXHAUSTED_MEMORY,
  /** Tried to exceed available recursion depth. */
  SSM_EXHAUSTED_PRIORITY,
  /** Invalid time, e.g., scheduled delayed assignment at an earlier time. */
  SSM_INVALID_TIME,
  /** Start of platform-specific error code range. */
  SSM_PLATFORM_ERROR
};

/** Ticks per nanosecond */
#define SSM_NANOSECOND 1L
/** Ticks per microsecond */
#define SSM_MICROSECOND (SSM_NANOSECOND * 1000L)
/** Ticks per millisecond */
#define SSM_MILLISECOND (SSM_MICROSECOND * 1000L)
/** Ticks per second */
#define SSM_SECOND (SSM_MILLISECOND * 1000L)
/** Ticks per minute */
#define SSM_MINUTE (SSM_SECOND * 60L)
/** Ticks per hour */
#define SSM_HOUR (SSM_MINUTE * 60L)

/** Absolute time; never to overflow. */
typedef uint64_t ssm_time_t;

/** Time indicating something will never happen
 *
 * The value of this must be derived from the type of ssm_time_t
 */
#define SSM_NEVER UINT64_MAX

/** Thread priority.
 *
 *  Lower numbers execute first in an instant
 */
typedef uint32_t ssm_priority_t;

/** The priority for the entry point of an SSM program. */
#define SSM_ROOT_PRIORITY 0

/** Index of least significant bit in a group of priorities
 *
 * This only needs to represent the number of bits in the ssm_priority_t type.
 */
typedef uint8_t ssm_depth_t;

/** The depth at the entry point of an SSM program. */
#define SSM_ROOT_DEPTH (sizeof(ssm_priority_t) * 8)

struct ssm_sv;
struct ssm_trigger;
struct ssm_act;

/** The function that does an instant's work for a routine */
typedef void ssm_stepf_t(struct ssm_act *);

/** Activation record for an SSM routine

    Routine activation record "base class." A struct for a particular
    routine must start with this type but then may be followed by
    routine-specific fields.   See SSM_ACT_FIELDS
*/
typedef struct ssm_act {
  ssm_stepf_t *step;       /**< C function for running this continuation */
  struct ssm_act *caller;  /**< Activation record of caller */
  uint16_t pc;             /**< Stored "program counter" for the function */
  uint16_t children;       /**< Number of running child threads */
  ssm_priority_t priority; /**< Execution priority; lower goes first */
  ssm_depth_t depth;       /**< Index of the LSB in our priority */
  bool scheduled;          /**< True when in the schedule queue */
} ssm_act_t;

/** "Base class" fields for user-defined activation records
 *
 * The same fields as struct ssm_act.
 *
 * Define your own activation record types like this:
 *
 *     typedef struct {
 *        SSM_ACT_FIELDS;
 *        struct ssm_trigger trigger1;
 *        ssm_int8_t mysv;
 *     } myact_t;
 *
 */

#define SSM_ACT_FIELDS     \
  ssm_stepf_t *step;       \
  ssm_act_t *caller;       \
  uint16_t pc;             \
  uint16_t children;       \
  ssm_priority_t priority; \
  ssm_depth_t depth;       \
  bool scheduled          

/**  Indicates a routine should run when a scheduled variable is written
 *
 * Node in linked list of activation records, maintained by each scheduled
 * variable to determine which continuations should be scheduled when the
 * variable is updated.
 */
typedef struct ssm_trigger {
  struct ssm_trigger *next;      /**< Next sensitive trigger, if any */
  struct ssm_trigger **prev_ptr; /**< Pointer to ourself in previous list element */
  ssm_act_t *act;           /**< Routine triggered by this channel variable */
} ssm_trigger_t;


/** A variable that may have scheduled updates and triggers
 *
 * This is the "base class" for other scheduled variable types.
 *
 * On its own, this represents a pure event variable, i.e., a
 * scheduled variable with no data/payload.  The presence of an event on
 * such a variable can be tested with ssm_event_on() as well as awaited
 * with triggers.
 *
 * The update field must point to code that copies the new value of
 * the scheduled variable into its current value.  For pure events,
 * this function may do nothing, but the pointer must be non-zero.
 *
 * This can also be embedded in a wrapper struct/class to implement a scheduled
 * variable with a payload. In this case, the payload should also be embedded
 * in that wrapper class, and the vtable should have update/assign/later
 * methods specialized to be aware of the size and layout of the wrapper class.
 *
 * An invariant:
 * `later_time` != #SSM_NEVER if and only if this variable in the event queue.
 */
typedef struct ssm_sv {
  void (*update)(struct ssm_sv *); /**< Update "virtual method" */
  ssm_trigger_t *triggers;    /**< List of sensitive continuations */
  ssm_time_t later_time;       /**< When the variable should be next updated */
  ssm_time_t last_updated;     /**< When the variable was last updated */
} ssm_sv_t;


/** Indicate writing to a variable should trigger a routine
 *
 * Add a trigger to a variable's list of triggers.
 * When the scheduled variable is written, the scheduler
 * will run the trigger's routine routine.
 *
 * If a routine calls ssm_sensitize() on a trigger, it must call
 * ssm_desensitize() on the trigger if it ever calls ssm_leave() to
 * ensure a terminated routine is never inadvertantly triggered.
 */
extern void ssm_sensitize(ssm_sv_t *, ssm_trigger_t *);

/** Disable a sensitized routine
 *
 * Remove the trigger from its variable.  Only call this on
 * a previously-sensitized trigger.
 */
extern void ssm_desensitize(ssm_trigger_t *);

/** Schedule a routine to run in the current instant
 *
 * Enter the given activation record into the queue of activation
 * records.  This is idempotent: it may be called multiple times on
 * the same activation record within an instant; only the first call
 * has any effect.
 *
 * Invokes #SSM_RESOURCES_EXHAUSTED("ssm_activate") if the activation record queue is full.
 */
extern void ssm_activate(ssm_act_t *);

/**
 * Execute a routine immediately.
 */
static inline void ssm_call(ssm_act_t *act) { (*(act->step))(act); }

/** Enter a routine
 *
 * Enter a function: allocate the activation record by invoking
 * #SSM_ACT_MALLOC, set up the function and
 * program counter value, and remember the caller.
 *
 * Invokes #SSM_RESOURCES_EXHAUSTED("ssm_enter") if allocation fails.
 *
 */
static inline ssm_act_t *ssm_enter(size_t bytes, /**< size of the activation record, >0 */
				   ssm_stepf_t *step, /**< Pointer to "step" function, non-NULL */
				   ssm_act_t *parent, /**< Activation record of caller, non-NULL */
				   ssm_priority_t priority, /**< Priority: must be no less than parent's */
				   ssm_depth_t depth /**< Depth; used if this routine has children */
							     ) {
  assert(bytes > 0);
  assert(step);
  assert(parent);
  ++parent->children;
  ssm_act_t *act = (ssm_act_t *)SSM_ACT_MALLOC(bytes);
  if (!act) SSM_THROW(SSM_EXHAUSTED_MEMORY);
  *act = (ssm_act_t){
      .step = step,
      .caller = parent,
      .pc = 0,
      .children = 0,
      .priority = priority,
      .depth = depth,
      .scheduled = false,
  };
  return act;
}

/**
 * Deallocate an activation record; return to caller if we were the last child.
 */
static inline void ssm_leave(ssm_act_t *act, size_t bytes) {
  assert(act);
  assert(act->caller);
  assert(act->caller->step);
  ssm_act_t *caller = act->caller;
  SSM_ACT_FREE(act, bytes); /* Free the whole activation record, not just the start */
  if ((--caller->children) == 0)
    ssm_call(caller); /* If we were the last child, run our parent */
}

/** Return true if there is an event on the given variable in the current instant
 */
bool ssm_event_on(ssm_sv_t *var /**< Variable: must be non-NULL */ );

/** Initialize a scheduled variable
 *
 * Call this to initialize the contents of a newly allocated scheduled
 * variable, e.g., after ssm_enter()
 */
void ssm_initialize(ssm_sv_t *var,
		    void (*update)(ssm_sv_t *));

/** Schedule a future update to a variable
 *
 * Add an event to the global event queue for the given variable,
 * replacing any pending event.
 */
void ssm_schedule(ssm_sv_t *var, /**< Variable to schedule: non-NULL */
		  ssm_time_t later
		  /**< Event time; must be in the future (greater than #now) */);

/** Unschedule any pending event on a variable
 *
 * If there is a pending event on the given variable, remove the event
 * from the queue.  Nothing happens if the variable does not have a
 * pending event.
 */
void ssm_unschedule(ssm_sv_t *var);

/** Activate routines triggered by a variable
 *
 * Call this when a scheduled variable is assigned in the current instant
 * (i.e., not scheduled)
 *
 * The given priority should be that of the routine doing the update.
 * Instantaneous assignment can only activate lower-priority (i.e., later)
 * routines in the same instant.
 */
void ssm_trigger(ssm_sv_t *var, /**< Variable being assigned */
		 ssm_priority_t priority
		 /**< Priority of the routine doing the assignment. */
		 );

/** Return the time of the next event in the queue or #SSM_NEVER
 *
 * Typically used by the platform code that ultimately invokes ssm_tick().
 */
ssm_time_t ssm_next_event_time(void);

/** Return the current model time */
ssm_time_t ssm_now(void);

/** Run the system for the next scheduled instant
 *
 * Typically run by the platform code, not the SSM program per se.
 *
 * Advance #now to the time of the earliest event in the queue, if any.
 *
 * Remove every event at the head of the event queue scheduled for
 * #now, update the variable's current value by calling its
 * (type-specific) update function, and schedule all the triggers in
 * the activation queue.
 *
 * Remove the activation record in the activation record queue with the
 * lowest priority number and execute its "step" function.
 */
void ssm_tick();

/** Reset the scheduler.
 *
 *  Set now to 0; clear the event and activation record queues.  This
 *  does not need to be called before calling ssm_tick() for the first
 *  time; the global state automatically starts initialized.
 */
void ssm_reset();

/** An activation record for the parent of the topmost routine
 *
 * When you are starting up your SSM system, pass a pointer to this as
 * the parent of your topmost function.  E.g., if `main` is your topmost
 * function and your `enter_main()` function takes its parent as the first
 * argument,
 *
 * ~~~{.c}
 * main_act_t *enter_main(ssm_act_t *, ssm_priority_t, ssm_depth_t);
 * void step_main(ssm_act_t *act);
 *
 * enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH)
 * ~~~
 *
 * Here, `enter_main()` should cause ssm_enter() to be called with
 *
 * ~~~{.c}
 * ssm_enter(sizeof(main_act_t), step_main, &ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH)
 * ~~~
 */
extern ssm_act_t ssm_top_parent;


/**
 * Implementation of container_of that falls back to ISO C99 when GNU C is not
 * available (from https://stackoverflow.com/a/10269925/10497710)
 */
#ifdef __GNUC__
#define member_type(type, member) __typeof__(((type *)0)->member)
#else
#define member_type(type, member) const void
#endif
#define container_of(ptr, type, member)                                        \
  ((type *)((char *)(member_type(type, member) *){ptr} -                       \
            offsetof(type, member)))

typedef struct { ssm_sv_t sv; } ssm_event_t;
#define ssm_later_event(var, then) ssm_schedule(&(var)->sv, (then))
extern void ssm_assign_event(ssm_event_t *var, ssm_priority_t prio);
extern void ssm_initialize_event(ssm_event_t *);


#define SSM_DECLARE_SV_SCALAR(payload_t)                                       \
  typedef struct {                                                             \
    ssm_sv_t sv;                                                          \
    payload_t value;       /* Current value */                                 \
    payload_t later_value; /* Buffered value */                                \
  } ssm_##payload_t##_t;                                                       \
  void ssm_assign_##payload_t(ssm_##payload_t##_t *sv, ssm_priority_t prio,    \
                          const payload_t value);                              \
  void ssm_later_##payload_t(ssm_##payload_t##_t *sv, ssm_time_t then,         \
                         const payload_t value);                               \
  void ssm_initialize_##payload_t(ssm_##payload_t##_t *v);

#define SSM_DEFINE_SV_SCALAR(payload_t)                                        \
  static void ssm_update_##payload_t(ssm_sv_t *sv) {                      \
    ssm_##payload_t##_t *v = container_of(sv, ssm_##payload_t##_t, sv);        \
    v->value = v->later_value;                                                 \
  }                                                                            \
  void ssm_assign_##payload_t(ssm_##payload_t##_t *v, ssm_priority_t prio,     \
                              const payload_t value) {                         \
    v->value = value;					                       \
    v->sv.last_updated = ssm_now();			  		       \
    ssm_trigger(&v->sv, prio);                                                 \
  }                                                                            \
  void ssm_later_##payload_t(ssm_##payload_t##_t *v, ssm_time_t then,          \
                         const payload_t value) {                              \
    v->later_value = value;                                                    \
    ssm_schedule(&v->sv, then);					               \
  }		 	 						       \
  void ssm_initialize_##payload_t(ssm_##payload_t##_t *v) {                    \
    ssm_initialize(&v->sv, ssm_update_##payload_t);	     	               \
  }
 
typedef int8_t   i8;   /**< 8-bit Signed Integer */
typedef int16_t  i16;  /**< 16-bit Signed Integer */
typedef int32_t  i32;  /**< 32-bit Signed Integer */
typedef int64_t  i64;  /**< 64-bit Signed Integer */
typedef uint8_t  u8;   /**< 8-bit Unsigned Integer */
typedef uint16_t u16;  /**< 16-bit Unsigned Integer */
typedef uint32_t u32;  /**< 32-bit Unsigned Integer */
typedef uint64_t u64;  /**< 64-bit Unsigned Integer */

/** \struct ssm_bool_t
    Scheduled Boolean variable */
/** \struct ssm_i8_t
    Scheduled 8-bit Signed Integer variable */
/** \struct ssm_i16_t
    Scheduled 16-bit Signed Integer variable */
/** \struct ssm_i32_t
    Scheduled 32-bit Signed Integer variable */
/** \struct ssm_i64_t
    Scheduled 64-bit Signed Integer variable */
/** \struct ssm_u8_t
    Scheduled 8-bit Unsigned Integer variable */
/** \struct ssm_u16_t
    Scheduled 16-bit Unsigned Integer variable */
/** \struct ssm_u32_t
    Scheduled 32-bit Unsigned Integer variable */
/** \struct ssm_u64_t
    Scheduled 64-bit Unsigned Integer variable */

SSM_DECLARE_SV_SCALAR(bool)
SSM_DECLARE_SV_SCALAR(i8)
SSM_DECLARE_SV_SCALAR(i16)
SSM_DECLARE_SV_SCALAR(i32)
SSM_DECLARE_SV_SCALAR(i64)
SSM_DECLARE_SV_SCALAR(u8)
SSM_DECLARE_SV_SCALAR(u16)
SSM_DECLARE_SV_SCALAR(u32)
SSM_DECLARE_SV_SCALAR(u64)

/** @} */

#endif
