#ifndef _SSM_ACT_H
#define _SSM_ACT_H

/**
 * The interface shared between (generated) routine implementations and
 * ssm-sched.c. All function prototypes here are implemented in ssm-sched.c.
 *
 * Note that the act_enter and act_leave functions use malloc and free. These
 * should be reimplemented using a custom allocator more suitable for realtime
 * applications.
 */

#include <ssm-core.h>
#include <ssm-debug.h>

/** Type of a step function */
typedef void stepf_t(struct act *);

/** Routine activation record "base class" */
struct act {
  stepf_t *step;       /* C function for running this continuation */
  struct act *caller;  /* Activation record of caller */
  uint16_t pc;         /* Stored "program counter" for the function */
  uint16_t children;   /* Number of running child threads */
  priority_t priority; /* Execution priority */
  depth_t depth;       /* Index of the LSB in our priority */
  bool scheduled;      /* True when in the schedule queue */
#ifdef DEBUG
  struct debug_act debug;
#endif
};

/**
 * Node in linked list of activation records, maintained by each scheduled
 * variable to determine which continuations should be scheduled when the
 * variable is updated.
 */
struct trigger {
  struct trigger *next;      /** Next sensitive trigger, if any */
  struct trigger **prev_ptr; /** Pointer to ourself in previous list element */
  struct act *act;           /** Routine triggered by this channel variable */
};

/**
 * Adds the trigger to the sv, i.e., have the activation record wait on
 * a scheduled variable.
 */
extern void sensitize(struct sv *, struct trigger *);

/**
 * Remove the trigger from its trigger list.
 */
extern void desensitize(struct trigger *);

/**
 * Add activation record to run queue, used to fork one child routine.
 */
extern void act_fork(struct act *);

/***
 * Inline functions
 *
 * Used only by generated code (so consider moving to separate header file).
 */

/**
 * Execute a routine immediately.
 */
static inline void act_call(struct act *act) { (*(act->step))(act); }

/**
 * Enter a function: allocate the activation record, set up the function and
 * program counter value, and remember the caller.
 */
static inline struct act *act_enter(size_t bytes, stepf_t *step,
                                    struct act *parent, priority_t priority,
                                    depth_t depth) {
  assert(bytes > 0);
  assert(step);
  assert(parent);
  ++parent->children;
  struct act *act = (struct act *)malloc(bytes);
  *act = (struct act){
      .step = step,
      .caller = parent,
      .pc = 0,
      .children = 0,
      .priority = priority,
      .depth = depth,
      .scheduled = false,
  };
  DEBUG_ACT_SET_ACT_NAME(act->debug, "(unknown act name)");
  return act;
}

/**
 * Deallocate an activation record; return to caller if we were the last child.
 */
static inline void act_leave(struct act *act, size_t bytes) {
  assert(act);
  assert(act->caller);
  assert(act->caller->step);
  struct act *caller = act->caller;
  free(act); /* Free the whole activation record, not just the start */
  if ((--caller->children) == 0)
    act_call(caller); /* If we were the last child, run our parent */
}

#endif /* _SSM_ACT_H */
