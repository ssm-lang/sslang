#ifndef _SSM_H
#define _SSM_H

/* Assumes C99 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

/* A year is 31,536,000 seconds, which fits in 25 bits
   There are 1,000,000 microseconds in a second, which fits in 20 bits
   If we count microseconds, we only have 12 bits left in 32 bits,
   enough for 4096 seconds, about 68 minutes or a little more than an hour.
   64 bits of microseconds gives 19 bits of years, over 500ky: plenty.
 */
typedef uint64_t ssm_time_t;         // timestamps: assumed never to overflow
#define NO_EVENT_SCHEDULED ULONG_MAX
#define TICKS_PER_SECOND 1000000     // ssm_time_t counts microseconds

extern ssm_time_t now;               // Name of current instant

// Thread priority types
typedef uint32_t priority_t; // 32-bit thread priority
typedef uint8_t  depth_t;   // Index of least significant priority bit
#define PRIORITY_AT_ROOT 0
#define DEPTH_AT_ROOT 32


// Routine activation records

typedef struct rar rar_t;      // Routine activation record "Base class"
typedef void stepf_t(rar_t *); // Type of a step function

#define ACTIVATION_RECORD_FIELDS \
  stepf_t *step;            /* C function for running this continuation */ \
  struct rar *caller;       /* Activation record of caller */\
  uint16_t pc;	            /* Stored "program counter" for the function */\
  uint16_t children;        /* Number of running child threads */\
  priority_t priority;	    /* Execution priority */\
  depth_t depth;  /* Index of the LSB in our priority */\
  bool scheduled            /* True when in the schedule queue */
  
struct rar {  // Start of every function activation record
  ACTIVATION_RECORD_FIELDS;
};

// Enter a function: allocate the activation record, set up the
// function and program counter value, and remember the caller
inline rar_t *
enter(size_t bytes, stepf_t *step, rar_t *parent,
      priority_t priority, depth_t depth)
{
  assert(bytes > 0);
  assert(step);
  assert(parent);
  ++parent->children; // Add ourself as a child
  rar_t *rar = malloc(bytes);
  *rar = (rar_t) { .step = step,
		   .caller = parent,
		   .pc = 0,
		   .children = 0,
		   .priority = priority,
		   .depth = depth,
		   .scheduled = false };
  return rar;
}

// Execute a routine immediately
inline void call(rar_t *rar) { (*(rar->step))(rar); }

// Schedule a routine for the current instant
extern void fork(rar_t *);

// Deallocate an activation record; return to caller if we were the last child
inline void leave(rar_t *rar, size_t bytes)
{
  assert(rar);
  assert(rar->caller);
  assert(rar->caller->step);
  rar_t *caller = rar->caller;
  free(rar);   // Free the whole activation record, not just the start
  if ((--caller->children) == 0) // Were we the last child?
    call(caller);                // If so, run our parent
}


// Channel variables

typedef struct cv cv_t;

#define CHANNEL_VARIABLE_FIELDS \
  void (*update)(cv_t *);   /* Function to update this particular type       */\
  struct trigger *triggers; /* Doubly-linked list of sensitive continuations */\
  ssm_time_t last_updated;  /* Time of last update, for detecting events     */\
  ssm_time_t event_time     /* Time at which the variable should be updated  */

// "Base class" for channel variable
struct cv {
    CHANNEL_VARIABLE_FIELDS;
};

// Was there an event on the given variable in the current instant?
inline bool event_on(cv_t *var) { return var->last_updated == now; }

// Event channel variables (valueless, pure events)
typedef cv_t cv_event_t;
extern void initialize_event(cv_event_t *);
extern void assign_event(cv_event_t *, priority_t);
extern void later_event(cv_t *, ssm_time_t);

// Int channel variables
typedef struct {
  CHANNEL_VARIABLE_FIELDS;
  
  int value;                // Current value
  int event_value;          // Value to be assigned at event_time
} cv_int_t;

extern void initialize_int(cv_int_t *, int);
extern void assign_int(cv_int_t *,  priority_t, int);
extern void later_int(cv_int_t *, ssm_time_t, int);

// Bool channel variables
typedef struct {
  CHANNEL_VARIABLE_FIELDS;
  
  bool value;                // Current value
  bool event_value;          // Value to be assigned at event_time
} cv_bool_t;

extern void initialize_bool(cv_bool_t *, bool);
extern void assign_bool(cv_bool_t *,  priority_t, bool);
extern void later_bool(cv_bool_t *, ssm_time_t, bool);

// A trigger: indicates that a write to a channel variable
// should schedule a routine to run in the current instant
// Each function needs at least one of these for every variable
// mentioned at the most complex "await" point
typedef struct trigger trigger_t;
struct trigger {
  rar_t *rar;             // Routine triggered by this channel variable

  trigger_t *next;       // Next trigger sensitive to this variable, if any
  trigger_t **prev_ptr;  // Pointer to ourself in previous list element
};


// Event queue: variable updates scheduled for the future
typedef uint16_t event_queue_index_t; // Number in the queue/highest index
extern event_queue_index_t event_queue_len;
extern cv_t *event_queue[];

extern void sensitize(cv_t *, trigger_t *); // Add a trigger to a variable
extern void desensitize(trigger_t *);      // Remove a trigger from its variable


// Continuation queue: scheduled for current instant
typedef uint16_t cont_queue_index_t;  // Number in the queue/highest index
extern cont_queue_index_t cont_queue_len;
extern rar_t *cont_queue[];

extern void tick(); // Execute the system for the current instant

#endif /* _SSM_H */
