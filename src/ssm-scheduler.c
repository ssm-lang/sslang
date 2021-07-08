#include "ssm-internal.h"

#ifndef SSM_EVENT_QUEUE_SIZE
/** Size of the event queue; override as necessary */
#define SSM_EVENT_QUEUE_SIZE 2048
#endif

#ifndef SSM_ACT_QUEUE_SIZE
/** Size of the activation record queue; override as necessary */
#define SSM_ACT_QUEUE_SIZE 1024
#endif

/** Extra entries at the start of a binary heap */
#define SSM_QUEUE_HEAD 1

/** Queue index type; these are 1-indexed*/
typedef uint16_t q_idx_t;

/**
 * \brief Event queue, used to track and schedule events between instants.
 *
 * Managed as a binary heap sorted by e->later_time
 */
static struct ssm_sv *event_queue[SSM_EVENT_QUEUE_SIZE + SSM_QUEUE_HEAD];
static q_idx_t event_queue_len = 0;

/**
 * \brief ctivation record queue, used to track and schedule
 * continuations at each instant.
 *
 * Managed as a binary heap sorted by a->priority
 */
static struct ssm_act *act_queue[SSM_ACT_QUEUE_SIZE + SSM_QUEUE_HEAD];
static q_idx_t act_queue_len = 0;

/**
 * Note that this starts out uninitialized. It is the responsibility of the
 * runtime to do so.
 */
static ssm_time_t now;

void ssm_activate(struct ssm_act *act)
{
  assert(act);
  if (act->scheduled) return; // Don't activate an already activated routine

  ssm_priority_t priority = act->priority;

  q_idx_t hole = ++act_queue_len;

  if (act_queue_len == SSM_ACT_QUEUE_SIZE) SSM_FAIL("ssm_activate");

  // Copy parent to child until we find where we can put the new one
  for ( ; hole > SSM_QUEUE_HEAD && priority < act_queue[hole >> 1]->priority ;
	hole >>= 1 )
    act_queue[hole] = act_queue[hole >> 1];
  act_queue[hole] = act;
  act->scheduled = true;
}

ssm_time_t next_event_time() {
  return event_queue_len ?
    event_queue[SSM_QUEUE_HEAD]->later_time :
    SSM_NEVER;
}

void ssm_tick()
{
  /* Update every variable in the event queue */
  
  while (event_queue_len > 0 &&
	 event_queue[SSM_QUEUE_HEAD]->later_time == now) {
    
    struct ssm_sv *sv = event_queue[SSM_QUEUE_HEAD];
    (*sv->update)(sv);  // Update the scheduled variable
    sv->last_updated = now;
    sv->later_time = SSM_NEVER;

    /* Schedule all sensitive triggers */
    for (struct ssm_trigger *trigger = sv->triggers ; trigger ;
	 trigger = trigger->next)
      ssm_activate(trigger->act);

    /* Remove the top event from the queue by inserting the last
       element in the queue at the front and percolating it down */
    struct ssm_sv *to_insert = event_queue[event_queue_len--]; // get last
    ssm_time_t then = to_insert->later_time;

    q_idx_t parent = 1;
    for (;;) {
      q_idx_t child = parent << 1; // Left child
      if ( child > event_queue_len) break;
      if ( child + 1 <= event_queue_len &&
	   event_queue[child+1]->later_time < event_queue[child]->later_time )
	child++; // Right child is earlier
      if (then < event_queue[child]->later_time) break; // to_insert is earlier
      event_queue[parent] = event_queue[child];
      parent = child;
    }
    event_queue[parent] = to_insert;
    /* End of remove-the-top-element from the event queue */
  }

  while (act_queue_len > 0) {
    struct ssm_act *to_run = act_queue[SSM_QUEUE_HEAD];
    to_run->scheduled = false;

    /* Remove the top activation record from the queue by inserting the
       last element in the queue at the from and percolating it down */
    struct ssm_act *to_insert = act_queue[act_queue_len--];
    ssm_priority_t priority = to_insert->priority;

    q_idx_t parent = 1;
    for (;;) {
      q_idx_t child = parent << 1; // Left child
      if (child > act_queue_len) break;
      if (child +1 <= act_queue_len &&
	  act_queue[child+1]->priority < act_queue[child]->priority)
	child++;
      if (priority < act_queue[child]->priority) break;
      act_queue[parent] = act_queue[child];
      parent = child;
    }
    act_queue[parent] = to_insert;
    /* End of remove-the-top-element from the activation record queue */

    to_run->step(to_run); // Execute the step function
  }
}

/*
 * Look for John's ssm-test-queue or something in his test/ directory:
 * test harness for the binary heaps
 */

