#include "ssm.h"

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
 *
 * Since the first element (QUEUE_HEAD) is unusued,
 * event_queue[event_queue_len] is the last element in the queue
 * provided event_queue_len is non-zero
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
static ssm_time_t now = 0L;

bool ssm_event_on(struct ssm_sv *var)
{
  assert(var);
  return var->last_updated == now;
}

void ssm_sensitize(struct ssm_sv *var, struct ssm_trigger *trigger)
{
  assert(var);
  assert(trigger);

  /* Point us to the first element */
  trigger->next = var->triggers;

  if (var->triggers)
    /* Make first element point to us */
    var->triggers->prev_ptr = &trigger->next;

  /* Insert us at the beginning */
  var->triggers = trigger;

  /* Our previous is the variable */
  trigger->prev_ptr = &var->triggers;
}

void ssm_desensitize(struct ssm_trigger *trigger)
{
  assert(trigger);
  assert(trigger->prev_ptr);

  /* Tell predecessor to skip us */
  *trigger->prev_ptr = trigger->next;

  if (trigger->next)
    /* Tell successor its predecessor is our predecessor */
    trigger->next->prev_ptr = trigger->prev_ptr;
} 

void ssm_trigger(struct ssm_sv *var, ssm_priority_t priority)
{
  assert(var);
  for (struct ssm_trigger *trig = var->triggers ; trig ; trig = trig->next)
    if (trig->act->priority > priority)
      ssm_activate(trig->act);       
}

void ssm_activate(struct ssm_act *act)
{
  assert(act);
  if (act->scheduled) return; // Don't activate an already activated routine

  ssm_priority_t priority = act->priority;

  q_idx_t hole = ++act_queue_len;

  if (act_queue_len > SSM_ACT_QUEUE_SIZE)
    SSM_RESOURCES_EXHAUSTED("ssm_activate");

  // Walk toward the root of the tree, copying parent to child until we find
  // where we can put the activation record
  for ( ; hole > SSM_QUEUE_HEAD && priority < act_queue[hole >> 1]->priority ;
	hole >>= 1 )
    act_queue[hole] = act_queue[hole >> 1];
  act_queue[hole] = act;
  act->scheduled = true;
}

ssm_time_t ssm_next_event_time() {
  return event_queue_len ?
    event_queue[SSM_QUEUE_HEAD]->later_time :
    SSM_NEVER;
}

ssm_time_t ssm_now() { return now; }

/** \brief Determine the index of an already scheduled event
 *
 * This is an inefficient linear search, so it's not meant to be
 * used often.  This is in preparation for either removing or rescheduling
 * an event in the queue.  The function should only be called
 * on a variable that is in the queue.
 *
 */
static q_idx_t find_queued_event(struct ssm_sv *var /**< must be non-NULL */)
{
  assert(var); // Should be a real variable
  assert(var->later_time != SSM_NEVER); // Should be in the queue
  for (q_idx_t i = SSM_QUEUE_HEAD ; i <= event_queue_len ; i++)
    if (event_queue[i] == var) return i;
  assert(0); // Should have found the variable if it was marked as queued
  return 0;
}

void ssm_initialize(struct ssm_sv *var, void (*update)(struct ssm_sv *))
{
  assert(var);
  *var = (struct ssm_sv){
    .update = update,
    .triggers = 0,
    .later_time = SSM_NEVER,
    .last_updated = now
  };
}

void ssm_schedule(struct ssm_sv *var, ssm_time_t later)
{
  assert(var);        // A real variable
  assert(later > now); // Must be in the future

  if (var->later_time == SSM_NEVER) {
    // Variable does not have a pending event: add it to the queue
    q_idx_t hole = ++event_queue_len;
    if (event_queue_len > SSM_EVENT_QUEUE_SIZE)
      SSM_RESOURCES_EXHAUSTED("ssm_schedule");

    // Walk toward the root of the tree, copying parent to child until we find
    // where we can put the activation record
    for ( ; hole > SSM_QUEUE_HEAD &&
	    later < event_queue[hole >> 1]->later_time ; hole >>= 1 )
      event_queue[hole] = event_queue[hole >> 1];
    event_queue[hole] = var;
    var->later_time = later;
    
  } else {
    // Variable has a pending event: reposition the event in the queue
    // as appropriate

    q_idx_t hole = find_queued_event(var);

    if (hole == SSM_QUEUE_HEAD || event_queue[hole >> 1]->later_time < later) {
      // We are at the head of the queue or our parent's event is
      // earlier; percolate us toward the leaves
      for (;;) {
	q_idx_t child = hole << 1;	
	if (child > SSM_EVENT_QUEUE_SIZE) break;
	if (child + 1 <= SSM_EVENT_QUEUE_SIZE &&
	    event_queue[child]->later_time >
	    event_queue[child+1]->later_time)
	  child++; // right child is earlier
	if (event_queue[child]->later_time < later)
	  break; // We are earlier than the earlier child, so we can stop here
	
	event_queue[hole] = event_queue[child];
	hole = child;
      }
    } else {
      // While we're farther from the root than we should be, percolate
      // us toward the root
      for ( ; hole > SSM_QUEUE_HEAD &&
	      later < event_queue[hole >> 1]->later_time ; hole >>= 1)
	event_queue[hole] = event_queue[hole >> 1];
    }
    event_queue[hole] = var;
    var->later_time = later;    
  }
}

void ssm_unschedule(struct ssm_sv *var)
{
  assert(var);        // A real variable
  if (var->later_time != SSM_NEVER) {
    q_idx_t hole = find_queued_event(var);
    var->later_time = SSM_NEVER;
    assert(0); // FIXME!   
    // Move the last element of the queue into the hole,
    // then percolate up or down as needed.  Similar to ssm_schedule.
    // Remove the hole from the event queue    
  }
}

void ssm_tick()
{
  // Advance time to the earliest event in the queue
  if (event_queue_len > 0) {
    assert(now < event_queue[SSM_QUEUE_HEAD]->later_time); // No time-traveling!
    now = event_queue[SSM_QUEUE_HEAD]->later_time;
  }
    
  /* Update every variable in the event queue at the current time */
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
    ssm_time_t later = to_insert->later_time;

    q_idx_t parent = 1;
    for (;;) {
      q_idx_t child = parent << 1; // Left child
      if ( child > event_queue_len) break;
      if ( child + 1 <= event_queue_len &&
	   event_queue[child+1]->later_time < event_queue[child]->later_time )
	child++; // Right child is earlier
      if (later < event_queue[child]->later_time) break; // to_insert is earlier
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

