#include "ssm.h"

/** If defined, makes normally hidden internal functions and variables
 * available for linking, allowing whitebox testing
 */
#ifdef SSM_DEBUG
#define SSM_STATIC
#define SSM_STATIC_INLINE
#else
#define SSM_STATIC static
#define SSM_STATIC_INLINE static inline
#endif

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

/** Queue index type; index of 1 is the first element */
typedef uint16_t q_idx_t;

/**
 * \brief Event queue, used to track and schedule events between instants.
 *
 * Managed as a binary heap sorted by later_time
 *
 * Since the first element (QUEUE_HEAD) is unusued,
 * event_queue[event_queue_len] is the last element in the queue
 * provided event_queue_len is non-zero
 */
SSM_STATIC ssm_sv_t *event_queue[SSM_EVENT_QUEUE_SIZE + SSM_QUEUE_HEAD];
SSM_STATIC q_idx_t event_queue_len = 0;

/**
 * \brief ctivation record queue, used to track and schedule
 * continuations at each instant.
 *
 * Managed as a binary heap sorted by a->priority
 */
SSM_STATIC ssm_act_t *act_queue[SSM_ACT_QUEUE_SIZE + SSM_QUEUE_HEAD];
SSM_STATIC q_idx_t act_queue_len = 0;

/**
 * The current model time.  Read with ssm_now(); user programs should not
 * manipuate this directly.
 *
 * This starts out initialized to 0; can be reset by ssm_reset().  ssm_tick()
 * advances it monotonically.
 */
SSM_STATIC ssm_time_t now = 0L;

void ssm_reset()
{
  now = 0L;
  event_queue_len = 0;
  act_queue_len = 0;
}

bool ssm_event_on(ssm_sv_t *var)
{
  assert(var);
  return var->last_updated == now;
}

void ssm_sensitize(ssm_sv_t *var, ssm_trigger_t *trigger)
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

void ssm_desensitize(ssm_trigger_t *trigger)
{
  assert(trigger);
  assert(trigger->prev_ptr);

  /* Tell predecessor to skip us */
  *trigger->prev_ptr = trigger->next;

  if (trigger->next)
    /* Tell successor its predecessor is our predecessor */
    trigger->next->prev_ptr = trigger->prev_ptr;
} 

void ssm_trigger(ssm_sv_t *var, ssm_priority_t priority)
{
  assert(var);
  for (ssm_trigger_t *trig = var->triggers ; trig ; trig = trig->next)
    if (trig->act->priority > priority)
      ssm_activate(trig->act);       
}


/** Starting at the hole, walk up toward the root of the tree, copying
 * parent to child until we find where we can put the new activation
 * record
 *
 * \param hole Index of the hole that needs to be filled
 * \param act Activation record that needs to be placed in the heap
 */
SSM_STATIC_INLINE void act_queue_percolate_up(q_idx_t hole, ssm_act_t *act)
{
  assert(act);
  assert(hole >= SSM_QUEUE_HEAD && hole <= act_queue_len);
  ssm_priority_t priority = act->priority;
  for ( ; hole > SSM_QUEUE_HEAD && priority < act_queue[hole >> 1]->priority ;
	hole >>= 1 )
    act_queue[hole] = act_queue[hole >> 1];
  act_queue[hole] = act;
  act->scheduled = true;
}

/** Starting at the hole, walk down towards the leaves of the tree,
 * moving the earlier child to the parent and repeating the process on
 * that child.  This makes the parent earlier than both children.
 * Stop when the activation record we're trying to place has a lower
 * priority than either children.
 *
 * \param hole Where to place the given activation record
 * \param act Activation record to be placed in the queue
 */
SSM_STATIC_INLINE void act_queue_percolate_down(q_idx_t hole,
						ssm_act_t *act)
{
  assert(act);
  assert(hole >= SSM_QUEUE_HEAD && hole <= act_queue_len);
  ssm_priority_t priority = act->priority;
  for (;;) {
    // Find the earlier of the two children
    q_idx_t child = hole << 1; // Left child
    if (child > act_queue_len) break; // The parent was a leaf
    if (child + 1 <= act_queue_len &&
	act_queue[child+1]->priority < act_queue[child]->priority)
      child++; // Right child is earlier than the left

    if (priority < act_queue[child]->priority)
      break; // Earlier child is later than what we're inserting
    act_queue[hole] = act_queue[child];
    hole = child;
  }
  act_queue[hole] = act;
}

void ssm_activate(ssm_act_t *act)
{
  assert(act);
  if (act->scheduled) return; // Don't activate an already activated routine

  q_idx_t hole = ++act_queue_len;

  if (act_queue_len > SSM_ACT_QUEUE_SIZE)
    SSM_THROW(SSM_EXHAUSTED_ACT_QUEUE);

  act_queue_percolate_up(hole, act);
}

ssm_time_t ssm_next_event_time() {
  return event_queue_len ?
    event_queue[SSM_QUEUE_HEAD]->later_time : SSM_NEVER;
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
SSM_STATIC_INLINE
q_idx_t find_queued_event(ssm_sv_t *var /**< must be non-NULL */)
{
  assert(var); // Should be a real variable
  assert(var->later_time != SSM_NEVER); // Should be in the queue
  for (q_idx_t i = SSM_QUEUE_HEAD ; i <= event_queue_len ; i++)
    if (event_queue[i] == var) return i;
  assert(0); // Should have found the variable if it was marked as queued
  return 0;
}

void ssm_initialize(ssm_sv_t *var, void (*update)(ssm_sv_t *))
{
  assert(var);
  *var = (ssm_sv_t){
    .update = update,
    .triggers = 0,
    .later_time = SSM_NEVER,
    .last_updated = SSM_NEVER
  };
}

/** Starting at the hole, walk up toward the root of the tree, copying
 * parent to child until we find where we can put the new event.
 *
 * \param hole Index of the hole, from 1 to event_queue_len, inclusive
 * \param var Event to place in the queue.
 *  Its later_time field should be set to its proper value.
 */
SSM_STATIC_INLINE void event_queue_percolate_up(q_idx_t hole,
						ssm_sv_t *var)
{
  assert(var);
  assert(hole >= SSM_QUEUE_HEAD && hole <= event_queue_len);  
  ssm_time_t later = var->later_time;
  for ( ; hole > SSM_QUEUE_HEAD &&
	  later < event_queue[hole >> 1]->later_time ; hole >>= 1 )
    event_queue[hole] = event_queue[hole >> 1];
  event_queue[hole] = var;
}

/** Starting at the hole, walk down towards the leaves of the tree,
 * moving the earlier child to the parent and repeating the process on
 * that child.  This makes the parent earlier than both children.
 * Stop when the event we're trying to place is earlier than both
 * of the children.
 *
 * \param hole Where to place the given event
 * \param event Event to place in the queue
 */
SSM_STATIC_INLINE void event_queue_percolate_down(q_idx_t hole,
						  ssm_sv_t *event)
{
  assert(event);
  assert(hole >= SSM_QUEUE_HEAD && hole <= event_queue_len);
  ssm_time_t later = event->later_time;
  for (;;) {
    // Find the earlier of the two children
    q_idx_t child = hole << 1; // Left child
    if (child > event_queue_len) break; // The parent was a leaf
    if (child + 1 <= event_queue_len &&
	event_queue[child+1]->later_time < event_queue[child]->later_time)
      child++; // Right child is earlier than the left

    if (later < event_queue[child]->later_time)
      break; // Earlier child is later than what we're inserting
    event_queue[hole] = event_queue[child];
    hole = child;
  }
  event_queue[hole] = event;
}

void ssm_schedule(ssm_sv_t *var, ssm_time_t later)
{
  assert(var);      // A real variable
  if (later <= now) // "later" must be in the future
    SSM_THROW(SSM_INVALID_TIME);

  if (var->later_time == SSM_NEVER) {
    // Variable does not have a pending event: add it to the queue
    q_idx_t hole = ++event_queue_len;
    if (event_queue_len > SSM_EVENT_QUEUE_SIZE)
      SSM_THROW(SSM_EXHAUSTED_EVENT_QUEUE);

    var->later_time = later;
    event_queue_percolate_up(hole, var);

  } else {
    // Variable has a pending event: reposition the event in the queue
    // as appropriate

    q_idx_t hole = find_queued_event(var);

    var->later_time = later;
    if (hole == SSM_QUEUE_HEAD || event_queue[hole >> 1]->later_time < later)
      event_queue_percolate_down(hole, var);
    else
      event_queue_percolate_up(hole, var);
  }
}

void ssm_unschedule(ssm_sv_t *var)
{
  assert(var);        // A real variable
  if (var->later_time != SSM_NEVER) {
    q_idx_t hole = find_queued_event(var);
    var->later_time = SSM_NEVER;
    ssm_sv_t *moved_var = event_queue[event_queue_len--];
    if (hole < SSM_QUEUE_HEAD + event_queue_len) {
      // Percolate only if removal led to a hole in the queue; no need to do
      // this if we happened to remove the last element of the queue.
      if (hole == SSM_QUEUE_HEAD ||
          event_queue[hole >> 1]->later_time < moved_var->later_time)
        event_queue_percolate_down(hole, moved_var);
      else
        event_queue_percolate_up(hole, moved_var);
    }
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
    
    ssm_sv_t *sv = event_queue[SSM_QUEUE_HEAD];
    (*sv->update)(sv);  // Update the scheduled variable
    sv->last_updated = now;
    sv->later_time = SSM_NEVER;

    /* Schedule all sensitive triggers */
    for (ssm_trigger_t *trigger = sv->triggers ; trigger ;
	 trigger = trigger->next)
      ssm_activate(trigger->act);

    /* Remove the top event from the queue by inserting the last
       element in the queue at the front and percolating it toward the leaves */
    ssm_sv_t *to_insert = event_queue[event_queue_len--]; // get last

    if (event_queue_len) // Was this the last?
      event_queue_percolate_down(SSM_QUEUE_HEAD, to_insert);
  }

  while (act_queue_len > 0) {
    ssm_act_t *to_run = act_queue[SSM_QUEUE_HEAD];
    to_run->scheduled = false;

    /* Remove the top activation record from the queue by inserting the
       last element in the queue at the front and percolating it down */
    ssm_act_t *to_insert = act_queue[act_queue_len--];

    if (act_queue_len)
      act_queue_percolate_down(SSM_QUEUE_HEAD, to_insert);

    to_run->step(to_run); // Execute the step function
  }
}

#ifdef SSM_DEBUG
/** Assert the event queue is well-formed
 */
void event_queue_consistency_check()
{
  if (event_queue_len == 0) return;
  
  assert(event_queue_len <= SSM_EVENT_QUEUE_SIZE); // No overflow

  for (q_idx_t i = SSM_QUEUE_HEAD ; i <= event_queue_len ; i++) {
    assert(event_queue[i]); // Events should be valid
    assert(event_queue[i]->later_time != SSM_NEVER); // Queue events should have valid time
    q_idx_t child = i << 1;
    if (child <= event_queue_len) {
      assert(event_queue[child]);
      assert(event_queue[child]->later_time >= event_queue[i]->later_time);
      if (++child <= event_queue_len) {
	assert(event_queue[child]);
	assert(event_queue[child]->later_time >= event_queue[i]->later_time);
      }
    }
  }
}

/** Assert the activation record queue is well-formed
 */
void act_queue_consistency_check()
{
  if (act_queue_len == 0) return;
  
  assert(act_queue_len <= SSM_ACT_QUEUE_SIZE); // No overflow

  for (q_idx_t i = SSM_QUEUE_HEAD ; i <= act_queue_len ; i++) {
    assert(act_queue[i]); // Acts should be valid
    assert(act_queue[i]->scheduled); // If it's in the queue, it should say so
    q_idx_t child = i << 1;
    if (child <= act_queue_len) {
      assert(act_queue[child]);
      assert(act_queue[child]->priority >= act_queue[i]->priority);
      if (++child <= act_queue_len) {
	assert(act_queue[child]);
	assert(act_queue[child]->priority >= act_queue[i]->priority);
      }
    }
  }
}
#endif

/*
 * Look for John's ssm-test-queue or something in his test/ directory:
 * test harness for the binary heaps
 */
