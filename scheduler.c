/* C99 */

/* Implementing a priority queue with a binary heap:
   https://www.cs.cmu.edu/~adamchik/15-121/lectures/Binary%20Heaps/heaps.html
   http://www.algolist.net/Data_structures/Binary_heap/Remove_minimum */

#include "ssm.h"
#include <assert.h>
#include <stdint.h>
// #include <stdio.h>

ssm_time_t now;

extern inline void leave(rar_t *, size_t);
extern inline rar_t *enter(size_t, stepf_t *,
				    rar_t *, priority_t, pdepth_t);
extern inline void call(rar_t *);
extern inline bool event_on(cv_t *);


#define CONT_QUEUE_SIZE 64
cont_queue_index_t cont_queue_len = 0;
rar_t *cont_queue[CONT_QUEUE_SIZE+1];

#define EVENT_QUEUE_SIZE 255
event_queue_index_t event_queue_len = 0;
cv_t *event_queue[EVENT_QUEUE_SIZE+1];

void sensitize(cv_t *var, trigger_t *trigger)
{
  assert(var);
  assert(trigger);
  trigger->next = var->triggers;              // Point us to the first element
  if (var->triggers)
    var->triggers->prev_ptr = &trigger->next; // Make first element point to us
  var->triggers = trigger;                    // Insert us at the beginning
  trigger->prev_ptr = &var->triggers;         // Our previous is the variable
}

void desensitize(trigger_t *trigger)
{
  assert(trigger);
  assert(trigger->prev_ptr);
  *trigger->prev_ptr = trigger->next;         // Tell predecessor to skip us
  if (trigger->next)
    trigger->next->prev_ptr = trigger->prev_ptr; // Tell successor its predecessor is our predecessor
}

void later_event(cv_t *var, ssm_time_t then)
{
  assert(var);
  assert(now < then);  // No scheduling in this instant
  if ( var->event_time == NO_EVENT_SCHEDULED ) {   
    // Insert this event in the global event queue

    event_queue_index_t i = ++event_queue_len;
    assert( i <= EVENT_QUEUE_SIZE ); // FIXME: should handle this better

    // Copy parent to child until we find where we can put the new one
    for ( ; i > 1 && then < event_queue[i >> 1]->event_time ; i >>= 1 )
      event_queue[i] = event_queue[i >> 1];
    event_queue[i] = var;
    var->event_time = then;
    
  } else {

    // FIXME: Remove the old event and insert this event in the global event queue
    assert(0);
  }

}

void enqueue(rar_t *cont)
{
  assert(cont);
  if (cont->scheduled) return; // Don't add a continuation twice

  priority_t priority = cont->priority;

  cont_queue_index_t i = ++cont_queue_len;
  assert( i <= CONT_QUEUE_SIZE ); // FIXME: should handle this better

  // Copy parent to child until we find where we can put the new one
  for ( ; i > 1 && priority < cont_queue[i >> 1]->priority ; i >>= 1 )
    cont_queue[i] = cont_queue[i >> 1];
  cont_queue[i] = cont;
  cont->scheduled = true;

  /*
  printf("Scheduling %08x: ", cont->priority);
  for ( int i = 1 ; i <= cont_queue_len ; i++ )
    printf("%08x ", cont_queue[i]->priority );
  printf("\n");
  */
}

void schedule_sensitive(cv_t *var, priority_t priority)
{
  assert(var);
  for (trigger_t *trigger = var->triggers ; trigger ; trigger = trigger->next)
    if (trigger->rar->priority > priority)
      enqueue( trigger->rar );
}

void fork(rar_t *rar)
{
  assert(rar);
  assert(rar->caller);
  rar->caller->children++;
  enqueue(rar);
}

void update_int(cv_t *var)
{
  assert(var);
  assert(var->event_time == now);
  cv_int_t *iv = (cv_int_t *) var;
  iv->value = iv->event_value;
}

void initialize_int(cv_int_t *v, int value)
{
  assert(v);
  *v = (cv_int_t) { .update = update_int,
		    .triggers = NULL,
		    .last_updated = now,
		    .event_time = NO_EVENT_SCHEDULED,
		    .value = value };
}

void assign_int(cv_int_t *iv, priority_t priority, int value)
{
  iv->value = value;
  iv->last_updated = now;
  schedule_sensitive((cv_t *) iv, priority);
}

void later_int(cv_int_t *var, ssm_time_t time, int val)
{
  assert(var);
  var->event_value = val;
  later_event((cv_t *) var, time);
}

void update_bool(cv_t *var)
{
  assert(var);
  assert(var->event_time == now);
  cv_bool_t *iv = (cv_bool_t *) var;
  iv->value = iv->event_value;
}

void initialize_bool(cv_bool_t *v, bool value)
{
  assert(v);
  *v = (cv_bool_t) { .update = update_bool,
		      .triggers = NULL,
		      .last_updated = now,
		      .event_time = NO_EVENT_SCHEDULED,
		      .value = value };
}

void assign_bool(cv_bool_t *iv, priority_t priority, bool value)
{
  iv->value = value;
  iv->last_updated = now;
  schedule_sensitive((cv_t *) iv, priority);
}

void later_bool(cv_bool_t *var, ssm_time_t time, bool val)
{
  assert(var);
  var->event_value = val;
  later_event((cv_t *) var, time);
}


// Nothing to do
void update_event(cv_t *var)
{
  assert(var);
  assert(var->event_time == now);
}

void initialize_event(cv_event_t *v)
{
  assert(v);
  *v = (cv_event_t) { .update = update_event,
		      .triggers = NULL,
		      .last_updated = now,
		      .event_time = NO_EVENT_SCHEDULED };
}

void assign_event(cv_event_t *iv, priority_t priority)
{
  iv->last_updated = now;
  schedule_sensitive((cv_t *) iv, priority);
}



void tick()
{
  // For each queued event scheduled for the current time,
  // remove the event from the queue, update its variable, and schedule
  // everything sensitive to it

  //printf("Tick @%ld\n", now);

  while ( event_queue_len > 0 && event_queue[1]->event_time == now ) {
    cv_t *var = event_queue[1];

    // printf("Updating %p\n", (void *) var);
    
    (*var->update)(var);     // Update the value
    var->last_updated = now; // Remember that it was updated
    // Schedule all sensitive continuations
    for (trigger_t *trigger = var->triggers ; trigger ; trigger = trigger->next)
      enqueue(trigger->rar);

    // Remove the earliest event from the queue
    var->event_time = NO_EVENT_SCHEDULED;
    cv_t *to_insert = event_queue[event_queue_len--];
    ssm_time_t then = to_insert->event_time;

    event_queue_index_t parent = 1;
    for (;;) {
      event_queue_index_t child = parent << 1; // Left child
      if ( child > event_queue_len) break;
      if ( child + 1 <= event_queue_len &&
	   event_queue[child+1]->event_time < event_queue[child]->event_time )
	child++; // Right child is earlier
      if (then < event_queue[child]->event_time) break; // to_insert is earlier
      event_queue[parent] = event_queue[child];
      parent = child;
    }
    event_queue[parent] = to_insert;
  }

  // Until the queue is empty, take the lowest-numbered continuation from
  // the queue and run it, which might insert additional continuations
  // in the queue

  while ( cont_queue_len > 0 ) {
    // Get minimum element
    rar_t *to_run = cont_queue[1];
    to_run->scheduled = false;
    
    rar_t *to_insert = cont_queue[cont_queue_len--];
    priority_t priority = to_insert->priority;

    cont_queue_index_t parent = 1;
    // Invariant: there's a hole at parent where we'd like to put the
    // "to_insert" continuation.  If all children are greater, we're done,
    // otherwise, move the smaller child into the hole and go to that child
    for (;;) {      
      cont_queue_index_t child = parent << 1;
      if (child > cont_queue_len) break;
      if (child + 1 <= cont_queue_len &&
	  cont_queue[child+1]->priority < cont_queue[child]->priority)
	child++;
      if (priority < cont_queue[child]->priority) break;
      cont_queue[parent] = cont_queue[child];
      parent = child;
    }
    cont_queue[parent] = to_insert;

    // printf("Invoking %08x\n", to_run->priority);

    call(to_run);
  }
  
}
