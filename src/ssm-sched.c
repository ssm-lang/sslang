/**
 * Implementation for the SSM runtime's scheduler, which is only aware of pure
 * events and abstract activation records.
 */

#include <ssm-act.h>
#include <ssm-queue.h>
#include <ssm-runtime.h>
#include <ssm-sv.h>

#ifndef EVENT_QUEUE_SIZE
#define EVENT_QUEUE_SIZE 2048
#elif EVENT_QUEUE_SIZE < 2
#error EVENT_QUEUE_SIZE < 2, use larger size
#endif

#ifndef ACT_QUEUE_SIZE
#define ACT_QUEUE_SIZE 1024
#elif ACT_QUEUE_SIZE < 2
#error ACT_QUEUE_SIZE < 2, use larger size
#endif

/**
 * Event queue, used to track and schedule events between instants.
 *
 * Managed as a binary heap sorted by e->later_time, implemented in ssm-queue.c.
 */
struct sv *event_queue[EVENT_QUEUE_SIZE + QUEUE_HEAD];
size_t event_queue_len = 0;

/**
 * Activation record queue, used to track and schedule continuations at each
 * instant.
 *
 * Managed as a binary heap sorted by a->priority, implemented in ssm-queue.c.
 */
struct act *act_queue[ACT_QUEUE_SIZE + QUEUE_HEAD];
size_t act_queue_len = 0;

/**
 * Note that this starts out uninitialized. It is the responsibility of the
 * runtime to do so.
 */
ssm_time_t now;

/*** Internal helpers {{{ ***/

static void schedule_act(struct act *act) {
  DEBUG_ASSERT(((int8_t)act->depth) >= 0, "negative depth\n");
  if (!act->scheduled) {
    DEBUG_ASSERT(act_queue_len + 1 <= ACT_QUEUE_SIZE, "contqueue full\n");
    enqueue_act(act_queue, &act_queue_len, act);
  }
  act->scheduled = true;
}

static struct act *unschedule_act(idx_t idx) {
  assert(idx >= QUEUE_HEAD);
  struct act *act = act_queue[idx];
  assert(act->scheduled);

  act->scheduled = false;
  dequeue_act(act_queue, &act_queue_len, idx);
  return act;
}

/**
 * Enqueue lower priority sensitive continuations of a scheduled variable.
 */
static void schedule_sensitive_triggers(struct sv *sv, priority_t priority) {
  for (struct trigger *trigger = sv->triggers; trigger; trigger = trigger->next)
    if (trigger->act->priority > priority)
      if (!trigger->act->scheduled)
        schedule_act(trigger->act);
}

/**
 * Enqueue all the sensitive continuations of a scheduled variable.
 */
static void schedule_all_sensitive_triggers(struct sv *sv) {
  for (struct trigger *trigger = sv->triggers; trigger; trigger = trigger->next)
    if (!trigger->act->scheduled)
      schedule_act(trigger->act);
}

/**
 * Calls sv->update to perform a delayed assignment, if its update
 * method/payload exists, then sets sv->later_time to NO_EVENT_SCHEDULED if
 * sv is an atomic type. Finally, put its sensitive triggers on act_queue.
 */
static void update_event(struct sv *sv) {
  assert(sv->later_time == now);

  if (sv->update)
    /* Non-unit type, so we need to call update method to update payload */
    sv->update(sv);

  sv->last_updated = now;
  sv->later_time = NO_EVENT_SCHEDULED;

  schedule_all_sensitive_triggers(sv);
  DEBUG_PRINT("event %lu value %s %s\n", now, DEBUG_SV_GET_TYPE_NAME(sv->debug),
              DEBUG_SV_GET_VALUE_REPR(sv->debug, sv));
}

/*** Internal helpers }}} ***/

/*** Events API, exposed via ssm-event.h {{{ ***/

#ifdef DEBUG
static struct debug_buffer value_repr_event(struct sv *sv) {
  return (struct debug_buffer){.buf = "(unknown/unit value)"};
}
#endif

void initialize_event(struct sv *sv) {
  sv->update = NULL;
  sv->triggers = NULL;
  sv->last_updated = now;
  sv->later_time = NO_EVENT_SCHEDULED;
  DEBUG_SV_SET_VAR_NAME(sv->debug, "(unknown var name)");
  DEBUG_SV_SET_TYPE_NAME(sv->debug, "(unknown/unit type)");
  DEBUG_SV_SET_VALUE_REPR(sv->debug, value_repr_event);
}

void unsched_event(struct sv *sv) {
  idx_t idx = index_of_event(event_queue, &event_queue_len, sv);
  assert(QUEUE_HEAD <= idx);
  assert(sv->later_time != NO_EVENT_SCHEDULED);
  sv->later_time = NO_EVENT_SCHEDULED;
  dequeue_event(event_queue, &event_queue_len, idx);
}

void assign_event(struct sv *sv, priority_t prio) {
  sv->last_updated = now;
  schedule_sensitive_triggers(sv, prio);
}

void later_event(struct sv *sv, ssm_time_t then) {
  assert(then != NO_EVENT_SCHEDULED);
  DEBUG_ASSERT(now < then, "bad after\n");

  if (sv->later_time == NO_EVENT_SCHEDULED) {
    /* This event isn't already scheduled, so add it to the event queue. */
    DEBUG_ASSERT(event_queue_len + 1 <= EVENT_QUEUE_SIZE, "eventqueue full\n");
    sv->later_time = then;
    enqueue_event(event_queue, &event_queue_len, sv);
  } else {
    /* This event is already scheduled, so we need to reschedule it. */
    idx_t idx = index_of_event(event_queue, &event_queue_len, sv);
    assert(QUEUE_HEAD <= idx);
    sv->later_time = then;
    requeue_event(event_queue, &event_queue_len, idx);
  }
}

bool event_on(struct sv *var) { return var->last_updated == now; }

/*** Events API }}} ***/

/*** Activation records API, exposed via ssm-act.h {{{ ***/

void act_fork(struct act *act) {
  assert(act);
  assert(act->caller);
  schedule_act(act);
}

void sensitize(struct sv *var, struct trigger *trigger) {
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

void desensitize(struct trigger *trigger) {
  assert(trigger);
  assert(trigger->prev_ptr);

  /* Tell predecessor to skip us */
  *trigger->prev_ptr = trigger->next;

  if (trigger->next)
    /* Tell successor its predecessor is our predecessor */
    trigger->next->prev_ptr = trigger->prev_ptr;
}

/*** Activation records API }}} ***/

/*** Runtime API, exposed via ssm-runtime.h ***/

ssm_time_t next_event_time(void) {
  return event_queue_len ? event_queue[QUEUE_HEAD]->later_time
                         : NO_EVENT_SCHEDULED;
}

void set_now(ssm_time_t t) { now = t; }

void tick() {
  /*
   * For each queued event scheduled for the current time, remove the event from
   * the queue, update its variable, and schedule everything sensitive to it.
   */
  while (event_queue_len > 0 && event_queue[QUEUE_HEAD]->later_time == now) {
    struct sv *sv = event_queue[QUEUE_HEAD];

    update_event(sv);

    if (sv->later_time == NO_EVENT_SCHEDULED)
      /* Unschedule */
      dequeue_event(event_queue, &event_queue_len, QUEUE_HEAD);
    else
      /* Reschedule */
      requeue_event(event_queue, &event_queue_len, QUEUE_HEAD);
  }

  DEBUG_PRINT("numconts %ld\n", act_queue_len);

  /*
   * Until the queue is empty, take the lowest-numbered continuation from the
   * activation record queue and run it, which might insert additional
   * continuations in the queue.
   *
   * Note that we remove it from the queue first before running it in case it
   * tries to schedule itself.
   */
  while (act_queue_len > 0) {
    struct act *to_run = unschedule_act(QUEUE_HEAD);
    to_run->step(to_run);
  }
  DEBUG_PRINT("now %lu eventqueuesize %ld\n", now, event_queue_len);
}

/*** Runtime API }}} ***/
