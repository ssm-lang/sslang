#ifndef _QUEUE_H
#define _QUEUE_H

/**
 * The interface for the queue implementation, used by ssm-sched.c.
 *
 * The primary purpose for this header file and the separate implementation in
 * ssm-queue.c is to separate the queue implementation from the business logic
 * of scheduling.
 *
 * There are three kinds of queues: event queues (sv_t *), run queues (cont *),
 * and inner queues (sel_t). (Inner queues allow data types to schedule events
 * internally.) For each of these, the priority queue logic is the same, with
 * the only difference being (1) the different sizes of the underlying type, and
 * (2) the different ways in which elements should be compared with one another
 * for priority.
 */

#include <ssm-core.h> /* for ssm_time_t */
#include <stddef.h>   /* for size_t */

/** Queue index type; these are 1-indexed*/
typedef size_t idx_t;

/** Priority queues start at index 1 */
#define QUEUE_HEAD 1

/**
 * For queues with non-pointer types, we can pack the length in the first
 * element. However, we can't do this for queues with non-pointer types, since
 * the correct lengths may be clobbered by optimizations that assume aligned
 * pointers.
 */
#define QUEUE_LEN 0

/*** Type-specialized queue operations ***/

void enqueue_event(struct sv **event_queue, size_t *queue_len,
                   struct sv *to_insert);
void dequeue_event(struct sv **event_queue, size_t *queue_len,
                   idx_t to_dequeue);
void requeue_event(struct sv **event_queue, size_t *queue_len,
                   idx_t to_requeue);
idx_t index_of_event(struct sv **event_queue, size_t *queue_len,
                     struct sv *to_find);

void enqueue_act(struct act **act_queue, size_t *queue_len,
                 struct act *to_insert);
void dequeue_act(struct act **act_queue, size_t *queue_len, idx_t to_dequeue);
void requeue_act(struct act **act_queue, size_t *queue_len, idx_t to_requeue);
idx_t index_of_act(struct act **act_queue, size_t *queue_len,
                   struct act *to_find);

#endif /* ifndef _QUEUE_H */
