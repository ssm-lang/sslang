/**
 * Type-generic implementation of priority queue logic, with type-specialized
 * instantiations for use in ssm-sched.c.
 *
 * With any luck, a good optimizing C compiler will inline the function pointers
 * and synthesize type-specific code for per-type implementations exposed by
 * this compilation unit.
 */
#include <ssm-act.h> /* for definition of struct act */
#include <ssm-queue.h>
#include <ssm-sv.h>         /* for definition of struct sv */
#include <test-ssm-queue.h> /* for testing */

/*** Priority queue logic {{{ */

/**
 * NOTE: Caller is responsible for incrementing queue_len *before* this.
 */
static inline void enqueue(int (*compar)(void *, void *, void *), void *ctx,
                           void *(*get)(void *, idx_t),
                           void (*set)(void *, idx_t, void *), void *queue,
                           size_t queue_len, void *to_insert) {
  /*
   * NOTE: queue_len is already 1 greater than the number of elements, so its
   * value is 2 greater than the last element in a 0-indexed scheme. However, in
   * idx_t's 1-indexed scheme, it is just 1 past the last element---where we
   * want the hole to be.
   */
  idx_t hole = queue_len + 1 - QUEUE_HEAD;
  for (; hole > QUEUE_HEAD && compar(to_insert, get(queue, hole >> 1), ctx) < 0;
       hole >>= 1)
    set(queue, hole, get(queue, hole >> 1));

  set(queue, hole, to_insert);
}

static inline void percolate_down(int (*compar)(void *, void *, void *),
                                  void *ctx, void *(*get)(void *, idx_t),
                                  void (*set)(void *, idx_t, void *),
                                  void *queue, size_t queue_len, idx_t hole,
                                  void *to_insert) {
  for (;;) {
    /* Find earlier of hole's two children */
    idx_t child = hole << 1; /* Left child */

    if (queue_len < child)
      /* Reached the bottom of minheap */
      break;

    /* Compare against right child */
    if (child + 1 <= queue_len &&
        compar(get(queue, child + 1), get(queue, child), ctx) < 0)
      child++; /* Right child is earlier */

    /* Is to_insert a suitable parent for (less than/equal to) both children? */
    if (compar(to_insert, get(queue, child), ctx) <= 0)
      /* to_insert is a suitable parent for (leq) both children */
      break;

    /* If not, swap earlier child up (push hole down), and descend */
    set(queue, hole, get(queue, child));
    hole = child;
  }
  set(queue, hole, to_insert);
  return;
}

static inline void percolate_up(int (*compar)(void *, void *, void *),
                                void *ctx, void *(*get)(void *, idx_t),
                                void (*set)(void *, idx_t, void *), void *queue,
                                size_t queue_len, idx_t hole, void *to_insert) {
  for (;;) {
    idx_t parent = hole >> 1;

    if (compar(get(queue, parent), to_insert, ctx) <= 0)
      break;

    set(queue, hole, get(queue, parent));
    hole = parent;

    /* No more parents to check; insert at root of heap */
    if (hole == QUEUE_HEAD)
      break;
  }

  set(queue, hole, to_insert);
  return;
}

/**
 * NOTE: to_insert must not already be in the queue, but queue_len should
 * already be large enough to accommodate new item.
 */
static inline void fill_hole(int (*compar)(void *, void *, void *), void *ctx,
                             void *(*get)(void *, idx_t),
                             void (*set)(void *, idx_t, void *), void *queue,
                             size_t queue_len, idx_t hole, void *to_insert) {

  /* If to_insert is less than the parent of the hole */
  if (hole == QUEUE_HEAD || compar(get(queue, hole >> 1), to_insert, ctx) < 0)
    /* We may need to percolate hole down, so we check its children */
    percolate_down(compar, ctx, get, set, queue, queue_len, hole, to_insert);
  else
    /* Otherwise, move hole up to find somewhere suitable for insertion */
    percolate_up(compar, ctx, get, set, queue, queue_len, hole, to_insert);
}

/*** Priority queue logic }}} */

/*** Event queue (struct sv *) {{{ */

static int compar_event(void *lp, void *rp, void *ctx) {
  struct sv *l = *(struct sv **)lp, *r = *(struct sv **)rp;

  if (l->later_time < r->later_time)
    return -1;
  else if (l->later_time == r->later_time)
    return 0;
  else
    return 1;
}

static void *get_event(void *queue, idx_t idx) {
  struct sv **q = (struct sv **)queue;
  return &q[idx];
}

static void set_event(void *queue, idx_t idx, void *val) {
  struct sv **q = (struct sv **)queue, **v = (struct sv **)val;
  q[idx] = *v;
}

void enqueue_event(struct sv **event_queue, size_t *queue_len,
                   struct sv *to_insert) {
  enqueue(compar_event, NULL, get_event, set_event, event_queue, ++*queue_len,
          &to_insert);
}

void dequeue_event(struct sv **event_queue, size_t *queue_len,
                   idx_t to_dequeue) {
  /*
   * We don't need to create a separate copy of the tail of the queue because
   * we decrement the queue_size before we call fill_hole, leaving tail beyond
   * the queue.
   */
  struct sv **to_insert = &event_queue[QUEUE_HEAD + --*queue_len];
  fill_hole(compar_event, NULL, get_event, set_event, event_queue, *queue_len,
            to_dequeue, to_insert);
}

void requeue_event(struct sv **event_queue, size_t *queue_len,
                   idx_t to_requeue) {
  struct sv *to_insert = event_queue[to_requeue];
  fill_hole(compar_event, NULL, get_event, set_event, event_queue, *queue_len,
            to_requeue, &to_insert);
}

idx_t index_of_event(struct sv **event_queue, size_t *queue_len,
                     struct sv *to_find) {
  for (idx_t idx = QUEUE_HEAD; idx < *queue_len; idx++)
    if (event_queue[idx] == to_find)
      return idx;
  return 0;
}

/*** Event queue }}} */

/*** Act queue (struct act *) {{{ */

static int compar_act(void *lp, void *rp, void *ctx) {
  struct act *l = *(struct act **)lp, *r = *(struct act **)rp;

  if (l->priority < r->priority)
    return -1;
  else if (l->priority == r->priority)
    return 0;
  else
    return 1;
}

static void *get_act(void *queue, idx_t idx) {
  struct act **q = (struct act **)queue;
  return &q[idx];
}

static void set_act(void *queue, idx_t idx, void *val) {
  struct act **q = (struct act **)queue, **v = (struct act **)val;
  q[idx] = *v;
}

void enqueue_act(struct act **act_queue, size_t *queue_len,
                 struct act *to_insert) {
  enqueue(compar_act, NULL, get_act, set_act, act_queue, ++*queue_len,
          &to_insert);
}

void dequeue_act(struct act **act_queue, size_t *queue_len, idx_t to_dequeue) {
  /*
   * We don't need to create a separate copy of the tail of the queue because
   * we decrement the queue_size before we call fill_hole, leaving tail beyon
   * the queue.
   */
  struct act **to_insert = &act_queue[QUEUE_HEAD + --*queue_len];
  fill_hole(compar_act, NULL, get_act, set_act, act_queue, *queue_len,
            to_dequeue, to_insert);
}

void requeue_act(struct act **act_queue, size_t *queue_len, idx_t to_requeue) {
  struct act *to_insert = act_queue[to_requeue];
  fill_hole(compar_act, NULL, get_act, set_act, act_queue, *queue_len,
            to_requeue, &to_insert);
}

idx_t index_of_act(struct act **act_queue, size_t *queue_len,
                   struct act *to_find) {
  for (idx_t idx = QUEUE_HEAD; idx < *queue_len; idx++)
    if (act_queue[idx] == to_find)
      return idx;
  return 0;
}

/*** Act queue }}} */

/*** long queue (for testing) {{{ */
/* For convenience, the 0th element of these arrays stores the length */

static int compar_test(void *lp, void *rp, void *ctx) {
  long l = *(long *)lp, r = *(long *)rp;

  if (l < r)
    return -1;
  else if (l == r)
    return 0;
  else
    return 1;
}

static void *get_test(void *queue, idx_t idx) {
  long *q = (long *)queue;
  return &q[idx];
}

static void set_test(void *queue, idx_t idx, void *val) {
  long *q = (long *)queue, *v = (long *)val;
  q[idx] = *v;
}

void enqueue_test(long *long_queue, long to_insert) {
  long *queue_len = &long_queue[QUEUE_LEN];
  enqueue(compar_test, NULL, get_test, set_test, long_queue, ++*queue_len,
          &to_insert);
}

void dequeue_test(long *long_queue, idx_t to_dequeue) {
  /*
   * We don't need to create a separate copy of the tail of the queue because
   * we decrement the queue_size before we call fill_hole, leaving tail beyon
   * the queue.
   */
  long *queue_len = &long_queue[QUEUE_LEN];
  long *to_insert = &long_queue[QUEUE_HEAD + --*queue_len];
  fill_hole(compar_test, NULL, get_test, set_test, long_queue, *queue_len,
            to_dequeue, to_insert);
}

void requeue_test(long *long_queue, idx_t to_requeue) {
  long *queue_len = &long_queue[QUEUE_LEN];
  long to_insert = long_queue[to_requeue];
  fill_hole(compar_test, NULL, get_test, set_test, long_queue, *queue_len,
            to_requeue, &to_insert);
}

idx_t index_of_test(long *long_queue, long to_find) {
  long *queue_len = &long_queue[QUEUE_LEN];
  for (idx_t idx = QUEUE_HEAD; idx < *queue_len; idx++)
    if (long_queue[idx] == to_find)
      return idx;
  return 0;
}

/*** long queue }}} */

/* vim: set ts=2 sw=2 tw=80 et foldmethod=marker :*/
