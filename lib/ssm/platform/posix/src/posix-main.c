#include "posix-common.h"

extern ssm_act_t *__enter_main(ssm_act_t *caller, ssm_priority_t priority,
                               ssm_depth_t depth, ssm_value_t *__argv,
                               ssm_value_t *__return_val);
extern ssm_act_t *__enter_stdout_handler(ssm_act_t *parent,
                                         ssm_priority_t priority,
                                         ssm_depth_t depth, ssm_value_t *argv,
                                         ssm_value_t *ret);
extern void __spawn_stdin_handler(ssm_sv_t *ssm_stdin);
extern void __kill_stdin_handler(void);

int ssm_sem_fd[2];
atomic_size_t rb_r;
atomic_size_t rb_w;
pthread_mutex_t rb_lk;

struct ssm_input ssm_input_rb[SSM_INPUT_RB_SIZE];

char **ssm_init_args;

#define MAX_PAGES 2048
static void *pages[MAX_PAGES];
static size_t allocated_pages = 0;
static void *alloc_page(void) {
  if (allocated_pages >= MAX_PAGES) {
    SSM_THROW(SSM_EXHAUSTED_MEMORY);
    exit(3);
  }
  void *m = pages[allocated_pages++] = malloc(SSM_MEM_PAGE_SIZE);
  memset(m, 0, SSM_MEM_PAGE_SIZE);
  return m;
}

static void *alloc_mem(size_t size) { return malloc(size); }

static void free_mem(void *mem, size_t size) { free(mem); }

#ifdef CONFIG_MEM_STATS
void print_mem_stats(ssm_mem_statistics_t *stats) {
  ssm_mem_statistics_collect(stats);

  fprintf(stderr, "sizeof(struct ssm_mm) = %lu\n", stats->sizeof_ssm_mm);
  fprintf(stderr, "page size %lu\n", stats->page_size);
  fprintf(stderr, "pages allocated %lu\n", stats->pages_allocated);
  fprintf(stderr, "objects allocated %lu\n", stats->objects_allocated);
  fprintf(stderr, "objects freed %lu\n", stats->objects_freed);
  fprintf(stderr, "live objects %lu\n", stats->live_objects);

  size_t pool_count = stats->pool_count;

  fprintf(stderr, "%lu pools\n", pool_count);

  for (size_t i = 0; i < pool_count; i++) {
    fprintf(stderr,
            "pool %3lu: pages %3lu  block-size %5lu  free-blocks %5lu\n", i,
            stats->pool[i].pages_allocated, stats->pool[i].block_size,
            stats->pool[i].free_list_length);
  }

  fprintf(stderr, "\n");
}
#endif

size_t ssm_input_consume(size_t r, size_t w) {
  if (!ssm_input_read_ready(r, w))
    return r;

  ssm_time_t packet_time = ssm_input_get(r)->time.ssm_time;

  if (ssm_next_event_time() < packet_time)
    return r;

  do {
    ssm_sv_later_unsafe(ssm_input_get(r)->sv, packet_time,
                        ssm_input_get(r)->payload);
  } while (ssm_input_read_ready(++r, w) &&
           packet_time == ssm_input_get(r)->time.ssm_time);
  return r;
}

static inline void poll_input_queue(size_t *r, size_t *w) {
  static size_t scaled = 0;
  pthread_mutex_lock(&rb_lk);
  *r = atomic_load(&rb_r);
  *w = atomic_load(&rb_w);
  pthread_mutex_unlock(&rb_lk);
  for (; scaled < *w; scaled++)
    ssm_input_get(scaled)->time =
        ssm_raw_time64_scale(ssm_input_get(scaled)->time, 1);
}

ssm_time_t next_time, wall_time;
size_t r, w;

int main(void) {
  ssm_mem_init(alloc_page, alloc_mem, free_mem);

  pipe(ssm_sem_fd);
  pthread_mutex_init(&rb_lk, NULL);

  struct timespec init_time;
  clock_gettime(CLOCK_MONOTONIC, &init_time);
  ssm_set_now(timespec_time(init_time));

  ssm_value_t ssm_stdin = ssm_new_sv(ssm_marshal((uint32_t)0));
  ssm_dup(ssm_stdin); // dup because this is passed to stdin_handler
  ssm_value_t ssm_stdout = ssm_new_sv(ssm_marshal((uint32_t)0));
  ssm_dup(ssm_stdout); // dup because this is passed to stdout_handler
  ssm_value_t std_argv[2] = {ssm_stdin, ssm_stdout};

  ssm_act_t *stdout_act = __enter_stdout_handler(
      &ssm_top_parent, SSM_ROOT_PRIORITY + 0 * (1 << (SSM_ROOT_DEPTH - 1)),
      SSM_ROOT_DEPTH - 1, &ssm_stdout, NULL);
  ssm_activate(stdout_act);

  ssm_activate(__enter_main(&ssm_top_parent,
                            SSM_ROOT_PRIORITY + 1 * (1 << (SSM_ROOT_DEPTH - 1)),
                            SSM_ROOT_DEPTH - 1, std_argv, NULL));
  __spawn_stdin_handler(ssm_to_sv(ssm_stdin));

  ssm_tick();
  int ret = 0;

#if 1
  for (;;) {
    // ssm_time_t next_time, wall_time;
    struct timespec wall_spec;
    // size_t r, w;

    DBG("before getting wall time\n");
    clock_gettime(CLOCK_MONOTONIC, &wall_spec);
    wall_time = timespec_time(wall_spec);
    next_time = ssm_next_event_time();

    poll_input_queue(&r, &w);
    if (ssm_input_read_ready(r, w)) {
      if (ssm_input_get(r)->time.ssm_time <= next_time) {
        DBG("Consuming input of time: %ld\n", ssm_input_get(r)->time.ssm_time);
        r = ssm_input_consume(r, w);
        atomic_store(&rb_r, r);
        goto do_tick;
      }
    }

    if (next_time <= wall_time) {
    do_tick:
      ssm_tick();
    } else {
      fd_set in_fds;
      FD_SET(ssm_sem_fd[0], &in_fds);
      if (next_time == SSM_NEVER) {
        if (!ssm_active())
          break;
        DBG("Sleeping indefinitely\n");
        ret = pselect(ssm_sem_fd[0] + 1, &in_fds, NULL, NULL, NULL, NULL);
        DBG("Woke from sleeping indefinitely\n");
        fd_sem_read(ssm_sem_fd);
      } else {
        struct timespec next_spec = timespec_of(next_time);
        struct timespec sleep_time = timespec_diff(next_spec, wall_spec);
        DBG("Sleeping\n");
        ret =
            pselect(ssm_sem_fd[0] + 1, &in_fds, NULL, NULL, &sleep_time, NULL);
        DBG("Woke up from sleeping\n");
        if (ret > 0)
          fd_sem_read(ssm_sem_fd);
        // otherwise, timed out
      }
    }
  }

#else
  for (;;) {
    size_t r, w;
    poll_input_queue(&r, &w);
  consume_loop:
    do {
      r = ssm_input_consume(r, w);
      atomic_store(&rb_r, r);
      ssm_tick();
    } while (ssm_input_read_ready(r, w));

    fflush(stdout);

    fd_set in_fds;
    FD_SET(ssm_sem_fd, &in_fds);

    ssm_time_t next = ssm_next_event_time();
    if (next == SSM_NEVER) {
      // printf("%s: sleeping indefinitely\n", __FUNCTION__);
      pselect(ssm_sem_fd + 1, &in_fds, NULL, NULL, NULL, NULL);
    } else {
      struct timespec next_time = timespec_of(next);
      struct timespec wall_time;
      clock_gettime(CLOCK_MONOTONIC, &wall_time);

      poll_input_queue(&r, &w);
      if (ssm_input_read_ready(r, w))
        goto consume_loop;

      // printf("%s: next time is %ld (currently (%ld)\n", __FUNCTION__,
      //        timespec_time(next_time), timespec_time(wall_time));
      if (timespec_lt(wall_time, next_time)) {
        struct timespec sleep_time = timespec_diff(next_time, wall_time);
        // printf("%s: sleeping for %ld\n", __FUNCTION__,
        //        timespec_time(sleep_time));
        pselect(ssm_sem_fd + 1, &in_fds, NULL, NULL, &sleep_time, NULL);
      }
    }
  }
#endif

  DBG("Broke out of main loop, quitting\n");

  // FIXME: hack to force the stdout_handler function to terminate
  // and free its activation record
  stdout_act->pc = 2; // FIXME: must match the code in step_stdout_handler()
  ssm_activate(stdout_act);
  ssm_tick();

  __kill_stdin_handler();
  ssm_drop(ssm_stdin); // FIXME: should probably be part of __kill_stdin_handler

#ifdef CONFIG_MEM_STATS
  ssm_mem_statistics_t stats;
  print_mem_stats(&stats);
  if (stats.live_objects) {
    fprintf(stderr, "FAILED: %lu live objects leaked at the end\n",
            stats.live_objects);
    exit(1);
  }
#endif

  for (size_t p = 0; p < allocated_pages; p++)
    free(pages[p]);

  return ret;
}
