#include <ssm-internal.h>
#include <ssm-platform.h>

#include <platform/zephyr-timer.h>

#include <drivers/gpio.h>
#include <kernel.h>

K_SEM_DEFINE(ssm_tick_sem, 0, 1);

#ifndef SSM_INPUT_RB_SIZE
#define SSM_INPUT_RB_SIZE 32
#endif

#ifndef SSM_NUM_PAGES
#define SSM_NUM_PAGES 32
#endif

typedef union {
  ssm_raw_time_t raw;
  ssm_time_t ssm;
} ssm_platform_time_t;

struct ssm_input {
  ssm_sv_t *sv;
  ssm_value_t payload;
  ssm_platform_time_t time;
};

struct ssm_input ssm_input_rb[SSM_INPUT_RB_SIZE];

#define ssm_input_idx(i) ((i) % SSM_INPUT_RB_SIZE)
#define ssm_input_get(i) (&ssm_input_rb[ssm_input_idx(i)])

#define ssm_input_read_ready(r, w) (ssm_input_idx(r) != ssm_input_idx(w))
#define ssm_input_write_ready(r, w) (ssm_input_read_ready(r, w + 1))

atomic_t rb_r;
atomic_t rb_w;

static void sem_post(void) { k_sem_give(&ssm_tick_sem); }

static void sem_wait(void) { k_sem_take(&ssm_tick_sem, K_FOREVER); }

static void sem_reset(void) { k_sem_reset(&ssm_tick_sem); }

static void send_timeout_event(ssm_time_t t, void *d) { sem_post(); }

static inline void poll_input_queue(size_t *r, size_t *w) {
  static size_t scaled = 0;
  *r = atomic_get(&rb_r);
  *w = atomic_get(&rb_w);

  for (; scaled < *w; scaled++)
    // Calculate
    ssm_input_get(scaled)->time.ssm =
        ssm_timer_calc(ssm_input_get(scaled)->time.raw);
}

static inline size_t consume_input_queue(size_t r, size_t w) {
  if (!ssm_input_read_ready(r, w))
    return r;

  ssm_time_t packet_time = ssm_input_get(r)->time.ssm;

  if (ssm_next_event_time() < packet_time)
    return r;

  do {
    ssm_sv_later_unsafe(ssm_input_get(r)->sv, packet_time,
                        ssm_input_get(r)->payload);
  } while (ssm_input_read_ready(++r, w) &&
           packet_time == ssm_input_get(r)->time.ssm);
  return r;
}

int ssm_insert_input(ssm_sv_t *sv, ssm_value_t val) {
  size_t w, r;

  ssm_raw_time_t raw_time;
  ssm_timer_read_raw(&raw_time);

  w = atomic_get(&rb_w);
  r = atomic_get(&rb_r);

  if (ssm_input_write_ready(r, w)) {
    struct ssm_input *pkt = ssm_input_get(w);

    pkt->sv = sv;
    pkt->payload = val;
    pkt->time.raw = raw_time;

    atomic_set(&rb_w, w + 1);

    sem_post();

    return 0;
  } else {
    return -1;
  }
}

static void setup_entry_point(void) {
  // NOTE: this is kind of a hack. Ideally the user program is able to specify
  // what it wants the entry point to be. But for now, we assume that there's
  // some entry point named 'main' whose activation record is set up using
  // '__enter_main'.
  extern ssm_act_t *__enter_main(ssm_act_t *, ssm_priority_t, ssm_depth_t,
                                 ssm_value_t *, ssm_value_t *);
  ssm_value_t nothing0 = ssm_new_sv(ssm_marshal(0));
  ssm_value_t nothing1 = ssm_new_sv(ssm_marshal(0));
  ssm_value_t main_argv[2] = {nothing0, nothing1};

  /* Start up main routine */
  ssm_activate(__enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH,
                            main_argv, NULL));
}

static void tick_loop(void) {
  ssm_timer_start();

  setup_entry_point();

  ssm_tick(); // NOTE: we are assuming no input events at time 0.

  for (;;) {
    ssm_time_t next_time, wall_time;

    wall_time = ssm_timer_read();
    next_time = ssm_next_event_time();

    compiler_barrier(); // Must read timer before reading from input queue

    size_t r, w;

    poll_input_queue(&r, &w);

    if (ssm_input_read_ready(r, w)) {
      if (ssm_input_get(r)->time.ssm <= next_time) {
        r = consume_input_queue(r, w);
        atomic_set(&rb_r, r);
        goto do_tick;
      }
    }

    if (next_time <= wall_time) {
    do_tick:
      ssm_tick();

    } else {

      if (next_time == SSM_NEVER) {
        // Nothing to do; sleep indefinitely

        // NOTE: we should only break if (1) there are no more active processes
        // AND (2) there are no more possible inputs.
        //
        // if (!ssm_active())
        //   break;

        sem_wait();

      } else {
        // Need to set alarm

        int err = ssm_timer_set_alarm(next_time, send_timeout_event, NULL);
        if (err < 0) {
          printk("Encountered error: %d!!\r\n", err);
          printk("wall_time: %llx!!\r\n", wall_time);
          printk("next_time: %llx!!\r\n", next_time);
          SSM_THROW(SSM_INTERNAL_ERROR);
          return; // unreachable
        }

        sem_wait();

        // Cancel any potential pending alarm if it hasn't gone off yet.
        ssm_timer_cancel();

        // It's possible that the alarm went off before we cancelled it; make
        // sure that its sem_give doesn't cause premature wake-up later on.
        sem_reset();
      }
    }
  }
}

static char mem[SSM_NUM_PAGES][SSM_MEM_PAGE_SIZE] = {0};
static size_t allocated_pages = 0;

static void *alloc_page(void) {
  if (allocated_pages >= SSM_NUM_PAGES)
    SSM_THROW(SSM_EXHAUSTED_MEMORY);
  return mem[allocated_pages++];
}

static void *alloc_mem(size_t size) { return malloc(size); }

static void free_mem(void *mem, size_t size) { free(mem); }

int ssm_platform_entry(void) {
  ssm_mem_init(alloc_page, alloc_mem, free_mem);

  tick_loop();
  return 0;
}
