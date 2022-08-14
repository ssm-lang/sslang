#include "posix-common.h"

typedef struct {
  ssm_act_t act;
  ssm_value_t ssm_stdout;
  ssm_trigger_t trigger;
} stdout_handler_act_t;

static void step_stdout_handler(ssm_act_t *act) {
  stdout_handler_act_t *cont = container_of(act, stdout_handler_act_t, act);

  switch (act->pc) {
  case 0:
    ssm_sensitize(cont->ssm_stdout, &cont->trigger);
    act->pc = 1;
    return;
  case 1:;
    char c = ssm_unmarshal(ssm_deref(cont->ssm_stdout));
    if (!c)
      break;
    write(1, &c, 1);
    fsync(1);
    return;
  }
  ssm_leave(&cont->act, sizeof(stdout_handler_act_t));
}

ssm_act_t *__enter_stdout_handler(ssm_act_t *parent, ssm_priority_t priority,
                                  ssm_depth_t depth, ssm_value_t *argv,
                                  ssm_value_t *ret) {
  stdout_handler_act_t *cont =
      container_of(ssm_enter(sizeof(stdout_handler_act_t), step_stdout_handler,
                             parent, priority, depth),
                   stdout_handler_act_t, act);

  cont->ssm_stdout = argv[0];
  cont->trigger.act = &cont->act;
  return &cont->act;
}

void *ssm_stdin_handler(void *sv) {
  for (;;) {
    int c = getchar();
    if (c == EOF) {
      return NULL;
    }

    size_t w, r;
    w = atomic_load(&rb_w);
    r = atomic_load(&rb_r);

    if (ssm_input_write_ready(r, w)) {
      pthread_mutex_lock(&rb_lk);

      struct timespec now;
      clock_gettime(CLOCK_MONOTONIC, &now);
      uint64_t t = timespec_time(now);

      struct ssm_input *packet = ssm_input_get(w);
      packet->sv = sv;
      packet->payload = ssm_marshal(c);
      packet->time.raw_time64 = t;
      atomic_store(&rb_w, w + 1);
      pthread_mutex_unlock(&rb_lk);
      fd_sem_write(ssm_sem_fd);
    } else {
      // fprintf(stderr, "Dropped input packet: %c (r=%ld, w=%ld)\n", c, r, w);
    }
  }
}

static pthread_attr_t ssm_stdin_attr;
static pthread_t ssm_stdin_tid;

void __spawn_stdin_handler(ssm_sv_t *ssm_stdin) {
  pthread_create(&ssm_stdin_tid, &ssm_stdin_attr, ssm_stdin_handler, ssm_stdin);
}

void __kill_stdin_handler(void) {
  pthread_cancel(ssm_stdin_tid);
  pthread_join(ssm_stdin_tid, NULL);
}
