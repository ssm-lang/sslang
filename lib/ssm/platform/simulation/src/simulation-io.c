#include <ssm-internal.h>
#include <stdio.h>

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
    putchar(c);
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

void __spawn_stdin_handler(ssm_sv_t *ssm_stdin) {
  /* For now, simulation does not support stdin, so do nothing. */
}
void __kill_stdin_handler(void) {
  /* Nothing to do, nothing to kill. */
}
