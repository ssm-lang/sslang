#include "ssm-examples.h"
typedef struct {
  ssm_act_t act;
  ssm_value_t ssm_stdin;
  ssm_value_t ssm_stdout;
  ssm_trigger_t trigger1;
} main_act_t;

ssm_stepf_t step_main;

ssm_act_t *enter_main(ssm_act_t *parent, ssm_priority_t priority,
                      ssm_depth_t depth, ssm_value_t *argv, ssm_value_t *ret) {

  main_act_t *cont = container_of(
      ssm_enter(sizeof(main_act_t), step_main, parent, priority, depth),
      main_act_t, act);
  cont->ssm_stdin = argv[0];
  cont->ssm_stdout = argv[1];
  cont->trigger1.act = &cont->act;
  return &cont->act;
}

void step_main(ssm_act_t *act) {
  main_act_t *cont = container_of(act, main_act_t, act);
  switch (act->pc) {
  case 0:
    for (;;) {
      ssm_sensitize(cont->ssm_stdin, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:;
      ssm_desensitize(&cont->trigger1);
      char c = ssm_unmarshal(ssm_deref(cont->ssm_stdin));

      ssm_later(cont->ssm_stdout, ssm_now() + (NANOS / 2),
                ssm_marshal(c >= 'a' && c <= 'z' ? c + 'A' - 'a' : c));

      ssm_sensitize(cont->ssm_stdout, &cont->trigger1);
      act->pc = 2;
      return;
    case 2:;
      ssm_later(cont->ssm_stdout, ssm_now() + (NANOS / 1000),
                ssm_marshal('\n'));
      act->pc = 3;
      return;
    case 3:;
      ssm_desensitize(&cont->trigger1);
      // printf("%s: ready to accept more input (missed latest: %c)\n",
      //        __FUNCTION__, ssm_unmarshal(ssm_deref(ssm_stdin)));
    }
  }
  ssm_leave(act, sizeof(main_act_t));
}

void ssm_program_init(void) {
  ssm_value_t ssm_stdin = ssm_new_sv(ssm_marshal(0));
  ssm_value_t ssm_stdout = ssm_new_sv(ssm_marshal(0));

  ssm_activate(__enter_stdout_handler(&ssm_top_parent, SSM_ROOT_PRIORITY,
                                      SSM_ROOT_DEPTH - 1, &ssm_stdout, NULL));

  ssm_value_t argv_main[2] = {ssm_stdin, ssm_stdout};
  ssm_activate(enter_main(&ssm_top_parent,
                          SSM_ROOT_PRIORITY + (1 << (SSM_ROOT_DEPTH - 1)),
                          SSM_ROOT_DEPTH - 1, argv_main, NULL));

  __spawn_stdin_handler(ssm_to_sv(ssm_stdin));
}

void ssm_program_exit(void) {
  printf("DBG: joining stdin handler\n");
  __kill_stdin_handler();
}
