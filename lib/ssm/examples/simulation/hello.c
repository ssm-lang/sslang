#include "ssm-examples.h"
#include <stdio.h>

typedef struct {
  ssm_act_t act;
  ssm_u8_t stdout;
} main_act_t;

typedef struct {
  ssm_act_t act;
  ssm_u8_t stdout;
  ssm_trigger_t trigger1;
} hello_act_t;

typedef struct {
  ssm_act_t act;
  ssm_u8_t stdout;
  ssm_trigger_t trigger1;
} print_act_t;

ssm_stepf_t step_main, step_hello, step_print;

ssm_act_t *enter_hello(ssm_act_t *parent, ssm_priority_t priority,
                       ssm_depth_t depth, ssm_u8_t stdout) {

  hello_act_t *cont = container_of(
      ssm_enter(sizeof(hello_act_t), step_hello, parent, priority, depth),
      hello_act_t, act);
  cont->trigger1.act = &cont->act;
  cont->stdout = stdout;
  return &cont->act;
}

void step_hello(ssm_act_t *act) {
  hello_act_t *cont = container_of(act, hello_act_t, act);
  switch (act->pc) {
  case 0:

#define print_yield(p, c)                                                      \
  ssm_later(cont->stdout, ssm_now() + 10, ssm_marshal(c));                     \
  ssm_sensitize(cont->stdout, &cont->trigger1);                     \
  act->pc = p;                                                                 \
  return;                                                                      \
  case p:                                                                      \
    ssm_desensitize(&cont->trigger1)

    print_yield(1, 'H');
    print_yield(2, 'e');
    print_yield(3, 'l');
    print_yield(4, 'l');
    print_yield(5, 'o');
    print_yield(6, ' ');
    print_yield(7, 'w');
    print_yield(8, 'o');
    print_yield(9, 'r');
    print_yield(10, 'l');
    print_yield(11, 'd');
    print_yield(12, '!');
    print_yield(13, '\r');
    print_yield(14, '\n');
    print_yield(15, 0);
  }
  ssm_drop(cont->stdout);
  ssm_leave(&cont->act, sizeof(hello_act_t));
}

ssm_act_t *enter_print(ssm_act_t *parent, ssm_priority_t priority,
                       ssm_depth_t depth, ssm_u8_t stdout) {
  print_act_t *cont = container_of(
      ssm_enter(sizeof(print_act_t), step_print, parent, priority, depth),
      print_act_t, act);
  cont->stdout = stdout;
  cont->trigger1.act = &cont->act;
  return &cont->act;
}

void step_print(ssm_act_t *act) {
  print_act_t *cont = container_of(act, print_act_t, act);

  switch (act->pc) {
  case 0:
    for (;;) {
      ssm_sensitize(cont->stdout, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger1);
      char c = ssm_unmarshal(ssm_deref(cont->stdout));
      if (c) {
        putchar(c);
      } else {
        break;
      }
    }
  }
  ssm_drop(cont->stdout);
  ssm_leave(&cont->act, sizeof(hello_act_t));
}

ssm_act_t *enter_main(ssm_act_t *parent, ssm_priority_t priority,
                      ssm_depth_t depth) {

  main_act_t *cont = container_of(
      ssm_enter(sizeof(main_act_t), step_main, parent, priority, depth),
      main_act_t, act);
  return &cont->act;
}

void step_main(ssm_act_t *act) {
  main_act_t *cont = container_of(act, main_act_t, act);
  switch (act->pc) {
  case 0:
    cont->stdout = ssm_new_sv(ssm_marshal(0));
    ssm_depth_t new_depth = act->depth - 1;
    ssm_priority_t new_priority = act->priority;
    ssm_priority_t pinc = 1 << new_depth;
    ssm_dup(cont->stdout);
    ssm_activate(enter_hello(act, new_priority, new_depth, cont->stdout));
    new_priority += pinc;
    ssm_dup(cont->stdout);
    ssm_activate(enter_print(act, new_priority, new_depth, cont->stdout));
    act->pc = 1;
    return;
  }
  ssm_leave((ssm_act_t *)act, sizeof(main_act_t));
}

void ssm_program_init(void) {
  ssm_act_t *act =
      enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH);
  ssm_activate(act);
}

void ssm_program_exit(void) {}
