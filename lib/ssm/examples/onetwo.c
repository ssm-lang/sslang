#include "ssm-examples.h"
#include <stdio.h>
#include <stdlib.h>

/*
 * one a =
 *   wait a
 *   a <- deref a + 1
 *
 * two a =
 *   wait a
 *   a <- deref a * 2
 *
 * main =
 *   let a = new 0
 *   after 1s, a <- 10
 *   par one a
 *       two a
 *   // a = 22 here
 */

typedef struct {
  ssm_act_t act;
  ssm_i32_t a;
  struct ssm_trigger trigger1;
} act_one_t;

typedef struct {
  ssm_act_t act;
  ssm_i32_t a;
  struct ssm_trigger trigger1;
} act_two_t;

typedef struct {
  ssm_act_t act;
  ssm_i32_t a;
} act_main_t;

ssm_stepf_t step_one;

ssm_act_t *ssm_enter_one(struct ssm_act *parent, ssm_priority_t priority,
                         ssm_depth_t depth, ssm_i32_t a) {
  act_one_t *cont = container_of(
      ssm_enter(sizeof(act_one_t), step_one, parent, priority, depth),
      act_one_t, act);
  cont->trigger1.act = &cont->act;
  cont->a = a;

  return &cont->act;
}

void step_one(struct ssm_act *act) {
  act_one_t *cont = container_of(act, act_one_t, act);

  switch (act->pc) {
  case 0:
    ssm_sensitize(cont->a, &cont->trigger1);
    act->pc = 1;
    return;
  case 1:
    ssm_desensitize(&cont->trigger1);
    ssm_assign(cont->a, act->priority,
               ssm_marshal(ssm_unmarshal(ssm_deref(cont->a)) + 1));
  }
  ssm_drop(cont->a);
  ssm_leave(act, sizeof(act_one_t));
}

ssm_stepf_t step_two;

ssm_act_t *ssm_enter_two(struct ssm_act *parent, ssm_priority_t priority,
                         ssm_depth_t depth, ssm_i32_t a) {
  act_two_t *cont = container_of(
      ssm_enter(sizeof(act_two_t), step_two, parent, priority, depth),
      act_two_t, act);
  cont->trigger1.act = &cont->act;
  cont->a = a;

  return &cont->act;
}

void step_two(struct ssm_act *act) {
  act_two_t *cont = container_of(act, act_two_t, act);
  switch (act->pc) {
  case 0:
    ssm_sensitize(cont->a, &cont->trigger1);
    act->pc = 1;
    return;
  case 1:
    ssm_desensitize(&cont->trigger1);
    ssm_assign(cont->a, act->priority,
               ssm_marshal(ssm_unmarshal(ssm_deref(cont->a)) * 2));
  }
  ssm_drop(cont->a);
  ssm_leave(act, sizeof(act_two_t));
}

ssm_stepf_t step_main;

ssm_act_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth) {
  act_main_t *cont = container_of(
      ssm_enter(sizeof(act_main_t), step_main, parent, priority, depth),
      act_main_t, act);
  return &cont->act;
}

void step_main(struct ssm_act *act) {
  act_main_t *cont = container_of(act, act_main_t, act);
  switch (act->pc) {
  case 0:
    cont->a = ssm_new_sv(ssm_marshal(0));

    ssm_later(cont->a, ssm_now() + 100, ssm_marshal(10));
    {
      ssm_depth_t new_depth = act->depth - 1; // 2 children
      ssm_priority_t new_priority = act->priority;
      ssm_priority_t pinc = 1 << new_depth;

      ssm_dup(cont->a);
      ssm_activate(ssm_enter_one(act, new_priority, new_depth, cont->a));

      new_priority += pinc;
      ssm_dup(cont->a);
      ssm_activate(ssm_enter_two(act, new_priority, new_depth, cont->a));
    }
    act->pc = 1;
    return;
  case 1:
    printf("a = %d\n", (int)ssm_unmarshal(ssm_deref(cont->a)));
  }
  ssm_drop(cont->a);
  ssm_leave(act, sizeof(act_main_t));
}

void ssm_program_init(void) {
  ssm_activate(
      ssm_enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));
}

void ssm_program_exit(void) {}
