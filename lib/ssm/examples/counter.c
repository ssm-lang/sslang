#include "ssm-examples.h"
#include <stdio.h>
#include <stdlib.h>

/*  Synchronous counter example, like scheduler-1/counter3.c
clock =
  loop
    after 100ms, clk = 1
    wait clk
    after 100ms, clk = 0
    wait clk

dff clk q d =
  loop
    wait clk
    if deref clk == 1
      q <- deref d

incr q2 d2 =
  loop
    wait q2
    d2 <- deref q2 + 1

adder d1 d2 q1 q2 =
  loop
    wait q2
         d2
    d1 <- deref q1 + deref d2

main =
  let clk = ref 0
  let q1 = ref 0
  let q2 = ref 0
  let d1 = ref 0
  let d2 = ref 0
  par clock clk
      dff q1 d1
      dff q2 d2
      incr q2 d2
      adder d1 d2 q2 q2
*/

typedef struct {
  ssm_act_t act;
  ssm_i32_t clk;
  ssm_i32_t d1;
  ssm_i32_t q1;
  ssm_i32_t d2;
  ssm_i32_t q2;
} act_main_t;

typedef struct {
  ssm_act_t act;
  ssm_i32_t clk;
  struct ssm_trigger trigger1;
} act_clock_t;

typedef struct {
  ssm_act_t act;
  ssm_i32_t clk;
  ssm_i32_t d1;
  ssm_i32_t q1;
  struct ssm_trigger trigger1;
} act_dff1_t;

typedef struct {
  ssm_act_t act;
  ssm_i32_t clk;
  ssm_i32_t d2;
  ssm_i32_t q2;
  struct ssm_trigger trigger1;
} act_dff2_t;

typedef struct {
  ssm_act_t act;
  ssm_i32_t d2;
  ssm_i32_t q2;
  struct ssm_trigger trigger1;
} act_incr_t;

typedef struct {
  ssm_act_t act;
  ssm_i32_t d1;
  ssm_i32_t q1;
  ssm_i32_t d2;
  ssm_i32_t q2;
  struct ssm_trigger trigger1;
  struct ssm_trigger trigger2;
} act_adder_t;

ssm_stepf_t step_clock;

// Create a new activation for clk
ssm_act_t *ssm_enter_clock(struct ssm_act *parent, ssm_priority_t priority,
                           ssm_depth_t depth, ssm_i32_t clk) {
  act_clock_t *cont = container_of(
      ssm_enter(sizeof(act_clock_t), step_clock, parent, priority, depth),
      act_clock_t, act);
  cont->clk = clk;
  cont->trigger1.act = &cont->act;
  return &cont->act;
}

void step_clock(struct ssm_act *act) {
  act_clock_t *cont = container_of(act, act_clock_t, act);

  // printf("step_clk @%d\n", act->pc);
  printf("clk = %d\n", (int)ssm_unmarshal(ssm_deref(cont->clk)));

  switch (act->pc) {
  case 0:
    for (;;) {
      ssm_later(cont->clk, ssm_now() + (2 * SSM_SECOND), ssm_marshal(1));
      ssm_sensitize(cont->clk, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger1);
      ssm_later(cont->clk, ssm_now() + (2 * SSM_SECOND), ssm_marshal(0));
      ssm_sensitize(cont->clk, &cont->trigger1);
      act->pc = 2;
      return;
    case 2:
      ssm_desensitize(&cont->trigger1);
    }
  }
  ssm_leave(act, sizeof(act_clock_t));
}

ssm_stepf_t step_dff1;

ssm_act_t *ssm_enter_dff1(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth, ssm_i32_t clk, ssm_i32_t q1,
                          ssm_i32_t d1) {
  act_dff1_t *cont = container_of(
      ssm_enter(sizeof(act_dff1_t), step_dff1, parent, priority, depth),
      act_dff1_t, act);

  cont->clk = clk;
  cont->q1 = q1;
  cont->d1 = d1;
  cont->trigger1.act = &cont->act;

  return &cont->act;
}

void step_dff1(struct ssm_act *act) {
  act_dff1_t *cont = container_of(act, act_dff1_t, act);

  /* printf("step_dff1 @%d\n", act->pc); */
  printf("q1 = %d d1 = %d\n", (int)ssm_unmarshal(ssm_deref(cont->q1)),
         (int)ssm_unmarshal(ssm_deref(cont->d1)));

  switch (act->pc) {
  case 0:
    for (;;) {
      if (ssm_unmarshal(ssm_deref(cont->clk)))
        ssm_assign(cont->q1, act->priority, ssm_deref(cont->d1));
      ssm_sensitize(cont->clk, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger1);
    }
  }
  ssm_leave(act, sizeof(act_dff1_t));
}

ssm_stepf_t step_dff2;

ssm_act_t *ssm_enter_dff2(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth, ssm_i32_t clk, ssm_i32_t q2,
                          ssm_i32_t d2) {

  act_dff2_t *cont = container_of(
      ssm_enter(sizeof(act_dff2_t), step_dff2, parent, priority, depth),
      act_dff2_t, act);

  cont->clk = clk;
  cont->q2 = q2;
  cont->d2 = d2;

  cont->trigger1.act = &cont->act;

  return &cont->act;
}

void step_dff2(struct ssm_act *act) {
  act_dff2_t *cont = container_of(act, act_dff2_t, act);

  // printf("step_dff2 @%d\n", act->pc);
  printf("q2 = %d d2 = %d\n", (int)ssm_unmarshal(ssm_deref(cont->q2)),
         (int)ssm_unmarshal(ssm_deref(cont->d2)));

  switch (act->pc) {
  case 0:
    for (;;) {
      if ((int)ssm_unmarshal(ssm_deref(cont->clk)))
        ssm_assign(cont->q2, act->priority, ssm_deref(cont->d2));
      ssm_sensitize(cont->clk, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger1);
    }
  }
  ssm_leave(act, sizeof(act_dff2_t));
}

ssm_stepf_t step_incr;

ssm_act_t *ssm_enter_incr(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth, ssm_i32_t q2, ssm_i32_t d2) {
  act_incr_t *cont = container_of(
      ssm_enter(sizeof(act_incr_t), step_incr, parent, priority, depth),
      act_incr_t, act);

  cont->d2 = d2;
  cont->q2 = q2;
  cont->trigger1.act = &cont->act;

  return &cont->act;
}

void step_incr(struct ssm_act *act) {
  act_incr_t *cont = container_of(act, act_incr_t, act);

  switch (act->pc) {
  case 0:
    for (;;) {
      ssm_assign(cont->d2, act->priority,
                 ssm_marshal(ssm_unmarshal(ssm_deref(cont->q2)) + 1));
      ssm_sensitize(cont->q2, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger1);
    }
  }
  ssm_leave(act, sizeof(act_incr_t));
}

ssm_stepf_t step_adder;

ssm_act_t *ssm_enter_adder(struct ssm_act *parent, ssm_priority_t priority,
                           ssm_depth_t depth, ssm_i32_t d1, ssm_i32_t q1,
                           ssm_i32_t d2, ssm_i32_t q2) {

  act_adder_t *cont = container_of(
      ssm_enter(sizeof(act_adder_t), step_adder, parent, priority, depth),
      act_adder_t, act);

  cont->d1 = d1;
  cont->q1 = q1;
  cont->d2 = d2;
  cont->q2 = q2;

  cont->trigger1.act = cont->trigger2.act = &cont->act;

  return &cont->act;
}

void step_adder(struct ssm_act *act) {
  act_adder_t *cont = container_of(act, act_adder_t, act);

  switch (act->pc) {
  case 0:
    for (;;) {
      ssm_assign(cont->d1, act->priority,
                 ssm_marshal(ssm_unmarshal(ssm_deref(cont->q1)) +
                             ssm_unmarshal(ssm_deref(cont->d2))));
      ssm_sensitize(cont->q2, &cont->trigger1);
      ssm_sensitize(cont->d2, &cont->trigger2);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger2);
      ssm_desensitize(&cont->trigger1);
    }
  }
  ssm_leave(act, sizeof(act_adder_t));
}

ssm_stepf_t step_main;

// Create a new activation record for main
ssm_act_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth) {
  act_main_t *cont = container_of(
      ssm_enter(sizeof(act_main_t), step_main, parent, priority, depth),
      act_main_t, act);

  return &cont->act;
}

void step_main(struct ssm_act *act) {
  act_main_t *cont = container_of(act, act_main_t, act);

  // printf("step_main @%d\n", act->pc);

  switch (act->pc) {
  case 0:
    cont->clk = ssm_new_sv(ssm_marshal(0));
    cont->q1 = ssm_new_sv(ssm_marshal(0));
    cont->q2 = ssm_new_sv(ssm_marshal(0));
    cont->d1 = ssm_new_sv(ssm_marshal(0));
    cont->d2 = ssm_new_sv(ssm_marshal(0));

    ssm_depth_t new_depth;
    ssm_priority_t pinc;
    new_depth = act->depth - 3; // Make space for 8 children
    pinc = 1 << new_depth;      // priority increment for each thread

    ssm_activate(
        ssm_enter_clock(act, act->priority + 0 * pinc, new_depth, cont->clk));
    ssm_activate(ssm_enter_dff1(act, act->priority + 1 * pinc, new_depth,
                                cont->clk, cont->q1, cont->d1));
    ssm_activate(ssm_enter_dff2(act, act->priority + 2 * pinc, new_depth,
                                cont->clk, cont->q2, cont->d2));
    ssm_activate(ssm_enter_incr(act, act->priority + 3 * pinc, new_depth,
                                cont->q2, cont->d2));
    ssm_activate(ssm_enter_adder(act, act->priority + 4 * pinc, new_depth,
                                 cont->d1, cont->d2, cont->q1, cont->q2));
    act->pc = 1;
    return;
  case 1:;
  }
  ssm_leave(act, sizeof(act_main_t));
}

void ssm_program_init(void) {
  ssm_act_t *act =
      ssm_enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH);
  ssm_activate(act);
}

void ssm_program_exit(void) {}
