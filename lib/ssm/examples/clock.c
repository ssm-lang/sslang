#include "ssm-examples.h"
#include <ssm.h>
#include <stdio.h>

/* Simple time keeper

second_clock (second_event: &()) =
  let timer = ref ()
  loop
    second_event <- ()
    after 1s, timer <- ()
    wait timer

report_seconds (second_event: &()) =
  let seconds = ref 0
  loop
    wait second_event
    seconds <- deref seconds + 1
    print(deref seconds)

main =
  let second = ref ()
  par second_clock second
      report_seconds second
 */

typedef struct {
  ssm_act_t act;
  ssm_event_t second_event;
  ssm_event_t timer;
  struct ssm_trigger trigger1;
} act_second_clock_t;

typedef struct {
  ssm_act_t act;
  ssm_event_t second_event;
  ssm_i32_t seconds;
  struct ssm_trigger trigger1;
} act_report_seconds_t;

typedef struct {
  ssm_act_t act;
  ssm_event_t second;
} act_main_t;

ssm_stepf_t step_second_clock;

ssm_act_t *ssm_enter_second_clock(struct ssm_act *parent,
                                  ssm_priority_t priority, ssm_depth_t depth,
                                  ssm_value_t second_event) {
  act_second_clock_t *cont =
      container_of(ssm_enter(sizeof(act_second_clock_t), step_second_clock,
                             parent, priority, depth),
                   act_second_clock_t, act);
  cont->second_event = second_event;
  return &cont->act;
}

void step_second_clock(struct ssm_act *act) {
  act_second_clock_t *cont = container_of(act, act_second_clock_t, act);
  switch (act->pc) {
  case 0:
    cont->timer = ssm_new_sv(EVENT_VALUE);
    for (;;) {
      ssm_assign(cont->second_event, act->priority, EVENT_VALUE);
      ssm_later(cont->timer, ssm_now() + SSM_SECOND, EVENT_VALUE);
      cont->trigger1.act = act;
      ssm_sensitize(cont->timer, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger1);
    }
  }
  ssm_drop(cont->timer);
  ssm_drop(cont->second_event);
  ssm_leave(&cont->act, sizeof(*cont));
}

ssm_stepf_t step_report_seconds;

ssm_act_t *ssm_enter_report_seconds(struct ssm_act *parent,
                                    ssm_priority_t priority, ssm_depth_t depth,
                                    ssm_value_t second_event) {
  act_report_seconds_t *cont =
      container_of(ssm_enter(sizeof(act_report_seconds_t), step_report_seconds,
                             parent, priority, depth),
                   act_report_seconds_t, act);
  cont->second_event = second_event;
  return &cont->act;
}

void step_report_seconds(struct ssm_act *act) {
  act_report_seconds_t *cont = container_of(act, act_report_seconds_t, act);

  switch (act->pc) {
  case 0:
    cont->seconds = ssm_new_sv(ssm_marshal(0));
    for (;;) {
      cont->trigger1.act = act;
      ssm_sensitize(cont->second_event, &cont->trigger1);
      act->pc = 1;
      return;
    case 1:
      ssm_desensitize(&cont->trigger1);
      ssm_assign(cont->seconds, act->priority,
                 ssm_marshal(ssm_unmarshal(ssm_deref(cont->seconds)) + 1));

      printf("%d\n", (int)ssm_unmarshal(ssm_deref(cont->seconds)));
    }
  }
  ssm_drop(cont->seconds);
  ssm_drop(cont->second_event);
  ssm_leave(&cont->act, sizeof(*cont));
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

  switch (act->pc) {
  case 0: {
    cont->second = ssm_new_sv(EVENT_VALUE);
    ssm_depth_t new_depth = act->depth - 1;
    ssm_priority_t new_priority = act->priority;
    ssm_priority_t pinc = 1 << new_depth;
    ssm_dup(cont->second);
    ssm_activate(
        ssm_enter_second_clock(act, new_priority, new_depth, cont->second));

    // NOTE: no need to ssm_dup() because we pass away ownership.
    // Correspondingly, there is no ssm_drop() for it at the end before leave.
    new_priority += pinc;
    ssm_activate(
        ssm_enter_report_seconds(act, new_priority, new_depth, cont->second));
  }
    act->pc = 1;
    return;
  case 1:
    break;
  }
  ssm_leave(&cont->act, sizeof(*cont));
}

void ssm_program_init(void) {
  ssm_act_t *act =
      ssm_enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH);
  ssm_activate(act);
}

void ssm_program_exit(void) {}
