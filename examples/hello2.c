#include "ssm.h"
#include <stdio.h>

typedef struct {
  SSM_ACT_FIELDS;
  ssm_u8_t stdout;
  ssm_event_t done;
} main_act_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_u8_t *stdout;
  ssm_event_t *done;
  char *ptr;
  ssm_trigger_t trigger1;
} hello_act_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_u8_t *stdout;
  ssm_event_t *done;
  ssm_trigger_t trigger1, trigger2;
} print_act_t;

ssm_stepf_t step_main, step_hello, step_print;

hello_act_t *enter_hello(ssm_act_t *parent, ssm_priority_t priority,
			 ssm_depth_t depth, ssm_u8_t *stdout, ssm_event_t *done)
{
  hello_act_t *act = (hello_act_t *)
    ssm_enter(sizeof(hello_act_t), step_hello, parent, priority, depth);
  act->trigger1.act = (ssm_act_t *) act;
  act->stdout = stdout;
  act->done = done;
  return act;
}

void step_hello(ssm_act_t *sact)
{
  static char hello_string[] = "Hello World!\n";
  u8 nextc;
  
  hello_act_t *act = (hello_act_t *) sact;

  switch (act->pc) {
  case 0:
    act->ptr = hello_string;
    ssm_sensitize((ssm_sv_t *) act->stdout, &act->trigger1);
    while ( (nextc = *(act->ptr)) != 0) {
      ssm_later_u8(act->stdout, ssm_now() + 10, nextc);
      act->pc = 1;
      return;

    case 1:
      ++act->ptr;
    }
    ssm_assign_event(act->done, act->priority);
    ssm_desensitize(&act->trigger1);
    ssm_leave((ssm_act_t *) act, sizeof(hello_act_t));
    return;
  }
}

print_act_t *enter_print(ssm_act_t *parent, ssm_priority_t priority,
			 ssm_depth_t depth, ssm_u8_t *stdout, ssm_event_t *done)
{
  print_act_t *act = (print_act_t *)
    ssm_enter(sizeof(print_act_t), step_print, parent, priority, depth);
  act->stdout = stdout;
  act->done = done;
  act->trigger1.act = act->trigger2.act = (ssm_act_t *) act;
  return act;
}

void step_print(ssm_act_t *sact)
{
  print_act_t *act = (print_act_t *) sact;

  switch (act->pc) {
  case 0:
    ssm_sensitize((ssm_sv_t *) act->stdout, &act->trigger1);
    ssm_sensitize((ssm_sv_t *) act->done, &act->trigger2);
    for (;;) {
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((ssm_sv_t *) act->stdout))
	putchar(act->stdout->value);
      if (ssm_event_on(&act->done->sv)) {
	ssm_desensitize(&act->trigger1);
	ssm_desensitize(&act->trigger2);
	ssm_leave((ssm_act_t *) act, sizeof(print_act_t));
	return;
      }
    }
  }
}

main_act_t *enter_main(ssm_act_t *parent, ssm_priority_t priority,
		       ssm_depth_t depth)
{
  main_act_t *act = (main_act_t *)
    ssm_enter(sizeof(main_act_t), step_main, parent, priority, depth);
  ssm_initialize_u8(&act->stdout);
  ssm_initialize_event(&act->done);
  return act;
}

void step_main(ssm_act_t *sact)
{
  main_act_t *act = (main_act_t *) sact;
  switch (act->pc) {
  case 0:
    ssm_assign_u8(&act->stdout, act->priority, 0);
    {
      ssm_depth_t new_depth = act->depth - 1; // 2 children
      ssm_priority_t new_priority = act->priority;
      ssm_priority_t pinc = 1 << new_depth;
      ssm_activate((ssm_act_t *) enter_hello(sact, new_priority, new_depth,
					     &act->stdout, &act->done));
      new_priority += pinc;
      ssm_activate((ssm_act_t *) enter_print(sact, new_priority, new_depth,
					     &act->stdout, &act->done));
    }
    act->pc = 1;
    return;    
  case 1:
    ssm_leave((ssm_act_t *) act, sizeof(main_act_t));
    return;
  }
}

int main()
{
  ssm_activate( (ssm_act_t *)
		enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));

  do
    ssm_tick();
  while (ssm_next_event_time() != SSM_NEVER);      

  return 0;
}

// FIXME: Change to pass the 0 through to indicate the end-of-string
