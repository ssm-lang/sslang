#include "ssm.h"
#include <stdio.h>

typedef struct {
  SSM_ACT_FIELDS;
  ssm_u8_t stdout;
} main_act_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_u8_t *stdout;
  char *ptr;
  char nextc;
  ssm_trigger_t trigger1;
} hello_act_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_u8_t *stdout;
  ssm_trigger_t trigger1;
} print_act_t;

ssm_stepf_t step_main, step_hello, step_print;

hello_act_t *enter_hello(ssm_act_t *parent, ssm_priority_t priority,
			 ssm_depth_t depth, ssm_u8_t *stdout)
{
  hello_act_t *act = (hello_act_t *)
    ssm_enter(sizeof(hello_act_t), step_hello, parent, priority, depth);
  act->trigger1.act = (ssm_act_t *) act;
  act->stdout = stdout;
  return act;
}

void step_hello(ssm_act_t *sact)
{
  static char hello_string[] = "Hello World!\n";
  
  hello_act_t *act = (hello_act_t *) sact;

  switch (act->pc) {
  case 0:
    act->ptr = hello_string;
    ssm_sensitize((ssm_sv_t *) act->stdout, &act->trigger1);
    do {
      act->nextc = *(act->ptr);
      ssm_later_u8(act->stdout, ssm_now() + 10, act->nextc);
      act->pc = 1;
      return;

    case 1:
      ++act->ptr;
    } while (act->nextc != 0);
    ssm_desensitize(&act->trigger1);
    ssm_leave((ssm_act_t *) act, sizeof(hello_act_t));
    return;
  }
}

print_act_t *enter_print(ssm_act_t *parent, ssm_priority_t priority,
			 ssm_depth_t depth, ssm_u8_t *stdout)
{
  print_act_t *act = (print_act_t *)
    ssm_enter(sizeof(print_act_t), step_print, parent, priority, depth);
  act->stdout = stdout;
  act->trigger1.act = (ssm_act_t *) act;
  return act;
}

void step_print(ssm_act_t *sact)
{
  print_act_t *act = (print_act_t *) sact;

  switch (act->pc) {
  case 0:
    ssm_sensitize((ssm_sv_t *) act->stdout, &act->trigger1);
    for (;;) {
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((ssm_sv_t *) act->stdout)) {
	if (act->stdout->value == 0) {
	  ssm_desensitize(&act->trigger1);
	  ssm_leave((ssm_act_t *) act, sizeof(print_act_t));
	  return;
	}
	putchar(act->stdout->value);	
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
					     &act->stdout));
      new_priority += pinc;
      ssm_activate((ssm_act_t *) enter_print(sact, new_priority, new_depth,
					     &act->stdout));
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
  do ssm_tick();
  while (ssm_next_event_time() != SSM_NEVER);      

  return 0;
}
