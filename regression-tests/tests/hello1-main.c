#include "ssm.h"
#include <stdio.h>

typedef struct {
  SSM_ACT_FIELDS;
  ssm_i32_t *cout;
  ssm_trigger_t trigger1;
} cout_act_t;

void step_cout(ssm_act_t *sact);
cout_act_t *enter_cout(ssm_act_t *parent, ssm_priority_t priority,
			 ssm_depth_t depth, ssm_i32_t *cout)
{
  cout_act_t *act = (cout_act_t *)
    ssm_enter(sizeof(cout_act_t), step_cout, parent, priority, depth);
  act->cout = cout;
  act->trigger1.act = (ssm_act_t *) act;
  return act;
}

void step_cout(ssm_act_t *sact)
{
  cout_act_t *act = (cout_act_t *) sact;

  switch (act->pc) {
  case 0:
    ssm_sensitize((ssm_sv_t *) act->cout, &act->trigger1);
    for (;;) {
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((ssm_sv_t *) act->cout)) {
	if (act->cout->value == 0) {
	  ssm_desensitize(&act->trigger1);
	  ssm_leave((ssm_act_t *) act, sizeof(cout_act_t));
	  return;
	}
	putchar(act->cout->value);
      }
    }
  }
}

struct ssm_act *enter_main(struct ssm_act *caller,
			   ssm_priority_t priority, ssm_depth_t depth,
			   ssm_i32_t *cout);

int main()  
{
  ssm_i32_t cout;
  ssm_initialize_i32(&cout);
  cout.value = 0;

  {
    ssm_depth_t new_depth = SSM_ROOT_DEPTH - 1; // 2 children
    ssm_priority_t new_priority = SSM_ROOT_PRIORITY;
    ssm_priority_t pinc = 1 << new_depth;
    ssm_activate((ssm_act_t *) enter_main(&ssm_top_parent,
					  new_priority, new_depth,
					  &cout));
    new_priority += pinc;
    ssm_activate((ssm_act_t *) enter_cout(&ssm_top_parent,
					  new_priority, new_depth,
					  &cout));
  }

  do ssm_tick(); while (ssm_next_event_time() != SSM_NEVER);  
  return 0;
}
