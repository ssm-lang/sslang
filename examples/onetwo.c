#include <stdio.h>
#include <stdlib.h>
#include "ssm.h"

/* 
one &a
  wait a
  a = a + 1

two &a
  wait a
  a = a * 2

main
  var a = 0
  after 1s a = 10
  ssm_activate one(a) two(a)
  // a = 22 here


 */

typedef struct {
  SSM_ACT_FIELDS;
  ssm_i32_t *a;
  struct ssm_trigger trigger1;
} rar_one_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_i32_t *a;
  struct ssm_trigger trigger1;
} rar_two_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_i32_t a;
} rar_main_t;

ssm_stepf_t step_one;

rar_one_t *ssm_enter_one(struct ssm_act *cont, ssm_priority_t priority,
		     ssm_depth_t depth, ssm_i32_t *a)
{
  rar_one_t *rar = (rar_one_t *) ssm_enter(sizeof(rar_one_t),
					     step_one, cont,
					     priority, depth);
  rar->trigger1.act = (struct ssm_act *) rar;
  rar->a = a;

  return rar;
}

void step_one(struct ssm_act *act)  
{
  rar_one_t *rar = (rar_one_t *) act;
  switch (rar->pc) {
  case 0:
    ssm_sensitize((struct ssm_sv *) rar->a, &rar->trigger1);
    rar->pc = 1;
    return;
  case 1:
    ssm_desensitize(&rar->trigger1);
    ssm_assign_i32(rar->a, rar->priority, rar->a->value + 4);
    ssm_leave((struct ssm_act *) rar, sizeof(rar_one_t));
    return;
  }
}



ssm_stepf_t step_two;

rar_two_t *ssm_enter_two(struct ssm_act *cont, ssm_priority_t priority,
			 ssm_depth_t depth, ssm_i32_t *a)
{
  rar_two_t *rar = (rar_two_t *) ssm_enter(sizeof(rar_two_t),
					   step_two, cont,
					   priority, depth);
  rar->trigger1.act = (struct ssm_act *) rar;
  rar->a = a;

  return rar;
}

void step_two(struct ssm_act *act)  
{
  rar_two_t *rar = (rar_two_t *) act;
  switch (rar->pc) {
  case 0:
    ssm_sensitize((struct ssm_sv *) rar->a, &rar->trigger1);
    rar->pc = 1;
    return;
  case 1:
    ssm_desensitize(&rar->trigger1);
    ssm_assign_i32(rar->a, rar->priority, rar->a->value * 2);
    ssm_leave((struct ssm_act *) rar, sizeof(rar_two_t));
    return;
  }
}


ssm_stepf_t step_main;

rar_main_t *ssm_enter_main(struct ssm_act *cont, ssm_priority_t priority,
		     ssm_depth_t depth)
{
  rar_main_t *rar = (rar_main_t *) ssm_enter(sizeof(rar_main_t), step_main, cont,
				       priority, depth);
  ssm_initialize_i32(&rar->a);
  rar->a.value = 0;

  return rar;
}

void step_main(struct ssm_act *act)  
{
  rar_main_t *rar = (rar_main_t *) act;
  switch (rar->pc) {    
  case 0:
    ssm_later_i32(&rar->a, ssm_now() + 1, 1);
    { ssm_depth_t new_depth = rar->depth - 1; // 2 children
      ssm_priority_t new_priority = rar->priority;
      ssm_priority_t pinc = 1 << new_depth;
      ssm_activate((struct ssm_act *) ssm_enter_one( (struct ssm_act *) rar, new_priority, new_depth,
				&rar->a));
      new_priority += pinc;
      ssm_activate((struct ssm_act *) ssm_enter_two( (struct ssm_act *) rar, new_priority, new_depth,
				&rar->a));
    }
    rar->pc = 1;
    return;
  case 1:
    printf("a = %d\n", rar->a.value);
    ssm_leave((struct ssm_act *) rar, sizeof(rar_main_t));
    return;
  }
}

void top_return(struct ssm_act *cont)
{
  return;
}

int main()
{  
  struct ssm_act top = { .step = top_return };
  ssm_activate((struct ssm_act *) ssm_enter_main(&top, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));

  do {
    ssm_tick();
    printf("ssm_now() %lu\n", ssm_now());
  } while (ssm_next_event_time() != SSM_NEVER);
  
  return 0;
}
