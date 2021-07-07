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
  fork one(a) two(a)
  // a = 22 here


 */

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_int_t *a;
  trigger_t trigger1;
} rar_one_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_int_t *a;
  trigger_t trigger1;
} rar_two_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_int_t a;
} rar_main_t;

stepf_t step_one;

rar_one_t *enter_one(rar_t *cont, priority_t priority,
		     depth_t depth, cv_int_t *a)
{
  rar_one_t *rar = (rar_one_t *) enter(sizeof(rar_one_t),
					     step_one, cont,
					     priority, depth);
  rar->trigger1.rar = (rar_t *) rar;
  rar->a = a;

  return rar;
}

void step_one(rar_t *act)  
{
  rar_one_t *rar = (rar_one_t *) act;
  switch (rar->pc) {
  case 0:
    sensitize((cv_t *) rar->a, &rar->trigger1);
    rar->pc = 1;
    return;
  case 1:
    desensitize(&rar->trigger1);
    assign_int(rar->a, rar->priority, rar->a->value + 4);
    leave((rar_t *) rar, sizeof(rar_one_t));
    return;
  }
}



stepf_t step_two;

rar_two_t *enter_two(rar_t *cont, priority_t priority,
		     depth_t depth, cv_int_t *a)
{
  rar_two_t *rar = (rar_two_t *) enter(sizeof(rar_two_t),
					     step_two, cont,
					     priority, depth);
  rar->trigger1.rar = (rar_t *) rar;
  rar->a = a;

  return rar;
}

void step_two(rar_t *act)  
{
  rar_two_t *rar = (rar_two_t *) act;
  switch (rar->pc) {
  case 0:
    sensitize((cv_t *) rar->a, &rar->trigger1);
    rar->pc = 1;
    return;
  case 1:
    desensitize(&rar->trigger1);
    assign_int(rar->a, rar->priority, rar->a->value * 2);
    leave((rar_t *) rar, sizeof(rar_two_t));
    return;
  }
}


stepf_t step_main;

rar_main_t *enter_main(rar_t *cont, priority_t priority,
		     depth_t depth)
{
  rar_main_t *rar = (rar_main_t *) enter(sizeof(rar_main_t), step_main, cont,
				       priority, depth);
  initialize_int(&rar->a, 0);

  return rar;
}

void step_main(rar_t *act)  
{
  rar_main_t *rar = (rar_main_t *) act;
  switch (rar->pc) {    
  case 0:
    later_int(&rar->a, now + 1, 1);
    { depth_t new_depth = rar->depth - 1; // 2 children
      priority_t new_priority = rar->priority;
      priority_t pinc = 1 << new_depth;
      fork((rar_t *) enter_one( (rar_t *) rar, new_priority, new_depth,
				&rar->a));
      new_priority += pinc;
      fork((rar_t *) enter_two( (rar_t *) rar, new_priority, new_depth,
				&rar->a));
    }
    rar->pc = 1;
    return;
  case 1:
    printf("a = %d\n", rar->a.value);
    leave((rar_t *) rar, sizeof(rar_main_t));
    return;
  }
}

void top_return(rar_t *cont)
{
  return;
}

int main()
{  
  rar_t top = { .step = top_return };
  fork((rar_t *) enter_main(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT));

  tick();
  while (event_queue_len > 0) {
    now = event_queue[1]->event_time;
    tick();
  }
  
  return 0;
}
