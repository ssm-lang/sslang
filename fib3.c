#include <stdio.h>
#include <stdlib.h>
#include "ssm.h"

/* 
mywait &r
  wait r

sum &r1 &r2 &r
  fork mywait(r1) mywait(r2)
  after 1s r = r1 + r2

fib n &r
  var r1 = 0  
  var r2 = 0
  if n < 2 then after 1s r = 1 else
    fork sum(r1, r2, r)  fib(n-1, r1)  fib(n-2, r2)

0 1 2 3 4 5  6  7  8  9 10  11  12  13
1 1 2 3 5 8 13 21 34 55 89 144 233 377

 */

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_int_t *r;
  trigger_t trigger1;
} rar_mywait_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_int_t *r1, *r2, *r;
} rar_sum_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;

  cv_int_t n;       // Local variable
  cv_int_t *r;      // Where we should write our result
  cv_int_t r1, r2;
} rar_fib_t;

stepf_t step_mywait;

rar_mywait_t *enter_mywait(rar_t *cont, priority_t priority,
		     depth_t depth, cv_int_t *r)
{
  rar_mywait_t *rar = (rar_mywait_t *) enter(sizeof(rar_mywait_t),
					     step_mywait, cont,
					     priority, depth);
  rar->trigger1.rar = (rar_t *) rar;
  rar->r = r;

  return rar;
}

void step_mywait(rar_t *act)  
{
  rar_mywait_t *rar = (rar_mywait_t *) act;
  switch (rar->pc) {
  case 0:
    sensitize((cv_t *) rar->r, &rar->trigger1);
    rar->pc = 1;
    return;
  case 1:
    desensitize(&rar->trigger1);
    leave((rar_t *) rar, sizeof(rar_mywait_t));
    return;
  }
}

stepf_t step_sum;

rar_sum_t *enter_sum(rar_t *cont, priority_t priority,
		     depth_t depth, cv_int_t *r1, cv_int_t *r2, cv_int_t *r)
{
  rar_sum_t *rar = (rar_sum_t *) enter(sizeof(rar_sum_t), step_sum, cont,
				       priority, depth);
  rar->r1 = r1;
  rar->r2 = r2;
  rar->r = r;

  return rar;
}

void step_sum(rar_t *act)  
{
  rar_sum_t *rar = (rar_sum_t *) act;
  switch (rar->pc) {
  case 0:
    { depth_t new_depth = rar->depth - 1; // 2 children
      priority_t new_priority = rar->priority;
      priority_t pinc = 1 << new_depth;
      fork((rar_t *) enter_mywait( (rar_t *) rar, new_priority, new_depth,
				   rar->r1));
      new_priority += pinc;
      fork((rar_t *) enter_mywait( (rar_t *) rar, new_priority, new_depth,
				   rar->r2));
    }
    rar->pc = 1;
    return;
  case 1:
    later_int(rar->r, now + 1,  rar->r1->value + rar->r2->value);
    leave((rar_t *) rar, sizeof(rar_sum_t));
    return;
  }
}


stepf_t step_fib;

rar_fib_t *enter_fib(rar_t *cont, priority_t priority,
		     depth_t depth, int n, cv_int_t *r)
{
  rar_fib_t *rar = (rar_fib_t *) enter(sizeof(rar_fib_t), step_fib, cont,
				       priority, depth);
  initialize_int(&rar->n, n);
  rar->r = r;
  initialize_int(&rar->r1, 0);
  initialize_int(&rar->r2, 0);

  return rar;
}

void step_fib(rar_t *act)  
{
  rar_fib_t *rar = (rar_fib_t *) act;
  switch (rar->pc) {    
  case 0:
    if (rar->n.value < 2) {
      later_int(rar->r, now + 1, 1);
      leave((rar_t *) rar, sizeof(rar_fib_t));
      return;
    }
    { depth_t new_depth = rar->depth - 2; // 4 children
      priority_t new_priority = rar->priority;
      priority_t pinc = 1 << new_depth;
      fork((rar_t *) enter_fib( (rar_t *) rar, new_priority, new_depth,
				rar->n.value - 1, &rar->r1));
      new_priority += pinc;
      fork((rar_t *) enter_fib( (rar_t *) rar, new_priority, new_depth,
				rar->n.value - 2, &rar->r2));  
      new_priority += pinc;
      fork((rar_t *) enter_sum( (rar_t *) rar, new_priority, new_depth,
				&rar->r1, &rar->r2, rar->r));

    }
    rar->pc = 1;
    return;
  case 1:
    leave((rar_t *) rar, sizeof(rar_fib_t));
    return;
  }
}

void top_return(rar_t *cont)
{
  return;
}

int main(int argc, char *argv[])
{  
  cv_int_t result;
  initialize_int(&result, 0);
  int n = argc > 1 ? atoi(argv[1]) : 3;

  rar_t top = { .step = top_return };
  fork((rar_t *) enter_fib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT,
			   n, &result));

  tick();
  
  while (event_queue_len > 0) {
    now = event_queue[1]->event_time;
    printf("now %lu (%d events pending)\n", now, event_queue_len);
    tick();
  }

  printf("%d\n", result.value);

  return 0;
}
