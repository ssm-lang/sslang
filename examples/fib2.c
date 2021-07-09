#include <stdio.h>
#include <stdlib.h>
#include "ssm.h"

/* 

fib n &r
  var r2 = 0
  if n < 2 then r = 1 else
    fork fib(n-1, r) fib(n-2, r2)
    r = r + r2

0 1 2 3 4 5  6  7  8  9 10  11  12  13
1 1 2 3 5 8 13 21 34 55 89 144 233 377

 */
typedef struct {
  SSM_ACT_FIELDS;

  ssm_i32_t n;       // Local variable
  ssm_i32_t *r;      // Where we should write our result
  ssm_i32_t r2;
} rar_fib_t;

ssm_stepf_t step_fib;

rar_fib_t *ssm_enter_fib(struct ssm_act *cont, ssm_priority_t priority,
		     ssm_depth_t depth, int n, ssm_i32_t *r)
{
  rar_fib_t *rar = (rar_fib_t *) ssm_enter(sizeof(rar_fib_t), step_fib, cont,
				       priority, depth);
  ssm_initialize_i32(&rar->n);
  rar->n.value = n;
  rar->r = r;
  ssm_initialize_i32(&rar->r2);
  rar->r2.value = 0;

  return rar;
}

void step_fib(struct ssm_act *act)  
{
  rar_fib_t *rar = (rar_fib_t *) act;
  switch (rar->pc) {    
  case 0:
    if (rar->n.value < 2) {
      ssm_assign_i32(rar->r, rar->priority, 1);
      ssm_leave((struct ssm_act *) rar, sizeof(rar_fib_t));
      return;
    }
    { ssm_depth_t new_depth = rar->depth - 1; // 2 children
      ssm_priority_t new_priority = rar->priority;
      ssm_priority_t pinc = 1 << new_depth;
      ssm_activate((struct ssm_act *) ssm_enter_fib( (struct ssm_act *) rar, new_priority, new_depth,
				rar->n.value - 1, rar->r));
      new_priority += pinc;
      ssm_activate((struct ssm_act *) ssm_enter_fib( (struct ssm_act *) rar, new_priority, new_depth,
				rar->n.value - 2, &rar->r2));  
    }
    rar->pc = 1;
    return;
  case 1:
    ssm_assign_i32(rar->r, rar->priority, rar->r->value + rar->r2.value );
    ssm_leave((struct ssm_act *) rar, sizeof(rar_fib_t));
    return;
  }
}

void top_return(struct ssm_act *cont)
{
  return;
}

int main(int argc, char *argv[])
{  
  ssm_i32_t result;
  ssm_initialize_i32(&result);
  result.value = 0;
  int n = argc > 1 ? atoi(argv[1]) : 3;

  struct ssm_act top = { .step = top_return };
  ssm_activate((struct ssm_act *) ssm_enter_fib(&top, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH,
			   n, &result));

  ssm_tick();

  printf("%d\n", result.value);

  return 0;
}
