#include <stdio.h>
#include <stdlib.h>
#include "ssm.h"

/* From my "runtime" PLT lecture 

int fib(int n) {
    int tmp1, tmp2, tmp3;
    tmp1 = n < 2;
    if (!tmp1) goto L1;
    return 1;
L1: tmp1 = n - 1;
    tmp2 = fib(tmp1);
L2: tmp1 = n - 2;
    tmp3 = fib(tmp1);
L3: tmp1 = tmp2 + tmp3;
    return tmp1;
}

0 1 2 3 4 5  6  7  8  9 10  11  12  13
1 1 2 3 5 8 13 21 34 55 89 144 233 377

 */
typedef struct {
  ACTIVATION_RECORD_FIELDS;
  
  int *result;               // Where we should write our result
  int n, tmp1, tmp2, tmp3;   // Local variables
} fib_act_t;

stepf_t step_fib;

fib_act_t *enter_fib(rar_t *cont, priority_t priority,
		     pdepth_t priority_depth, int *result, int n)
{
  fib_act_t *act = (fib_act_t *) enter(sizeof(fib_act_t), step_fib, cont,
				       priority, priority_depth);
  act->n = n;
  act->result = result;
  return act;
}

void step_fib(rar_t *cont)  
{
  fib_act_t *act = (fib_act_t *) cont;
  //  printf("fib_step @%d n=%d\n", cont->pc, act->n);
  switch (act->pc) {    
  case 0:
    act->tmp1 = act->n < 2;    // tmp1 = n < 2
    if (!act->tmp1) goto L1;   // if (!tmp) goto L1
    *(act->result) = 1;        // return 1
    leave((rar_t *) act, sizeof(fib_act_t));
    return;
    
  L1:
    act->tmp1 = act->n - 1;                  // tmp1 = n - 1
    act->pc = 1;			     // tmp2 = fib(tmp1)
    invoke((rar_t *)
	   enter_fib((rar_t *) act, act->priority, act->priority_depth,
		     &act->tmp2, act->tmp1));
    return;

  case 1: // L2:
    act->tmp1 = act->n - 2;                  // tmp1 = n - 2
    act->pc = 2;  			     // tmp3 = fib(tmp1)
    invoke((rar_t *)
	   enter_fib((rar_t *) act, act->priority, act->priority_depth,
		     &act->tmp3, act->tmp1));
    return;

  case 2: // L3:
    act->tmp1 = act->tmp2 + act->tmp3;
    *(act->result) = act->tmp1;
    leave((rar_t *) act, sizeof(fib_act_t));
    return;
  }
}

void top_return(rar_t *cont)
{
  return;
}

int main(int argc, char *argv[])
{  
  int result;
  int n = argc > 1 ? atoi(argv[1]) : 3;

  rar_t top = { .step = top_return };
  invoke((rar_t *) enter_fib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT,
				      &result, n));

  printf("%d\n", result);

  return 0;
}
