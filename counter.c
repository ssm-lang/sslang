#include <stdio.h>
#include <stdlib.h>
#include "ssm.h"

/*  Synchronous counter example, like scheduler-1/counter3.c)

fork
  loop
    clk = 0
    after 100ms clk = 1     await 100ms
    await @clk              clk = 1
    after 100ms clk = 0     await 100ms
    await @clk              clk = 0
  loop
    await
      clk == true: q1 = d1
  loop
    await
      clk == true: q2 = d2
  loop
    await
      @q2: d2 = q2 + 1
  loop
    await
      @q2 or @d2: d1 = q1 + d2



How do you check @q2?   Any assignment set the event_time?
Probably need another time stamp in the variable: indicates the instant in which
the variable was last updated

    await         
      x != lastx:     Syntactic sugar for this?
        lastx = x



    await
       expr:
         code
         code
       expr:
         code
         code

    await expr: code

    await expr

*/

typedef struct {
  ACTIVATION_RECORD_FIELDS;

  cv_int_t clk;
  cv_int_t d1;
  cv_int_t q1;
  cv_int_t d2;
  cv_int_t q2;
} act_main_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;

  act_main_t *static_link;
  trigger_t trigger1;
} act_clk_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;

  act_main_t *static_link;
  trigger_t trigger1;
} act_dff1_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;

  act_main_t *static_link;
  trigger_t trigger1;
} act_dff2_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;

  act_main_t *static_link;
  trigger_t trigger1;
} act_inc_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;

  act_main_t *static_link;
  trigger_t trigger1;
  trigger_t trigger2;
} act_adder_t;


stepf_t step_clk;

// Create a new activation for clk
act_clk_t *enter_clk(rar_t *parent, priority_t priority,
		     depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);
  
  act_clk_t *act = (act_clk_t *) enter(sizeof(act_clk_t), step_clk, parent,
				       priority, depth);
  
  act->static_link = static_link;
  
  act->trigger1.rar = (rar_t *) act;

  // Putting sensitize here is an optimization; more typical to put it at
  // each "await" site
  sensitize((cv_t *) &(act->static_link->clk), &(act->trigger1));

  return act;
}

void step_clk(rar_t *cont)
{
  act_clk_t *act = (act_clk_t *) cont;

  // printf("step_clk @%d\n", act->pc);
  printf("clk = %d\n", act->static_link->clk.value);

  switch (act->pc) {
  case 0:
    later_int(&act->static_link->clk, now + 100, 1);
    act->pc = 1;
    return;

  case 1:
    later_int(&act->static_link->clk, now + 100, 0);
    act->pc = 0;
    return;
  }
}


stepf_t step_dff1;

act_dff1_t *enter_dff1(rar_t *parent, priority_t priority,
		     depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);
  
  act_dff1_t *act = (act_dff1_t *) enter(sizeof(act_dff1_t), step_dff1, parent,
					 priority, depth);
  
  act->static_link = static_link;
  
  act->trigger1.rar = (rar_t *) act;

  // Putting sensitize here is an optimization; more typical to put it at
  // each "await" site
  sensitize((cv_t *) &(act->static_link->clk), &(act->trigger1));

  return act;
}

void step_dff1(rar_t *cont)
{
  act_dff1_t *act = (act_dff1_t *) cont;

  // printf("step_dff1 @%d\n", act->pc);
  printf("q1 = %d d1 = %d\n",
	 act->static_link->q1.value,
	 act->static_link->d1.value);

  switch (act->pc) {
  case 0:
    if (act->static_link->clk.value)
      assign_int(&act->static_link->q1, act->priority,
		 act->static_link->d1.value);
  }
}


stepf_t step_dff2;

act_dff2_t *enter_dff2(rar_t *parent, priority_t priority,
		       depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);
  
  act_dff2_t *act = (act_dff2_t *) enter(sizeof(act_dff2_t), step_dff2, parent,
					 priority, depth);
  
  act->static_link = static_link;
  
  act->trigger1.rar = (rar_t *) act;

  // Putting sensitize here is an optimization; more typical to put it at
  // each "await" site
  sensitize((cv_t *) &(act->static_link->clk), &(act->trigger1));

  return act;
}

void step_dff2(rar_t *cont)
{
  act_dff2_t *act = (act_dff2_t *) cont;

  // printf("step_dff2 @%d\n", act->pc);
  printf("q2 = %d d2 = %d\n",
	 act->static_link->q2.value,
	 act->static_link->d2.value);
  
  switch (act->pc) {
  case 0:
    if (act->static_link->clk.value)
      assign_int(&act->static_link->q2, act->priority,
		 act->static_link->d2.value);
  }
}


stepf_t step_inc;

act_inc_t *enter_inc(rar_t *parent, priority_t priority,
		     depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);

  act_inc_t *act = (act_inc_t *) enter(sizeof(act_inc_t), step_inc, parent,
				       priority, depth);
  act->static_link = static_link;
  act->trigger1.rar = (rar_t *) act;

  sensitize((cv_t *) &act->static_link->q2, &act->trigger1);
  
  return act;
}

void step_inc(rar_t *cont)
{
  act_inc_t *act = (act_inc_t *) cont;

  switch (act->pc) {
  case 0:
    assign_int(&act->static_link->d2, act->priority,
	       act->static_link->q2.value + 1);
  }

}

stepf_t step_adder;

act_adder_t *enter_adder(rar_t *parent, priority_t priority,
			 depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);

  act_adder_t *act = (act_adder_t *) enter(sizeof(act_adder_t), step_adder,
					   parent, priority, depth);
  act->static_link = static_link;
  
  act->trigger1.rar = act->trigger2.rar = (rar_t *) act;
  sensitize((cv_t *) &act->static_link->q2, &act->trigger1);
  sensitize((cv_t *) &act->static_link->d2, &act->trigger2);

  return act;
}

void step_adder(rar_t *cont)
{
  act_adder_t *act = (act_adder_t *) cont;

  switch (act->pc) {
  case 0:
    assign_int(&act->static_link->d1, act->priority,
	       act->static_link->q1.value + act->static_link->d2.value);
  }
}



stepf_t step_main;

// Create a new activation record for main
act_main_t *enter_main(rar_t *parent, priority_t priority,
		       depth_t depth)
{
  act_main_t *act = (act_main_t *) enter(sizeof(act_main_t), step_main, parent,
					 priority, depth);
  
  // Initialize managed variables
  initialize_int(&(act->clk), 0);
  initialize_int(&(act->d1), 0);
  initialize_int(&(act->q1), 0);
  initialize_int(&(act->d2), 0);
  initialize_int(&(act->q2), 0);
  
  return act;
}

void step_main(rar_t *cont)
{
  act_main_t *act = (act_main_t *) cont;
  depth_t new_depth;
  priority_t pinc;

  // printf("step_main @%d\n", act->pc);
  
  switch (act->pc) {
  case 0:
    new_depth = act->depth - 3; // Make space for 8 children
    pinc = 1 << new_depth;    // priority increment for each thread
    
    fork((rar_t *) enter_clk((rar_t *) act,
				 act->priority + 0 * pinc, new_depth, act));
    fork((rar_t *) enter_dff1((rar_t *) act,
				  act->priority + 1 * pinc, new_depth, act));
    fork((rar_t *) enter_dff2((rar_t *) act,
				  act->priority + 2 * pinc, new_depth, act));
    fork((rar_t *) enter_inc((rar_t *) act,
				 act->priority + 3 * pinc, new_depth, act));
    fork((rar_t *) enter_adder((rar_t *) act,
				   act->priority + 4 * pinc, new_depth, act));
    act->pc = 1;
    return;
  case 1:
    leave((rar_t *) act, sizeof(act_main_t));
    return;
  }
}

void main_return(rar_t *cont)
{
  return;
}

int main(int argc, char *argv[])
{
  ssm_time_t stop_at = argc > 1 ? atoi(argv[1]) : 1000;
  
  rar_t top = { .step = main_return };  
  act_main_t *act = enter_main(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT);  
  fork((rar_t *) act);

  tick();

  while (event_queue_len > 0 && now < stop_at) {
    now = event_queue[1]->event_time;
    printf("now %lu\n", now);
    tick();
  }
  
  return 0;
}
