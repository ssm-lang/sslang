#include <stdio.h>
#include <stdlib.h>
#include "ssm.h"

/*  Synchronous counter example, like scheduler-1/counter3.c)

ssm_activate
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
  SSM_ACT_FIELDS;

  ssm_i32_t clk;
  ssm_i32_t d1;
  ssm_i32_t q1;
  ssm_i32_t d2;
  ssm_i32_t q2;
} act_main_t;

typedef struct {
  SSM_ACT_FIELDS;

  act_main_t *static_link;
  struct ssm_trigger trigger1;
} act_clk_t;

typedef struct {
  SSM_ACT_FIELDS;

  act_main_t *static_link;
  struct ssm_trigger trigger1;
} act_dff1_t;

typedef struct {
  SSM_ACT_FIELDS;

  act_main_t *static_link;
  struct ssm_trigger trigger1;
} act_dff2_t;

typedef struct {
  SSM_ACT_FIELDS;

  act_main_t *static_link;
  struct ssm_trigger trigger1;
} act_inc_t;

typedef struct {
  SSM_ACT_FIELDS;

  act_main_t *static_link;
  struct ssm_trigger trigger1;
  struct ssm_trigger trigger2;
} act_adder_t;


ssm_stepf_t step_clk;

// Create a new activation for clk
act_clk_t *ssm_enter_clk(struct ssm_act *parent, ssm_priority_t priority,
		     ssm_depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);
  
  act_clk_t *act = (act_clk_t *) ssm_enter(sizeof(act_clk_t), step_clk, parent,
				       priority, depth);
  
  act->static_link = static_link;
  
  act->trigger1.act = (struct ssm_act *) act;

  // Putting ssm_sensitize here is an optimization; more typical to put it at
  // each "await" site
  ssm_sensitize((struct ssm_sv *) &(act->static_link->clk), &(act->trigger1));

  return act;
}

void step_clk(struct ssm_act *cont)
{
  act_clk_t *act = (act_clk_t *) cont;

  // printf("step_clk @%d\n", act->pc);
  printf("clk = %d\n", act->static_link->clk.value);

  switch (act->pc) {
  case 0:
    ssm_later_i32(&act->static_link->clk, ssm_now() + 100, 1);
    act->pc = 1;
    return;

  case 1:
    ssm_later_i32(&act->static_link->clk, ssm_now() + 100, 0);
    act->pc = 0;
    return;
  }
}


ssm_stepf_t step_dff1;

act_dff1_t *ssm_enter_dff1(struct ssm_act *parent, ssm_priority_t priority,
		     ssm_depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);
  
  act_dff1_t *act = (act_dff1_t *) ssm_enter(sizeof(act_dff1_t), step_dff1, parent,
					 priority, depth);
  
  act->static_link = static_link;
  
  act->trigger1.act = (struct ssm_act *) act;

  // Putting ssm_sensitize here is an optimization; more typical to put it at
  // each "await" site
  ssm_sensitize((struct ssm_sv *) &(act->static_link->clk), &(act->trigger1));

  return act;
}

void step_dff1(struct ssm_act *cont)
{
  act_dff1_t *act = (act_dff1_t *) cont;

  // printf("step_dff1 @%d\n", act->pc);
  printf("q1 = %d d1 = %d\n",
	 act->static_link->q1.value,
	 act->static_link->d1.value);

  switch (act->pc) {
  case 0:
    if (act->static_link->clk.value)
      ssm_assign_i32(&act->static_link->q1, act->priority,
		 act->static_link->d1.value);
  }
}


ssm_stepf_t step_dff2;

act_dff2_t *ssm_enter_dff2(struct ssm_act *parent, ssm_priority_t priority,
		       ssm_depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);
  
  act_dff2_t *act = (act_dff2_t *) ssm_enter(sizeof(act_dff2_t), step_dff2, parent,
					 priority, depth);
  
  act->static_link = static_link;
  
  act->trigger1.act = (struct ssm_act *) act;

  // Putting ssm_sensitize here is an optimization; more typical to put it at
  // each "await" site
  ssm_sensitize((struct ssm_sv *) &(act->static_link->clk), &(act->trigger1));

  return act;
}

void step_dff2(struct ssm_act *cont)
{
  act_dff2_t *act = (act_dff2_t *) cont;

  // printf("step_dff2 @%d\n", act->pc);
  printf("q2 = %d d2 = %d\n",
	 act->static_link->q2.value,
	 act->static_link->d2.value);
  
  switch (act->pc) {
  case 0:
    if (act->static_link->clk.value)
      ssm_assign_i32(&act->static_link->q2, act->priority,
		 act->static_link->d2.value);
  }
}


ssm_stepf_t step_inc;

act_inc_t *ssm_enter_inc(struct ssm_act *parent, ssm_priority_t priority,
		     ssm_depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);

  act_inc_t *act = (act_inc_t *) ssm_enter(sizeof(act_inc_t), step_inc, parent,
				       priority, depth);
  act->static_link = static_link;
  act->trigger1.act = (struct ssm_act *) act;

  ssm_sensitize((struct ssm_sv *) &act->static_link->q2, &act->trigger1);
  
  return act;
}

void step_inc(struct ssm_act *cont)
{
  act_inc_t *act = (act_inc_t *) cont;

  switch (act->pc) {
  case 0:
    ssm_assign_i32(&act->static_link->d2, act->priority,
	       act->static_link->q2.value + 1);
  }

}

ssm_stepf_t step_adder;

act_adder_t *ssm_enter_adder(struct ssm_act *parent, ssm_priority_t priority,
			 ssm_depth_t depth, act_main_t *static_link)
{
  assert(parent);
  assert(static_link);

  act_adder_t *act = (act_adder_t *) ssm_enter(sizeof(act_adder_t), step_adder,
					   parent, priority, depth);
  act->static_link = static_link;
  
  act->trigger1.act = act->trigger2.act = (struct ssm_act *) act;
  ssm_sensitize((struct ssm_sv *) &act->static_link->q2, &act->trigger1);
  ssm_sensitize((struct ssm_sv *) &act->static_link->d2, &act->trigger2);

  return act;
}

void step_adder(struct ssm_act *cont)
{
  act_adder_t *act = (act_adder_t *) cont;

  switch (act->pc) {
  case 0:
    ssm_assign_i32(&act->static_link->d1, act->priority,
	       act->static_link->q1.value + act->static_link->d2.value);
  }
}



ssm_stepf_t step_main;

// Create a new activation record for main
act_main_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
		       ssm_depth_t depth)
{
  act_main_t *act = (act_main_t *) ssm_enter(sizeof(act_main_t), step_main, parent,
					 priority, depth);
  
  // Initialize managed variables
  ssm_initialize_i32(&(act->clk));
  act->clk.value = 0;
  ssm_initialize_i32(&(act->d1));
  act->d1.value = 0;
  ssm_initialize_i32(&(act->q1));
  act->q1.value = 0;
  ssm_initialize_i32(&(act->d2));
  act->d2.value = 0;
  ssm_initialize_i32(&(act->q2));
  act->q2.value = 0;
  
  return act;
}

void step_main(struct ssm_act *cont)
{
  act_main_t *act = (act_main_t *) cont;
  ssm_depth_t new_depth;
  ssm_priority_t pinc;

  // printf("step_main @%d\n", act->pc);
  
  switch (act->pc) {
  case 0:
    new_depth = act->depth - 3; // Make space for 8 children
    pinc = 1 << new_depth;    // priority increment for each thread
    
    ssm_activate((struct ssm_act *) ssm_enter_clk((struct ssm_act *) act,
				 act->priority + 0 * pinc, new_depth, act));
    ssm_activate((struct ssm_act *) ssm_enter_dff1((struct ssm_act *) act,
				  act->priority + 1 * pinc, new_depth, act));
    ssm_activate((struct ssm_act *) ssm_enter_dff2((struct ssm_act *) act,
				  act->priority + 2 * pinc, new_depth, act));
    ssm_activate((struct ssm_act *) ssm_enter_inc((struct ssm_act *) act,
				 act->priority + 3 * pinc, new_depth, act));
    ssm_activate((struct ssm_act *) ssm_enter_adder((struct ssm_act *) act,
				   act->priority + 4 * pinc, new_depth, act));
    act->pc = 1;
    return;
  case 1:
    ssm_leave((struct ssm_act *) act, sizeof(act_main_t));
    return;
  }
}

void main_return(struct ssm_act *cont)
{
  return;
}

int main(int argc, char *argv[])
{
  ssm_time_t stop_at = argc > 1 ? atoi(argv[1]) : 1000;
  
  struct ssm_act top = { .step = main_return };  
  act_main_t *act = ssm_enter_main(&top, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH);  
  ssm_activate((struct ssm_act *) act);

  do {
    ssm_tick();
    printf("finished time %lu\n", ssm_now());
  } while (ssm_next_event_time() != SSM_NEVER && ssm_now() < stop_at);
  
  return 0;
}
