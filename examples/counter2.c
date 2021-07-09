#include <stdio.h>
#include <stdlib.h>
#include "ssm.h"

/* Synchronous counter example using pass-by-reference variables

def clock clk : int ref =
  var timer : event
  loop
    after 100 timer = Event       await 100ms
    await @timer
    clk = True
    after 100 timer = Event
    await @timer
    clk = False

def dff clk d q : int ref =
  loop
    await
       clk: q = d

def inc a y : int ref =
  loop
    await
      @a: y = a + 1

def adder (a b y : int ref) : void =
   loop
     await
       @a or @b: y = a + b

def main : void =         // priority 0000....
  var clk : bool
  var q1 d1 q2 d2 : int

  clock(clk)              // priority 0000....
  || dff(clk, d1, q1)     //          0010....
                          //          0010....  first child
                          //          0011....  second child
  || dff(clk, d2, q2 + 1) //          0100....
  || inc(q2, d2)          //          0110....
  || adder(q1, d2, d1)    //          1000....


  // What about Go syntax?  Inputs vs. outputs
    
 */

typedef struct {
  SSM_ACT_FIELDS;
  ssm_bool_t clk;
  ssm_i32_t q1, d1, q2, d2;  
} act_main_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_bool_t *clk;
  ssm_event_t timer;
  struct ssm_trigger trigger1;
} act_clock_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_bool_t *clk;
  ssm_i32_t *d, *q;
  struct ssm_trigger trigger1;
} act_dff_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_i32_t *a, *y;
  struct ssm_trigger trigger1;
} act_inc_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_i32_t *a, *b, *y;
  struct ssm_trigger trigger1, trigger2;
} act_adder_t;


ssm_stepf_t step_clock;

act_clock_t *ssm_enter_clock(struct ssm_act *parent, ssm_priority_t priority,
			 ssm_depth_t depth, ssm_bool_t *clk)
{
  assert(parent);
  assert(clk);

  act_clock_t *act = (act_clock_t *) ssm_enter(sizeof(act_clock_t), step_clock,
					   parent, priority, depth);
  act->clk = clk;
  ssm_initialize_event( &(act->timer) );
  act->trigger1.act = (struct ssm_act *) act;

  return act;
}

void step_clock(struct ssm_act *cont)
{
  act_clock_t *act = (act_clock_t *) cont;

  printf("clk = %d\n", act->clk->value);

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      ssm_later_event(&act->timer, ssm_now() + 100); // after 100 timer = Event

      ssm_sensitize((struct ssm_sv *) &act->timer, &act->trigger1); // await @timer
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((struct ssm_sv *) &act->timer)) { // @timer
	ssm_desensitize(&act->trigger1);
      }	else return;

      ssm_assign_bool(act->clk, act->priority, true); // clk = True
      
      ssm_later_event(&act->timer, ssm_now() + 100); // after 100 timer = Event
      
      ssm_sensitize((struct ssm_sv *) &act->timer, &act->trigger1); // await @timer
      act->pc = 2;
      return;
    case 2:
      if (ssm_event_on((struct ssm_sv *) &act->timer)) { // @timer
	ssm_desensitize(&act->trigger1);
      } else return;

      ssm_assign_bool(act->clk, act->priority, false); // clk = False
    }
  }
  ssm_leave((struct ssm_act *) act, sizeof(act_clock_t));
}


ssm_stepf_t step_dff;

act_dff_t *ssm_enter_dff(struct ssm_act *parent, ssm_priority_t priority,
		     ssm_depth_t depth,
		     ssm_bool_t *clk, ssm_i32_t *d, ssm_i32_t *q)
{
  assert(parent);
  assert(clk);
  assert(d);
  assert(q);
  
  act_dff_t *act = (act_dff_t *) ssm_enter(sizeof(act_dff_t), step_dff, parent,
				       priority, depth);

  act->clk = clk;
  act->d = d;
  act->q = q;
  act->trigger1.act = (struct ssm_act *) act;

  return act;
}

void step_dff(struct ssm_act *cont)
{
  act_dff_t *act = (act_dff_t *) cont;

  printf("q = %d d = %d\n",
	 act->q->value,
	 act->d->value);

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      // await clk == True
      ssm_sensitize((struct ssm_sv *) act->clk, &act->trigger1);
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((struct ssm_sv *)(act->clk)) && act->clk->value ) {  // clk == true
	ssm_desensitize(&act->trigger1);
      } else return;
      
      ssm_assign_i32(act->q, act->priority, act->d->value); // q = d
    }

  }
  ssm_leave((struct ssm_act *) act, sizeof(act_dff_t)); 
}


ssm_stepf_t step_inc;

act_inc_t *ssm_enter_inc(struct ssm_act *parent, ssm_priority_t priority,
		     ssm_depth_t depth,
		     ssm_i32_t *a, ssm_i32_t *y)
{
  assert(parent);
  assert(a);
  assert(y);
  
  act_inc_t *act = (act_inc_t *) ssm_enter(sizeof(act_inc_t), step_inc, parent,
				       priority, depth);

  act->a = a;
  act->y = y;
  act->trigger1.act = (struct ssm_act *) act;

  return act;
}

void step_inc(struct ssm_act *cont)
{
  act_inc_t *act = (act_inc_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      // await @a
      ssm_sensitize((struct ssm_sv *) act->a, &act->trigger1);
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((struct ssm_sv *)(act->a))) {  // @a
	ssm_desensitize(&act->trigger1);
	ssm_assign_i32(act->y, act->priority, act->a->value + 1);
      } else return;     
    }

  }
  ssm_leave((struct ssm_act *) act, sizeof(act_inc_t)); 
}


ssm_stepf_t step_adder;

act_adder_t *ssm_enter_adder(struct ssm_act *parent, ssm_priority_t priority,
			 ssm_depth_t depth,
			 ssm_i32_t *a, ssm_i32_t *b, ssm_i32_t *y)
{
  assert(parent);
  assert(a);
  assert(b);
  assert(y);
  
  act_adder_t *act = (act_adder_t *) ssm_enter(sizeof(act_adder_t),
					   step_adder, parent,
					   priority, depth);

  act->a = a;
  act->b = b;
  act->y = y;
  act->trigger1.act = act->trigger2.act = (struct ssm_act *) act;

  return act;
}

void step_adder(struct ssm_act *cont)
{
  act_adder_t *act = (act_adder_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      // await @a or @b
      ssm_sensitize((struct ssm_sv *) act->a, &act->trigger1);
      ssm_sensitize((struct ssm_sv *) act->b, &act->trigger2);
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((struct ssm_sv *)(act->a)) ||
	  ssm_event_on((struct ssm_sv *)(act->b))) {  // @a or @b
	ssm_desensitize(&act->trigger1);
	ssm_desensitize(&act->trigger2);
	ssm_assign_i32(act->y, act->priority, act->a->value + act->b->value);
      } else return;     
    }
  }
  ssm_leave((struct ssm_act *) act, sizeof(act_adder_t)); 
}



ssm_stepf_t step_main;

// Create a new activation record for main
act_main_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
		       ssm_depth_t depth)
{
  act_main_t *act = (act_main_t *) ssm_enter(sizeof(act_main_t), step_main, parent,
					 priority, depth);
  
  // Initialize managed variables
  ssm_initialize_bool(&act->clk);
  act->clk.value = false;
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

  switch (act->pc) {
  case 0:
    {                                               // ssm_activate
      ssm_depth_t new_depth = act->depth - 3; // 8 children
      ssm_priority_t new_priority = act->priority;
      ssm_priority_t pinc = 1 << new_depth;             // increment for each thread
      ssm_activate((struct ssm_act *)              // clock clk
		    ssm_enter_clock((struct ssm_act *) act, new_priority, new_depth,
				&act->clk ));
      new_priority += pinc;
      ssm_activate((struct ssm_act *)              // dff clk d1 q1
		    ssm_enter_dff((struct ssm_act *) act, new_priority, new_depth,
			      &act->clk, &act->d1, &act->q1 ));
      new_priority += pinc;
      ssm_activate((struct ssm_act *)              // dff clk d2 q2
		    ssm_enter_dff((struct ssm_act *) act, new_priority, new_depth,
			      &act->clk, &act->d2, &act->q2 ));
      new_priority += pinc;
      ssm_activate((struct ssm_act *)              // inc q2 d2
		    ssm_enter_inc((struct ssm_act *) act, new_priority, new_depth,
			      &act->q2, &act->d2 ));
      new_priority += pinc;
      ssm_activate((struct ssm_act *)              // adder q1 d2 d1
		    ssm_enter_adder((struct ssm_act *) act, new_priority, new_depth,
				&act->q1, &act->d2, &act->d1 ));
      // new_priority += pinc;
    }
    act->pc = 1;
    return;
  case 1:
    ssm_leave((struct ssm_act *) act, sizeof(act_adder_t));
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

  ssm_tick();

  while (ssm_next_event_time() != SSM_NEVER && ssm_now() < stop_at) {
    ssm_tick();
    printf("ssm_now() %lu\n", ssm_now());
  }
  
  return 0;
}
