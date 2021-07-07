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
  ACTIVATION_RECORD_FIELDS;
  cv_bool_t clk;
  cv_int_t q1, d1, q2, d2;  
} act_main_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_bool_t *clk;
  cv_event_t timer;
  trigger_t trigger1;
} act_clock_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_bool_t *clk;
  cv_int_t *d, *q;
  trigger_t trigger1;
} act_dff_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_int_t *a, *y;
  trigger_t trigger1;
} act_inc_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_int_t *a, *b, *y;
  trigger_t trigger1, trigger2;
} act_adder_t;


stepf_t step_clock;

act_clock_t *enter_clock(rar_t *parent, priority_t priority,
			 depth_t depth, cv_bool_t *clk)
{
  assert(parent);
  assert(clk);

  act_clock_t *act = (act_clock_t *) enter(sizeof(act_clock_t), step_clock,
					   parent, priority, depth);
  act->clk = clk;
  initialize_event( &(act->timer) );
  act->trigger1.rar = (rar_t *) act;

  return act;
}

void step_clock(rar_t *cont)
{
  act_clock_t *act = (act_clock_t *) cont;

  printf("clk = %d\n", act->clk->value);

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      later_event(&act->timer, now + 100); // after 100 timer = Event

      sensitize((cv_t *) &act->timer, &act->trigger1); // await @timer
      act->pc = 1;
      return;
    case 1:
      if (event_on((cv_t *) &act->timer)) { // @timer
	desensitize(&act->trigger1);
      }	else return;

      assign_bool(act->clk, act->priority, true); // clk = True
      
      later_event(&act->timer, now + 100); // after 100 timer = Event
      
      sensitize((cv_t *) &act->timer, &act->trigger1); // await @timer
      act->pc = 2;
      return;
    case 2:
      if (event_on((cv_t *) &act->timer)) { // @timer
	desensitize(&act->trigger1);
      } else return;

      assign_bool(act->clk, act->priority, false); // clk = False
    }
  }
  leave((rar_t *) act, sizeof(act_clock_t));
}


stepf_t step_dff;

act_dff_t *enter_dff(rar_t *parent, priority_t priority,
		     depth_t depth,
		     cv_bool_t *clk, cv_int_t *d, cv_int_t *q)
{
  assert(parent);
  assert(clk);
  assert(d);
  assert(q);
  
  act_dff_t *act = (act_dff_t *) enter(sizeof(act_dff_t), step_dff, parent,
				       priority, depth);

  act->clk = clk;
  act->d = d;
  act->q = q;
  act->trigger1.rar = (rar_t *) act;

  return act;
}

void step_dff(rar_t *cont)
{
  act_dff_t *act = (act_dff_t *) cont;

  printf("q = %d d = %d\n",
	 act->q->value,
	 act->d->value);

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      // await clk == True
      sensitize((cv_t *) act->clk, &act->trigger1);
      act->pc = 1;
      return;
    case 1:
      if (event_on((cv_t *)(act->clk)) && act->clk->value ) {  // clk == true
	desensitize(&act->trigger1);
      } else return;
      
      assign_int(act->q, act->priority, act->d->value); // q = d
    }

  }
  leave((rar_t *) act, sizeof(act_dff_t)); 
}


stepf_t step_inc;

act_inc_t *enter_inc(rar_t *parent, priority_t priority,
		     depth_t depth,
		     cv_int_t *a, cv_int_t *y)
{
  assert(parent);
  assert(a);
  assert(y);
  
  act_inc_t *act = (act_inc_t *) enter(sizeof(act_inc_t), step_inc, parent,
				       priority, depth);

  act->a = a;
  act->y = y;
  act->trigger1.rar = (rar_t *) act;

  return act;
}

void step_inc(rar_t *cont)
{
  act_inc_t *act = (act_inc_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      // await @a
      sensitize((cv_t *) act->a, &act->trigger1);
      act->pc = 1;
      return;
    case 1:
      if (event_on((cv_t *)(act->a))) {  // @a
	desensitize(&act->trigger1);
	assign_int(act->y, act->priority, act->a->value + 1);
      } else return;     
    }

  }
  leave((rar_t *) act, sizeof(act_inc_t)); 
}


stepf_t step_adder;

act_adder_t *enter_adder(rar_t *parent, priority_t priority,
			 depth_t depth,
			 cv_int_t *a, cv_int_t *b, cv_int_t *y)
{
  assert(parent);
  assert(a);
  assert(b);
  assert(y);
  
  act_adder_t *act = (act_adder_t *) enter(sizeof(act_adder_t),
					   step_adder, parent,
					   priority, depth);

  act->a = a;
  act->b = b;
  act->y = y;
  act->trigger1.rar = act->trigger2.rar = (rar_t *) act;

  return act;
}

void step_adder(rar_t *cont)
{
  act_adder_t *act = (act_adder_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      // await @a or @b
      sensitize((cv_t *) act->a, &act->trigger1);
      sensitize((cv_t *) act->b, &act->trigger2);
      act->pc = 1;
      return;
    case 1:
      if (event_on((cv_t *)(act->a)) ||
	  event_on((cv_t *)(act->b))) {  // @a or @b
	desensitize(&act->trigger1);
	desensitize(&act->trigger2);
	assign_int(act->y, act->priority, act->a->value + act->b->value);
      } else return;     
    }
  }
  leave((rar_t *) act, sizeof(act_adder_t)); 
}



stepf_t step_main;

// Create a new activation record for main
act_main_t *enter_main(rar_t *parent, priority_t priority,
		       depth_t depth)
{
  act_main_t *act = (act_main_t *) enter(sizeof(act_main_t), step_main, parent,
					 priority, depth);
  
  // Initialize managed variables
  initialize_bool(&act->clk, false);
  initialize_int(&act->d1, 0);
  initialize_int(&act->q1, 0);
  initialize_int(&act->d2, 0);
  initialize_int(&act->q2, 0);
  
  return act;
}

void step_main(rar_t *cont)
{
  act_main_t *act = (act_main_t *) cont;

  switch (act->pc) {
  case 0:
    {                                               // fork
      depth_t new_depth = act->depth - 3; // 8 children
      priority_t new_priority = act->priority;
      priority_t pinc = 1 << new_depth;             // increment for each thread
      fork((rar_t *)              // clock clk
		    enter_clock((rar_t *) act, new_priority, new_depth,
				&act->clk ));
      new_priority += pinc;
      fork((rar_t *)              // dff clk d1 q1
		    enter_dff((rar_t *) act, new_priority, new_depth,
			      &act->clk, &act->d1, &act->q1 ));
      new_priority += pinc;
      fork((rar_t *)              // dff clk d2 q2
		    enter_dff((rar_t *) act, new_priority, new_depth,
			      &act->clk, &act->d2, &act->q2 ));
      new_priority += pinc;
      fork((rar_t *)              // inc q2 d2
		    enter_inc((rar_t *) act, new_priority, new_depth,
			      &act->q2, &act->d2 ));
      new_priority += pinc;
      fork((rar_t *)              // adder q1 d2 d1
		    enter_adder((rar_t *) act, new_priority, new_depth,
				&act->q1, &act->d2, &act->d1 ));
      // new_priority += pinc;
    }
    act->pc = 1;
    return;
  case 1:
    leave((rar_t *) act, sizeof(act_adder_t));
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
