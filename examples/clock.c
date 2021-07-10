#include <stdio.h>
#include "ssm.h"

/* Simple time keeper

def second_clock second_event : event ref =
   var timer : event
   loop
     seconds = Event
     after 1s timer = Event
     await @timer

def report_seconds second_event : event ref =
   var seconds : int = 0
   loop
     await @second_event
     seconds = seconds + 1
     print(seconds)

def main : void =
  var second : event
    second_clock(second) || report_seconds(second)

 */

typedef struct {
  SSM_ACT_FIELDS;
  ssm_event_t second;
} act_main_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_event_t *second_event;
  ssm_event_t timer;
  struct ssm_trigger trigger1;
} act_second_clock_t;

typedef struct {
  SSM_ACT_FIELDS;
  ssm_event_t *second_event;
  ssm_i32_t seconds;
  struct ssm_trigger trigger1;
} act_report_seconds_t;

ssm_stepf_t step_second_clock;

act_second_clock_t *
ssm_enter_second_clock(struct ssm_act *parent, ssm_priority_t priority,
		   ssm_depth_t depth, ssm_event_t *second_event)
{
  assert(parent);
  assert(second_event);

  act_second_clock_t *act = (act_second_clock_t *)
    ssm_enter(sizeof(act_second_clock_t), step_second_clock,
	  parent, priority, depth);
  act->second_event = second_event;
  ssm_initialize_event( &(act->timer) );
  act->trigger1.act = (struct ssm_act *) act;

  return act;
}

void step_second_clock(struct ssm_act *cont)
{
  act_second_clock_t *act = (act_second_clock_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      ssm_assign_event(act->second_event, act->priority); // seconds = Event

      // after 1s timer = Event
      ssm_later_event(&act->timer, ssm_now() + SSM_SECOND);

      ssm_sensitize((struct ssm_sv *) &act->timer, &act->trigger1); // await @timer
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((struct ssm_sv *) &act->timer)) { // @timer
	ssm_desensitize(&act->trigger1);
      }	else return;
    } // end of loop
  }
  ssm_leave((struct ssm_act *) act, sizeof(act_second_clock_t));
}

ssm_stepf_t step_report_seconds;

act_report_seconds_t *
ssm_enter_report_seconds(struct ssm_act *parent, ssm_priority_t priority,
		   ssm_depth_t depth, ssm_event_t *second_event)
{
  assert(parent);
  assert(second_event);

  act_report_seconds_t *act = (act_report_seconds_t *)
    ssm_enter(sizeof(act_report_seconds_t), step_report_seconds,
	  parent, priority, depth);
  act->second_event = second_event;
  ssm_initialize_i32(&act->seconds);
  act->seconds.value = 0;
  act->trigger1.act = (struct ssm_act *) act;
  
  return act;
}

void step_report_seconds(struct ssm_act *cont)
{
  act_report_seconds_t *act = (act_report_seconds_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      ssm_sensitize((struct ssm_sv *) act->second_event, &act->trigger1); // await @timer
      act->pc = 1;
      return;
    case 1:
      if (ssm_event_on((struct ssm_sv *) act->second_event)) { // @second_event
	ssm_desensitize(&act->trigger1);
      }	else return;
      
      ssm_assign_i32(&act->seconds, act->priority,
		 act->seconds.value + 1);

      printf("%d\n", act->seconds.value);
    } // end of loop
  }
  ssm_leave((struct ssm_act *) act, sizeof(act_report_seconds_t));
}


ssm_stepf_t step_main;

// Create a new activation record for main
act_main_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
		       ssm_depth_t depth)
{
  act_main_t *act = (act_main_t *) ssm_enter(sizeof(act_main_t), step_main, parent,
					 priority, depth);
  
  // Initialize managed variables
  ssm_initialize_event(&act->second);
  return act;
}

void step_main(struct ssm_act *cont)
{
  act_main_t *act = (act_main_t *) cont;

  switch (act->pc) {
  case 0:
    {                                         // fork
      ssm_depth_t new_depth = act->depth - 1; // 2 children
      ssm_priority_t new_priority = act->priority;
      ssm_priority_t pinc = 1 << new_depth;   // increment for each thread
      ssm_activate((struct ssm_act *)
		    ssm_enter_second_clock((struct ssm_act *) act,
				       new_priority, new_depth, &act->second));
      new_priority += pinc;
      ssm_activate((struct ssm_act *)
		    ssm_enter_report_seconds((struct ssm_act *) act,
					 new_priority, new_depth,
					 &act->second ));
    }
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
  ssm_time_t stop_at = (argc > 1 ? atoi(argv[1]) : 20) * SSM_SECOND;
  
  struct ssm_act top = { .step = main_return };  
  act_main_t *act = ssm_enter_main(&top, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH);  
  ssm_activate((struct ssm_act *) act);

  ssm_tick();

  while (ssm_next_event_time() != SSM_NEVER && ssm_now() < stop_at)
    ssm_tick();
  
  printf("simulated %lu seconds\n", ssm_now() / SSM_SECOND);
  
  return 0;
}
