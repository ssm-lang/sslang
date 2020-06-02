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
  ACTIVATION_RECORD_FIELDS;
  cv_event_t second;
} act_main_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_event_t *second_event;
  cv_event_t timer;
  trigger_t trigger1;
} act_second_clock_t;

typedef struct {
  ACTIVATION_RECORD_FIELDS;
  cv_event_t *second_event;
  cv_int_t seconds;
  trigger_t trigger1;
} act_report_seconds_t;

stepf_t step_second_clock;

act_second_clock_t *
enter_second_clock(rar_t *parent, priority_t priority,
		   pdepth_t priority_depth, cv_event_t *second_event)
{
  assert(parent);
  assert(second_event);

  act_second_clock_t *act = (act_second_clock_t *)
    enter(sizeof(act_second_clock_t), step_second_clock,
	  parent, priority, priority_depth);
  act->second_event = second_event;
  initialize_event( &(act->timer) );
  act->trigger1.rar = (rar_t *) act;

  return act;
}

void step_second_clock(rar_t *cont)
{
  act_second_clock_t *act = (act_second_clock_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      assign_event(act->second_event, act->priority); // seconds = Event

      // after 1s timer = Event
      later_event(&act->timer, now + 1 * TICKS_PER_SECOND);

      sensitize((cv_t *) &act->timer, &act->trigger1); // await @timer
      act->pc = 1;
      return;
    case 1:
      if (event_on((cv_t *) &act->timer)) { // @timer
	desensitize(&act->trigger1);
      }	else return;
    } // end of loop
  }
  leave((rar_t *) act, sizeof(act_second_clock_t));
}

stepf_t step_report_seconds;

act_report_seconds_t *
enter_report_seconds(rar_t *parent, priority_t priority,
		   pdepth_t priority_depth, cv_event_t *second_event)
{
  assert(parent);
  assert(second_event);

  act_report_seconds_t *act = (act_report_seconds_t *)
    enter(sizeof(act_report_seconds_t), step_report_seconds,
	  parent, priority, priority_depth);
  act->second_event = second_event;
  initialize_int(&act->seconds, 0);
  act->trigger1.rar = (rar_t *) act;
  
  return act;
}

void step_report_seconds(rar_t *cont)
{
  act_report_seconds_t *act = (act_report_seconds_t *) cont;

  switch (act->pc) {
  case 0:
    for (;;) { // loop
      sensitize((cv_t *) act->second_event, &act->trigger1); // await @timer
      act->pc = 1;
      return;
    case 1:
      if (event_on((cv_t *) act->second_event)) { // @second_event
	desensitize(&act->trigger1);
      }	else return;
      
      assign_int(&act->seconds, act->priority,
		 act->seconds.value + 1);

      printf("%d\n", act->seconds.value);
    } // end of loop
  }
  leave((rar_t *) act, sizeof(act_report_seconds_t));
}


stepf_t step_main;

// Create a new activation record for main
act_main_t *enter_main(rar_t *parent, priority_t priority,
		       pdepth_t priority_depth)
{
  act_main_t *act = (act_main_t *) enter(sizeof(act_main_t), step_main, parent,
					 priority, priority_depth);
  
  // Initialize managed variables
  initialize_event(&act->second);
  return act;
}

void step_main(rar_t *cont)
{
  act_main_t *act = (act_main_t *) cont;

  switch (act->pc) {
  case 0:
    {                                               // fork
      pdepth_t new_depth = act->priority_depth - 1; // 2 children
      priority_t new_priority = act->priority;
      priority_t pinc = 1 << new_depth;             // increment for each thread
      fork((rar_t *)              // clock clk
		    enter_second_clock((rar_t *) act,
				       new_priority, new_depth, &act->second));
      new_priority += pinc;
      fork((rar_t *)              // dff clk d1 q1
		    enter_report_seconds((rar_t *) act,
					 new_priority, new_depth,
					 &act->second ));
    }
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
  ssm_time_t stop_at = argc > 1 ? atoi(argv[1]) : 20 * TICKS_PER_SECOND;
  
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
