#include "doubleblink.h"
#include "ssm.h"
#include <stdio.h>

void top_return(act_t *act)
{
  return;
}

int main(int argc, char *argv[])  
{
  peng_time_t stop_at = argc > 1 ? atoi(argv[1]) : MILLISECOND_TICKS(500);

  sv_int_t led;
  initialize_int(&led);
  led.value = 0;
  
  act_t top = { .step = top_return };
  fork_routine( (act_t *) enter_main(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT,
				     &led) );
  
  tick();
  while (event_queue_len > 0 && now < stop_at) {
    now = next_event_time();
    tick();
    printf("%12lu led: %d\n", now, led.value);
  }
  return 0;
}
