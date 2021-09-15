#include "doubleblink.h"
#include "ssm.h"
#include <stdio.h>

void top_return(ssm_act_t *act) { return; }

int main(int argc, char *argv[]) {
  ssm_time_t stop_at = argc > 1 ? atoi(argv[1]) : SSM_MILLISECOND * 500;

  ssm_i32_t led;
  ssm_initialize_i32(&led);
  led.value = 0;

  ssm_act_t top = {.step = top_return};
  ssm_activate(
      (ssm_act_t *)enter_main(&top, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH, &led));

  ssm_tick();
  while (ssm_next_event_time() != SSM_NEVER && ssm_now() < stop_at) {
    ssm_tick();
    printf("%12lu led: %d\n", ssm_now(), led.value);
  }
  return 0;
}
