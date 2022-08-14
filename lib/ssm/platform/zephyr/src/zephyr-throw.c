/**
 * Override for ssm_throw to properly panic and log reason.
 */
// TODO: test that this actually works, i.e., is linked in

#include <ssm.h>
#include <stdlib.h>
#include <logging/log.h>
#include <logging/log_ctrl.h>

LOG_MODULE_REGISTER(ssm_throw);

/** Override ssm_throw function with some platform-specific logging. */
void ssm_throw(ssm_error_t reason, const char *file, int line, const char *func) {
  log_panic();
  LOG_ERR("Threw error code %d at time: %016llx", reason, ssm_now());
  switch (reason) {
  case SSM_INTERNAL_ERROR:
    LOG_ERR("Unforeseen internal error.");
    break;
  case SSM_EXHAUSTED_ACT_QUEUE:
    LOG_ERR("Tried to insert into full activation record queue.");
    break;
  case SSM_EXHAUSTED_EVENT_QUEUE:
    LOG_ERR("Tried to insert into full event queue.");
    break;
  case SSM_EXHAUSTED_MEMORY:
    LOG_ERR("Could not allocate more memory.");
    break;
  case SSM_EXHAUSTED_PRIORITY:
    LOG_ERR("Tried to exceed available recursion depth.");
    break;
  case SSM_INVALID_TIME:
    LOG_ERR(
        "Invalid time, e.g., scheduled delayed assignment at an earlier time.");
    break;
  default:
    LOG_ERR("Unknown/platform-specific error.");
    break;
  }
  exit(reason);
}
