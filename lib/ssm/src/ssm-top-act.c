/** @file ssm-top-act.c
 *  @brief Parent of the top-level activation record (entry point).
 */
#include <ssm.h>

/** @brief The top-level parent does nothing. */
static void ssm_top_return(ssm_act_t *act) { return; }

ssm_act_t ssm_top_parent = { .step = ssm_top_return };
