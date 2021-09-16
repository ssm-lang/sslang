#include "ssm.h"

static void ssm_top_return(ssm_act_t *act) { return; }

ssm_act_t ssm_top_parent = { .step = ssm_top_return };
