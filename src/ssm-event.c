#include "ssm.h"

static void ssm_update_event(ssm_event_t *v)
{
}

void ssm_initialize_event(ssm_event_t *v)
{
  ssm_initialize(v, ssm_update_event);
}
