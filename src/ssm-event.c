#include "ssm.h"

void ssm_assign_event(ssm_event_t *v, ssm_priority_t prio)
{
  assert(v);
  v->last_updated = ssm_now();
  ssm_trigger(v, prio);
}

static void ssm_update_event(ssm_event_t *v)
{
}

void ssm_initialize_event(ssm_event_t *v)
{
  ssm_initialize(v, ssm_update_event);
}
