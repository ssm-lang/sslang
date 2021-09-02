#include "ssm.h"

void ssm_assign_event(ssm_event_t *v, ssm_priority_t prio)
{
  assert(v);
  v->sv.last_updated = ssm_now();
  ssm_trigger(&v->sv, prio);
}

static void ssm_update_event(ssm_sv_t *v)
{
}

void ssm_initialize_event(ssm_event_t *v)
{
  ssm_initialize(&v->sv, ssm_update_event);
}
