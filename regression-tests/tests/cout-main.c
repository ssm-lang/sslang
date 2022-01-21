#include "ssm-internal.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  ssm_act_t act;
  ssm_value_t cout;
  ssm_trigger_t trigger1;
} cout_act_t;

void step_cout(ssm_act_t *sact);
ssm_act_t *enter_cout(ssm_act_t *parent, ssm_priority_t priority,
                      ssm_depth_t depth, ssm_value_t cout) {
  cout_act_t *cont = container_of(
      ssm_enter(sizeof(cout_act_t), step_cout, parent, priority, depth),
      cout_act_t, act);
  cont->cout = cout;
  cont->trigger1.act = &cont->act;
  return &cont->act;
}

void step_cout(ssm_act_t *act) {
  cout_act_t *cont = container_of(act, cout_act_t, act);

  switch (act->pc) {
  case 0:
    ssm_sensitize(cont->cout, &cont->trigger1);
    for (;;) {
      act->pc = 1;
      return;
    case 1:
      if (ssm_to_sv(cont->cout)->last_updated == ssm_now()) {
        if (ssm_unmarshal(ssm_deref(cont->cout)) == 0) {
          ssm_desensitize(&cont->trigger1);
          ssm_leave(act, sizeof(cout_act_t));
          return;
        }
        putchar(ssm_unmarshal(ssm_deref(cont->cout))); // unmarshal value before sending to C IO function
      }
    }
  }
}

struct ssm_act *enter_main(struct ssm_act *caller, ssm_priority_t priority,
                           ssm_depth_t depth, ssm_value_t cout, ssm_value_t *__ret);

void ssm_throw(enum ssm_error reason, const char *file, int line,
               const char *func) {
  printf("SSM error at %s:%s:%d: reason: %d\n", file, func, line, reason);
  exit(1);
}

#define MAX_PAGES 2048
static void *pages[MAX_PAGES];
static size_t allocated_pages = 0;

static void *alloc_page(void) {
  if (allocated_pages >= MAX_PAGES) {
    SSM_THROW(SSM_EXHAUSTED_MEMORY);
    exit(3);
  }
  void *m = pages[allocated_pages++] = malloc(SSM_MEM_PAGE_SIZE);
  memset(m, 0, SSM_MEM_PAGE_SIZE);
  return m;
}

static void *alloc_mem(size_t size) { return malloc(size); }

static void free_mem(void *mem, size_t size) { free(mem); }

int main(void) {
  ssm_mem_init(alloc_page, alloc_mem, free_mem);
  ssm_value_t cout = ssm_new_sv(ssm_marshal(0));
  ssm_value_t ret;

  {
    ssm_depth_t new_depth = SSM_ROOT_DEPTH - 1; // 2 children
    ssm_priority_t new_priority = SSM_ROOT_PRIORITY;
    ssm_priority_t pinc = 1 << new_depth;
    ssm_dup(cout);
    ssm_activate(enter_main(&ssm_top_parent, new_priority, new_depth, cout, &ret));
    new_priority += pinc;
    ssm_dup(cout);
    ssm_activate(enter_cout(&ssm_top_parent, new_priority, new_depth, cout));
  }
  do
    ssm_tick();
  while (ssm_next_event_time() != SSM_NEVER);
  ssm_drop(cout);
  return 0;
}
