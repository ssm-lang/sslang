// #include "blink.h"
#include "ssm-internal.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ssm_act_t *enter_main(struct ssm_act *caller, ssm_priority_t priority,
                      ssm_depth_t depth, ssm_value_t out, ssm_value_t *ret);

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

int main(int argc, char *argv[]) {
  ssm_mem_init(alloc_page, alloc_mem, free_mem);
  ssm_time_t stop_at = argc > 1 ? atoi(argv[1]) : 500; // SSM_MILLISECOND * 500;

  ssm_value_t led = ssm_new_sv(ssm_marshal(0));
  ssm_value_t ret;

  ssm_activate(
      enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH, led, &ret));

  ssm_tick();
  while (ssm_next_event_time() != SSM_NEVER && ssm_now() < stop_at) {
    ssm_tick();
    printf("%12lu led: %d\n", ssm_now(), ssm_unmarshal(ssm_deref(led)));
  }
  return 0;
}
