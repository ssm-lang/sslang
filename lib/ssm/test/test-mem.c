#include <ssm-internal.h> /* I don't want to have to do this */
#include <ssm.h>

#include <stdio.h>

#ifndef CONFIG_MEM_STATS
#error "CONFIG_MEM_STATS must be defined to run these tests"
#endif

#ifndef SSM_NUM_PAGES
#define SSM_NUM_PAGES 32
#endif

void ssm_throw(enum ssm_error reason, const char *file, int line,
               const char *func) {
  printf("SSM error at %s:%s:%d: reason: %d\n", file, func, line, reason);
  exit(1);
}

static char mem[SSM_NUM_PAGES][SSM_MEM_PAGE_SIZE] = {0};
static size_t allocated_pages = 0;

static void *alloc_page(void) {
  if (allocated_pages >= SSM_NUM_PAGES)
    SSM_THROW(SSM_EXHAUSTED_MEMORY);
  return mem[allocated_pages++];
}

static void *alloc_mem(size_t size) { return malloc(size); }

static void free_mem(void *mem, size_t size) { free(mem); }

void print_stats(ssm_mem_statistics_t *stats) {
  ssm_mem_statistics_collect(stats);

  printf("sizeof(struct ssm_mm) = %lu\n", stats->sizeof_ssm_mm);
  printf("page size %lu\n", stats->page_size);
  printf("pages allocated %lu\n", stats->pages_allocated);
  printf("objects allocated %lu\n", stats->objects_allocated);
  printf("objects freed %lu\n", stats->objects_freed);
  printf("live objects %lu\n", stats->live_objects);

  size_t pool_count = stats->pool_count;

  printf("%lu pools\n", pool_count);

  for (size_t i = 0; i < pool_count; i++) {
    printf("pool %3lu: pages %3lu  block-size %5lu  free-blocks %5lu\n", i,
           stats->pool[i].pages_allocated, stats->pool[i].block_size,
           stats->pool[i].free_list_length);
  }

  printf("\n");
}

int main() {
  int num_errors = 0;
  ssm_mem_statistics_t stats;

  ssm_value_t objects[100];

  ssm_mem_init(alloc_page, alloc_mem, free_mem);

  printf("--- Initial state: no live objects or pages allocated\n");
  print_stats(&stats);

  objects[0] = ssm_new_blob(1);

  printf("--- One blob of size 1 allocated\n");
  print_stats(&stats);

  ssm_drop(objects[0]);

  printf("--- Back to no objects\n");
  print_stats(&stats);

  objects[0] = ssm_new_blob(1);  // pool 0
  objects[1] = ssm_new_blob(11); // pool 0
  objects[2] = ssm_new_blob(12); // pool 0

  printf("--- Three objects in pool 0\n");
  print_stats(&stats);

  objects[3] = ssm_new_blob(13); // pool 1
  objects[4] = ssm_new_blob(59); // pool 1
  objects[5] = ssm_new_blob(60); // pool 1

  printf("--- Three new objects in pool 1\n");
  print_stats(&stats);

  objects[6] = ssm_new_blob(61);  // pool 2
  objects[7] = ssm_new_blob(251); // pool 2
  objects[8] = ssm_new_blob(252); // pool 2

  printf("--- Three new objects in pool 2\n");
  print_stats(&stats);

  objects[9] = ssm_new_blob(253); // pool 3

  printf("--- One new object in pool 3\n");
  print_stats(&stats);

  ssm_drop(objects[9]);

  printf("--- Pool 3 empty\n");
  print_stats(&stats);

  ssm_drop(objects[8]);
  ssm_drop(objects[6]);
  ssm_drop(objects[7]);

  printf("--- Pool 2 empty\n");
  print_stats(&stats);

  ssm_drop(objects[0]);
  ssm_drop(objects[1]);
  ssm_drop(objects[2]);

  printf("--- Pool 0 empty\n");
  print_stats(&stats);

  ssm_drop(objects[3]);
  ssm_drop(objects[4]);
  ssm_drop(objects[5]);

  printf("--- Pool 1 empty\n");
  print_stats(&stats);

  if (num_errors == 0)
    printf("PASSED\n");
  else
    printf("FAILED (%d errors)\n", num_errors);
  return num_errors;
}
