#include "ssm.h"
#include <stdio.h>

#ifndef SSM_DEBUG
#error "SSM_DEBUG undefined; it should be defined for the library"
#endif

typedef uint16_t q_idx_t;
extern ssm_sv_t *event_queue[];
extern q_idx_t event_queue_len;
extern void event_queue_consistency_check(void);
extern void event_queue_percolate_down(q_idx_t hole, ssm_sv_t *event);

#define NUM_VARIABLES 2048
ssm_sv_t variables[NUM_VARIABLES];

void print_event_queue()
{
  for (int i = 1 ; i <= event_queue_len ; i++)
    if (event_queue[i])
      printf("%c", (char) event_queue[i]->later_time);
    else
      printf("[NULL]");
  printf("\n");
}

void event_queue_sort_string(const char *input)
{
  assert(event_queue_len == 0);
  ssm_sv_t *var = variables;
  for (const char *cp = input ; *cp ; ++cp, ++var) {
    var->later_time = SSM_NEVER;
    ssm_schedule(var, (ssm_time_t) *cp);
    event_queue_consistency_check();
  }

  while (event_queue_len) {
    printf("%c", (char) event_queue[1]->later_time );
    ssm_sv_t *to_insert = event_queue[event_queue_len--]; // get last

    if (event_queue_len) // Was this the last?
      event_queue_percolate_down(1, to_insert); 
  }
  printf("\n");
}

int main()
{
  event_queue_sort_string("");
  event_queue_sort_string("ABC");
  event_queue_sort_string("CBA");
  event_queue_sort_string("ABCD");
  event_queue_sort_string("DBCA");
  event_queue_sort_string("DCBA");
  event_queue_sort_string("MR JOCK TV QUIZ PHD BAGS FEW LYNX");
  event_queue_sort_string("WALTZ BAD NYMPH FOR QUICK JIGS VEX");
  event_queue_sort_string("SPHINX OF BLACK QUARTZ JUDGE MY VOW");
  event_queue_sort_string("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG");
  return 0;
}
