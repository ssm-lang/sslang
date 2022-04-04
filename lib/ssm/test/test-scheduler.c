#include <ssm-internal.h>
#include <ssm.h>
#include <stdio.h>
#include <string.h>

#ifndef SSM_DEBUG
#error "SSM_DEBUG undefined; it should be defined for the library"
#endif

#define DUMMY_VALUE ssm_marshal(0)

typedef uint16_t q_idx_t;
extern ssm_sv_t *event_queue[];
extern q_idx_t event_queue_len;
extern void event_queue_consistency_check(void);
extern void event_queue_percolate_down(q_idx_t, ssm_sv_t *);

extern ssm_act_t *act_queue[];
extern q_idx_t act_queue_len;
extern void act_queue_consistency_check(void);
extern void act_queue_percolate_down(q_idx_t, ssm_act_t *);

#define NUM_VARIABLES 1024
ssm_value_t variables[NUM_VARIABLES];

#define NUM_ACTS 1024
ssm_act_t acts[NUM_ACTS];

#define NUM_TRIGGERS 1024
ssm_trigger_t triggers[NUM_TRIGGERS];

void check_starts_initialized() {
  SSM_ASSERT(ssm_now() == 0L);
  SSM_ASSERT(event_queue_len == 0);
  SSM_ASSERT(act_queue_len == 0);
}

void reset_all() {
  for (int i = 0; i < NUM_VARIABLES; i++)
    ssm_drop(variables[i]);
  ssm_reset();
  for (int i = 0; i < NUM_VARIABLES; i++)
    variables[i] = ssm_new_sv(DUMMY_VALUE);

  check_starts_initialized();
}

void event_queue_basic() {
  reset_all();
  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);
  SSM_ASSERT(ssm_to_sv(variables[0])->last_updated != ssm_now());
  ssm_later(variables[0], 1, DUMMY_VALUE);
  SSM_ASSERT(event_queue_len == 1);
  SSM_ASSERT(ssm_next_event_time() == 1);
  event_queue_consistency_check();
  ssm_tick();
  SSM_ASSERT(ssm_to_sv(variables[0])->last_updated == ssm_now());
  SSM_ASSERT(ssm_now() == 1);
  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);
  SSM_ASSERT(event_queue_len == 0);
}

void print_event_queue() {
  printf("Event queue: ");
  for (int i = 1; i <= event_queue_len; i++)
    if (event_queue[i])
      printf("%c", (char)event_queue[i]->later_time);
    else
      printf("[NULL]");
  printf("\n");
}

/*** Fill the event queue with events whose times are the characters
 * in the input string, schedule each, then repeatedly remove the
 * earliest from the queue, making sure they come out in the order
 * given by the expected string.
 */
void event_queue_sort_string(const char *input, const char *expected) {
  reset_all();
  SSM_ASSERT(event_queue_len == 0);
  ssm_value_t *var = variables;
  for (const char *cp = input; *cp; ++cp, ++var) {
    ssm_later(*var, (ssm_time_t)*cp, DUMMY_VALUE);
    event_queue_consistency_check();
  }

  while (event_queue_len) {
    char c = (char)event_queue[1]->later_time;
    printf("%c", c);
    SSM_ASSERT(c == *expected++);
    event_queue[1]->later_time = SSM_NEVER;
    ssm_sv_t *to_insert = event_queue[event_queue_len--]; // get last

    if (event_queue_len) // Was this the last?
      event_queue_percolate_down(1, to_insert);
    event_queue_consistency_check();
  }
  printf("\n");
}

/** Schedule the characters of a string as events whose times are the
 * characters, then reschedule each, replacing non-space characters with
 * lowercase versions of each, then run ssm_tick() until the event queue is
 * empty.  The sequence of output characters is a sorted, unique list of
 * all the characters in the string"
 *
 */
void event_queue_reschedule_string(const char *input, const char *expected) {
  reset_all();
  ssm_value_t *var = variables;

  for (const char *cp = input; *cp; ++cp, ++var) {
    ssm_later(*var, (ssm_time_t)*cp, DUMMY_VALUE);
    event_queue_consistency_check();
  }

  // print_event_queue();

  // Reschedule each element, swapping its case

  var = variables;
  for (const char *cp = input; *cp; ++cp, ++var) {
    if (*cp != ' ') {
      if (*cp > 'Z')
        ssm_later(*var, (ssm_time_t)*cp + 'A' - 'a', DUMMY_VALUE);
      else
        ssm_later(*var, (ssm_time_t)*cp + 'a' - 'A', DUMMY_VALUE);
    }
    event_queue_consistency_check();
  }

  // print_event_queue();

  while (ssm_next_event_time() != SSM_NEVER) {
    ssm_tick();
    event_queue_consistency_check();
    char c = ssm_now();
    printf("%c", c);
    SSM_ASSERT(c == *expected++);
  }
  printf("\n");
}

/*** Fill the event queue with events times by the characters in the
 * string, then unschedule the first n and verify what's left
 */
void event_queue_unschedule_string(const char *input, int n,
                                   const char *expected) {
  reset_all();
  ssm_value_t *var = variables;
  for (const char *cp = input; *cp; ++cp, ++var) {
    ssm_later(*var, (ssm_time_t)*cp, DUMMY_VALUE);
    event_queue_consistency_check();
  }

  var = variables;
  for (int j = n; j; ++var, --j) {
    ssm_unschedule(ssm_to_sv(*var));
    event_queue_consistency_check();
  }

  // Unschedule them again; it shouldn't matter

  var = variables;
  for (int j = n; j; ++var, --j) {
    ssm_unschedule(ssm_to_sv(*var));
    event_queue_consistency_check();
  }

  while (event_queue_len) {
    char c = (char)event_queue[1]->later_time;
    printf("%c", c);
    SSM_ASSERT(c == *expected++);
    event_queue[1]->later_time = SSM_NEVER;
    ssm_sv_t *to_insert = event_queue[event_queue_len--]; // get last

    if (event_queue_len) // Was this the last?
      event_queue_percolate_down(1, to_insert);
    event_queue_consistency_check();
  }
  printf("\n");
}

void act_queue_basic() {
  reset_all();
  SSM_ASSERT(act_queue_len == 0);
  SSM_ASSERT(!acts[0].scheduled);
  ssm_activate(acts);
  SSM_ASSERT(act_queue_len == 1);
  act_queue_consistency_check();
  ssm_tick();
  SSM_ASSERT(act_queue_len == 0);
}

void act_queue_sort_string(const char *input, const char *expected) {
  reset_all();
  SSM_ASSERT(act_queue_len == 0);
  ssm_act_t *act = acts;
  for (const char *cp = input; *cp; ++cp, ++act) {
    act->scheduled = false;
    act->priority = *cp;
    ssm_activate(act);
    act_queue_consistency_check();
  }

  while (act_queue_len) {
    char c = (char)act_queue[1]->priority;
    printf("%c", c);
    SSM_ASSERT(c == *expected++);
    ssm_act_t *to_insert = act_queue[act_queue_len--]; // get last

    if (act_queue_len) // Was this the last?
      act_queue_percolate_down(1, to_insert);
    act_queue_consistency_check();
  }
  printf("\n");
}

const char *next_expected;

void check_priority_step(ssm_act_t *act) {
  SSM_ASSERT(act);
  char c = act->priority;
  printf("%c", c);
  SSM_ASSERT(c == *next_expected++);
}

/** Enter the string into the activation record queue with priorities
 * then run ssm_tick() making the "step" function print and check the
 * priority of the activation record just executed.
 */
void act_queue_sort_tick(const char *input, const char *expected) {
  reset_all();
  SSM_ASSERT(act_queue_len == 0);
  ssm_act_t *act = acts;
  for (const char *cp = input; *cp; ++cp, ++act) {
    act->step = check_priority_step;
    act->scheduled = false;
    act->priority = *cp;
    ssm_activate(act);
    SSM_ASSERT(act->scheduled);
    act_queue_consistency_check();
  }

  next_expected = expected;

  ssm_tick();
  act_queue_consistency_check();

  SSM_ASSERT(*next_expected ==
             0); // Did we end up at the end of the expected string?

  SSM_ASSERT(act_queue_len ==
             0); // Should have emptied the activation record queue
  printf("\n");

  // Make sure all the activation records were unscheduled
  act = acts;
  for (const char *cp = input; *cp; ++cp, ++act)
    SSM_ASSERT(!act->scheduled);
}

bool step0ran, step1ran;

void step0(ssm_act_t *act) {
  step0ran = true;
  printf("step0 ");
}
void step1(ssm_act_t *act) {
  step1ran = true;
  printf("step1 ");
}

void trigger_basic() {
  reset_all();

  acts[0].step = step0;
  acts[0].priority = 3;
  SSM_ASSERT(!acts[0].scheduled);
  acts[1].step = step1;
  acts[1].priority = 4;
  SSM_ASSERT(!acts[1].scheduled);

  triggers[0].act = &acts[0];
  triggers[1].act = &acts[1];
  triggers[2].act = &acts[0];

  step0ran = step1ran = false;

  ssm_sensitize(variables[0], &triggers[0]);
  ssm_sensitize(variables[0], &triggers[1]);
  ssm_sensitize(variables[0], &triggers[2]);

  ssm_later(variables[0], 1, DUMMY_VALUE);

  ssm_tick();
  printf("\n");
  SSM_ASSERT(ssm_now() == 1);

  SSM_ASSERT(step0ran);
  SSM_ASSERT(step1ran);

  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);

  ssm_later(variables[0], 2, DUMMY_VALUE);

  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 2);
  SSM_ASSERT(step0ran);
  SSM_ASSERT(step1ran);

  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);

  ssm_later(variables[0], 3, DUMMY_VALUE);
  ssm_desensitize(&triggers[1]);

  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 3);
  SSM_ASSERT(step0ran);
  SSM_ASSERT(!step1ran);

  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);

  ssm_later(variables[0], 4, DUMMY_VALUE);
  ssm_desensitize(&triggers[0]);

  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 4);
  SSM_ASSERT(step0ran);
  SSM_ASSERT(!step1ran);

  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);

  ssm_later(variables[0], 5, DUMMY_VALUE);
  ssm_desensitize(&triggers[2]);

  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 5);
  SSM_ASSERT(!step0ran);
  SSM_ASSERT(!step1ran);

  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);

  ssm_later(variables[0], 6, DUMMY_VALUE);
  ssm_sensitize(variables[0], &triggers[1]);
  ssm_sensitize(variables[0], &triggers[0]);

  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 6);
  SSM_ASSERT(step0ran);
  SSM_ASSERT(step1ran);

  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);

  ssm_desensitize(&triggers[0]);
  ssm_later(variables[0], 7, DUMMY_VALUE);

  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 7);
  SSM_ASSERT(!step0ran);
  SSM_ASSERT(step1ran);

  SSM_ASSERT(ssm_next_event_time() == SSM_NEVER);

  ssm_sensitize(variables[0], &triggers[0]);

  for (ssm_trigger_t *trig = ssm_to_sv(variables[0])->triggers; trig;
       trig = trig->next)
    if (trig->act->priority > 0)
      ssm_activate(trig->act);
  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 7);
  SSM_ASSERT(step0ran);
  SSM_ASSERT(step1ran);

  for (ssm_trigger_t *trig = ssm_to_sv(variables[0])->triggers; trig;
       trig = trig->next)
    if (trig->act->priority > 3)
      ssm_activate(trig->act);

  step0ran = step1ran = false;
  ssm_tick();
  printf("\n");

  SSM_ASSERT(ssm_now() == 7);
  SSM_ASSERT(!step0ran);
  SSM_ASSERT(step1ran);
}

void vacuous_step(ssm_act_t *act) {}

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

  for (int i = 0; i < NUM_VARIABLES; i++)
    variables[i] = ssm_new_sv(DUMMY_VALUE);

  for (int i = 0; i < NUM_ACTS; i++)
    acts[i] = (ssm_act_t){
        .step = vacuous_step,
        .caller = &ssm_top_parent,
        .pc = 0,
        .children = 0,
        .priority = 0,
        .depth = 0,
        .scheduled = false,
    };

  check_starts_initialized();

  event_queue_basic();
  event_queue_sort_string("", "");
  event_queue_sort_string("ABC", "ABC");
  event_queue_sort_string("CBA", "ABC");
  event_queue_sort_string("ABCD", "ABCD");
  event_queue_sort_string("DBCA", "ABCD");
  event_queue_sort_string("DCBA", "ABCD");
  event_queue_sort_string("MR JOCK TV QUIZ PHD BAGS FEW LYNX",
                          "       ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  event_queue_sort_string("WALTZ BAD NYMPH FOR QUICK JIGS VEX",
                          "      AABCDEFGHIIJKLMNOPQRSTUVWXYZ");
  event_queue_sort_string("SPHINX OF BLACK QUARTZ JUDGE MY VOW",
                          "      AABCDEFGHIJKLMNOOPQRSTUUVWXYZ");
  event_queue_sort_string("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG",
                          "        ABCDEEEFGHHIJKLMNOOOOPQRRSTTUUVWXYZ");
  event_queue_sort_string("The Quick Brown Fox Jumps Over The Lazy Dog",
                          "        BDFJLOQTTaceeeghhikmnoooprrsuuvwxyz");

  event_queue_reschedule_string("ABC", "abc");
  event_queue_reschedule_string("CBA", "abc");
  event_queue_reschedule_string("ABCD", "abcd");
  event_queue_reschedule_string("DBCA", "abcd");
  event_queue_reschedule_string("DCBA", "abcd");
  event_queue_reschedule_string("ABBA SAID GO AWAY", " abdgioswy");
  event_queue_reschedule_string("MR JOCK TV QUIZ PHD BAGS FEW LYNX",
                                " abcdefghijklmnopqrstuvwxyz");
  event_queue_reschedule_string("WALTZ BAD NYMPH FOR QUICK JIGS VEX",
                                " abcdefghijklmnopqrstuvwxyz");
  event_queue_reschedule_string("SPHINX OF BLACK QUARTZ JUDGE MY VOW",
                                " abcdefghijklmnopqrstuvwxyz");
  event_queue_reschedule_string("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG",
                                " abcdefghijklmnopqrstuvwxyz");
  event_queue_reschedule_string("The Quick Brown Fox Jumps Over The Lazy Dog",
                                " ACEGHIKMNOPRSUVWXYZbdfjloqt");

  event_queue_unschedule_string("ABCD", 1, "BCD");
  event_queue_unschedule_string("ABCD", 2, "CD");
  event_queue_unschedule_string("ABCD", 3, "D");
  event_queue_unschedule_string("bcda", 2, "ad");

  event_queue_unschedule_string("The Quick Brown Fox Jumps Over The Lazy Dog",
                                4, "       BDFJLOQTaceeghikmnoooprrsuuvwxyz");
  event_queue_unschedule_string("The Quick Brown Fox Jumps Over The Lazy Dog",
                                15, "      DFJLOTaeeghmooprsuvxyz");

  act_queue_basic();

  act_queue_sort_string("", "");
  act_queue_sort_string("ABC", "ABC");
  act_queue_sort_string("CBA", "ABC");
  act_queue_sort_string("ABCD", "ABCD");
  act_queue_sort_string("DBCA", "ABCD");
  act_queue_sort_string("DCBA", "ABCD");
  act_queue_sort_string("MR JOCK TV QUIZ PHD BAGS FEW LYNX",
                        "       ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  act_queue_sort_string("WALTZ BAD NYMPH FOR QUICK JIGS VEX",
                        "      AABCDEFGHIIJKLMNOPQRSTUVWXYZ");
  act_queue_sort_string("SPHINX OF BLACK QUARTZ JUDGE MY VOW",
                        "      AABCDEFGHIJKLMNOOPQRSTUUVWXYZ");
  act_queue_sort_string("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG",
                        "        ABCDEEEFGHHIJKLMNOOOOPQRRSTTUUVWXYZ");
  act_queue_sort_string("The Quick Brown Fox Jumps Over The Lazy Dog",
                        "        BDFJLOQTTaceeeghhikmnoooprrsuuvwxyz");

  act_queue_sort_tick("", "");
  act_queue_sort_tick("ABC", "ABC");
  act_queue_sort_tick("CBA", "ABC");
  act_queue_sort_tick("ABCD", "ABCD");
  act_queue_sort_tick("DBCA", "ABCD");
  act_queue_sort_tick("DCBA", "ABCD");

  act_queue_sort_tick("MR JOCKTVQUIZPHDBAGSFEWLYNX",
                      " ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  act_queue_sort_tick("MR JOCK TV QUIZ PHD BAGS FEW LYNX",
                      "       ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  act_queue_sort_tick("WALTZ BAD NYMPH FOR QUICK JIGS VEX",
                      "      AABCDEFGHIIJKLMNOPQRSTUVWXYZ");
  act_queue_sort_tick("SPHINX OF BLACK QUARTZ JUDGE MY VOW",
                      "      AABCDEFGHIJKLMNOOPQRSTUUVWXYZ");
  act_queue_sort_tick("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG",
                      "        ABCDEEEFGHHIJKLMNOOOOPQRRSTTUUVWXYZ");
  act_queue_sort_tick("The Quick Brown Fox Jumps Over The Lazy Dog",
                      "        BDFJLOQTTaceeeghhikmnoooprrsuuvwxyz");

  trigger_basic();

  for (int i = 0; i < NUM_VARIABLES; i++)
    ssm_drop(variables[i]);

  printf("PASSED\n");
  return 0;
}
