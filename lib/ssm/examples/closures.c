#include "ssm-examples.h"
#include "ssm.h"
#include <stdio.h>

/*
This example demonstrates how the SSM runtime's closure API should be used, and
how it may be optimized. It shows function applications categorized along two
axes: whether the function is global, local and used later, or local and used
for the last time; and whether the application reduces (i.e., the caller process
should yield to allow a child process to run). The following program
demonstrates all six combinations thereof:

foo x y z = x + y + z

main =
  let f = foo 1     // global, non-reducing
      g = f 2       // local, non-last, non-reducing
      a = foo 3 4 5 // global, reducing
      h = f a       // local, last, non-reducing
      b = g a       // local, non-last, reducing
      c = g b       // local, last, reducing

  h (a + b + c)     // local, last, reducing

should return:

  58

This file shows both an unoptimized implementation of this program, and
a selectively hand-optimized implementation designed to be instructive of the
kinds of optimizations the SSM closure API allows.

The optimized implementation is used by default. To select the unoptimized
implementation, define CLOSURES_UNOPTIMIZED (or compile this file with
-DCLOSURES_UNOPTIMIZED).
*/

typedef struct {
  ssm_act_t act;
  ssm_value_t x;
  ssm_value_t y;
  ssm_value_t z;
  ssm_value_t *__ret;
} act_foo_t;

ssm_stepf_t step_foo;

ssm_act_t *enter_foo(ssm_act_t *parent, ssm_priority_t priority,
                     ssm_depth_t depth, ssm_value_t *argv, ssm_value_t *__ret) {
  act_foo_t *cont = container_of(
      ssm_enter(sizeof(act_foo_t), step_foo, parent, priority, depth),
      act_foo_t, act);
  cont->x = argv[0];
  cont->y = argv[1];
  cont->z = argv[2];
  cont->__ret = __ret;
  return &cont->act;
}

void step_foo(ssm_act_t *act) {
  act_foo_t *cont = container_of(act, act_foo_t, act);
  *cont->__ret = ssm_marshal(ssm_unmarshal(cont->x) + ssm_unmarshal(cont->y) +
                             ssm_unmarshal(cont->z));
  ssm_drop(cont->x);
  ssm_drop(cont->y);
  ssm_drop(cont->z);
  ssm_leave(&cont->act, sizeof(act_foo_t));
}

#ifdef CLOSURES_UNOPTIMIZED
typedef struct {
  ssm_act_t act;
  ssm_value_t foo;
  ssm_value_t f;
  ssm_value_t g;
  ssm_value_t h;
  ssm_value_t a2;
  ssm_value_t a1;
  ssm_value_t a;
  ssm_value_t b;
  ssm_value_t c;
  ssm_value_t *__ret;
} act_main_t;
#else
/* With optimization, we may omit some local variables. */
typedef struct {
  ssm_act_t act;
  ssm_value_t f;
  ssm_value_t g;
  ssm_value_t h;
  ssm_value_t a;
  ssm_value_t b;
  ssm_value_t c;
  ssm_value_t *__ret;
} act_main_t;
#endif

ssm_stepf_t step_main;

ssm_act_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth, ssm_value_t *argv,
                          ssm_value_t *ret) {
  act_main_t *cont = container_of(
      ssm_enter(sizeof(act_main_t), step_main, parent, priority, depth),
      act_main_t, act);
  cont->__ret = ret;

  return &cont->act;
}

#ifdef CLOSURES_UNOPTIMIZED

void step_main(struct ssm_act *act) {
  act_main_t *cont = container_of(act, act_main_t, act);

  switch (act->pc) {
  case 0:;
    cont->foo = ssm_new_closure(&enter_foo, 3);

    ssm_closure_apply_auto(cont->foo, ssm_marshal(1), act, act->priority,
                           act->depth, &cont->f);
    if (ssm_has_children(act)) {
      act->pc = 1;
      return;
    case 1:;
    }

    ssm_closure_apply_auto(cont->f, ssm_marshal(2), act, act->priority,
                           act->depth, &cont->g);
    if (ssm_has_children(act)) {
      act->pc = 2;
      return;
    case 2:;
    }

    ssm_closure_apply_auto(cont->foo, ssm_marshal(3), act, act->priority,
                           act->depth, &cont->a2);
    if (ssm_has_children(act)) {
      act->pc = 3;
      return;
    case 3:;
    }
    ssm_closure_apply_auto(cont->a2, ssm_marshal(4), act, act->priority,
                           act->depth, &cont->a1);
    if (ssm_has_children(act)) {
      act->pc = 4;
      return;
    case 4:;
    }
    ssm_closure_apply_auto(cont->a1, ssm_marshal(5), act, act->priority,
                           act->depth, &cont->a);
    if (ssm_has_children(act)) {
      act->pc = 5;
      return;
    case 5:;
    }

    ssm_closure_apply_auto(cont->f, cont->a, act, act->priority, act->depth,
                           &cont->h);
    if (ssm_has_children(act)) {
      act->pc = 6;
      return;
    case 6:;
    }

    ssm_closure_apply_auto(cont->g, cont->a, act, act->priority, act->depth,
                           &cont->b);
    if (ssm_has_children(act)) {
      act->pc = 7;
      return;
    case 7:;
    }

    ssm_closure_apply_auto(cont->g, cont->b, act, act->priority, act->depth,
                           &cont->c);
    if (ssm_has_children(act)) {
      act->pc = 8;
      return;
    case 8:;
    }

    ssm_value_t h_arg =
        ssm_marshal(ssm_unmarshal(cont->a) + ssm_unmarshal(cont->b) +
                    ssm_unmarshal(cont->c));

    ssm_closure_apply_auto(cont->h, h_arg, act, act->priority, act->depth,
                           cont->__ret);
    if (ssm_has_children(act)) {
      act->pc = 9;
      return;
    case 9:;
    }

    ssm_drop(cont->c);
    ssm_drop(cont->b);
    ssm_drop(cont->b);
    ssm_drop(cont->a);
    ssm_drop(cont->a1);
    ssm_drop(cont->a2);
    ssm_drop(cont->h);
    ssm_drop(cont->g);
    ssm_drop(cont->f);
    ssm_drop(cont->foo);
    break;
  }
  ssm_leave(&cont->act, sizeof(*cont));
}

#else

/*
 * This implementation of main's step function demonstrates a few optimizations.
 * Of course, any decent optimizing compiler would probably realize it can
 * compute everything at compile-time and leave the code returning an integer
 * literal; instead, the optimizations we show here will be purposefully less
 * aggressive so as to be instructive. In particular, we will not attempt to
 * inline anything.
 *
 * However, we will exploit knowledge about whether a referenced value is
 * a top-level function (i.e., f), and whether a local variable is being used
 * for the last time. We also exploit knowledge that all variables used are
 * local, meaning if something is used for the last time, we know definitively
 * that it can be clobbered; in general (i.e., if variables are passed in via
 * arguments), we would need to check ssm_is_shared() before we call
 * ssm_closure_apply_final().
 */
void step_main(struct ssm_act *act) {
  act_main_t *cont = container_of(act, act_main_t, act);

  switch (act->pc) {
  case 0:;
    // No need to allocate a closure for a top-level functions.
    /* cont->foo = ssm_new_closure(&enter_foo, 3); */

    // We already know that foo is a top-level function that evaluates after
    // receiving 3 arguments, so we directly allocate f with a foo's enter
    // function, and then push an argument on. No need to dup() the argument
    // since we know it is packed integer value.
    cont->f = ssm_new_closure(&enter_foo, 3);
    ssm_closure_push(cont->f, ssm_marshal(1));
    /*
    ssm_closure_apply_auto(cont->foo, ssm_marshal(1), act, act->priority,
                           act->depth, &cont->f);
    if (ssm_has_children(act)) {
      act->pc = 1;
      return;
    case 1:;
    }
    */

    // This isn't the last time we use f, so we can't just modify it in-place.
    // Instead, we clone it using ssm_closure_apply() (no need to use the auto
    // variant since we know we don't need to dup() the argument).
    ssm_closure_apply(cont->f, ssm_marshal(2), act, act->priority, act->depth,
                      &cont->g);
    /*
    ssm_closure_apply_auto(cont->f, ssm_marshal(2), act, act->priority,
                           act->depth, &cont->g);
    */
    // Note that we can't just use ssm_closure_push() because we purposefully
    // "forget" that f is actually just foo partially-applied (for the sake of
    // being instructive). After ssm_closure_apply(), we must still check
    // whether we need to yield.
    if (ssm_has_children(act)) {
      act->pc = 2;
      return;
    case 2:;
    }

    // This is similar to g, except we know for a fact that we're fully-applying
    // global function f. Instead, we create a stack-allocated argv, to which we
    // push all arguments, and directly activate f without creating a closure.
    ssm_value_t foo_argv[3] = {ssm_marshal(3), ssm_marshal(4), ssm_marshal(5)};
    ssm_activate(enter_foo(act, act->priority, act->depth, foo_argv, &cont->a));
    // We already know that this produced a child, so we yield unconditionally.
    act->pc = 3;
    return;
  case 3:;
    /*
    ssm_closure_apply_auto(cont->foo, ssm_marshal(3), act, act->priority,
                           act->depth, &cont->a2);
    if (ssm_has_children(act)) {
      act->pc = 3;
      return;
    case 3:;
    }
    ssm_closure_apply_auto(cont->a2, ssm_marshal(4), act, act->priority,
                           act->depth, &cont->a1);
    if (ssm_has_children(act)) {
      act->pc = 4;
      return;
    case 4:;
    }
    ssm_closure_apply_auto(cont->a1, ssm_marshal(5), act, act->priority,
                           act->depth, &cont->a);
    if (ssm_has_children(act)) {
      act->pc = 5;
      return;
    case 5:;
    }
    */

    // This is the last time we use f, so we can use ssm_closure_apply_final(),
    // which logically drops f for us. Under the hood, it moves the closure
    // pointer from f to h (since this call does not reduce).
    ssm_closure_apply_final(cont->f, cont->a, act, act->priority, act->depth,
                            &cont->h);
    /*
    ssm_closure_apply_auto(cont->f, cont->a, act, act->priority, act->depth,
                           &cont->h);
    */
    // We've still "forgotten" whether applying f will lead to any children, so
    // we check whether we need to yield.
    if (ssm_has_children(act)) {
      act->pc = 6;
      return;
    case 6:;
    }

    // This is not the last time we use g, so we must make sure not to clobber
    // it while performing this application. However, in some cases, we can
    // surmise that this application must lead to a reduction (and not another
    // closure). In this case, we know applying g must reduce because its return
    // type is int, so we can inline ssm_closure_apply() and omit the
    // arg_cap/arg_count check.
    ssm_dups(ssm_closure_arg_count(cont->g), ssm_closure_argv(cont->g));
    ssm_closure_push(cont->g, cont->a);
    ssm_closure_activate(cont->g, act, act->priority, act->depth, &cont->b);
    ssm_closure_pop(cont->g);
    /*
    ssm_closure_apply_auto(cont->g, cont->a, act, act->priority, act->depth,
                           &cont->b);
    */
    // Furthermore, knowing that it must reduce means we can also yield
    // unconditionally.
    act->pc = 7;
    return;
  case 7:;
    /*
    if (ssm_has_children(act)) {
      act->pc = 7;
      return;
    case 7:;
    }
    */

    // This is the last time we use g, so we can use ssm_closure_apply_final(),
    // which logically drops g for us. Under the hood, it frees the closure
    // for us (since this call does reduce).
    //
    // (Note that there is an additional optimization opportunity we aren't
    // exploiting here: we can infer that g must reduce based on its return
    // type. To see how we exploit that information, see the application of h.)
    ssm_closure_apply_final(cont->g, cont->b, act, act->priority, act->depth,
                            &cont->c);
    /*
    ssm_closure_apply_auto(cont->g, cont->b, act, act->priority, act->depth,
                           &cont->c);
    */
    // However, we still need to check whether we should yield, since we have
    // forgotten that g is foo partially-applied twice.
    if (ssm_has_children(act)) {
      act->pc = 8;
      return;
    case 8:;
    }

    ssm_value_t h_arg =
        ssm_marshal(ssm_unmarshal(cont->a) + ssm_unmarshal(cont->b) +
                    ssm_unmarshal(cont->c));
    // This is the last time we use h, and we know that it must reduce. So, we
    // can inline ssm_closure_apply_final() and omit checking arg_cap/arg_count.
    ssm_closure_push(cont->h, h_arg);
    ssm_closure_activate(cont->h, act, act->priority, act->depth, cont->__ret);
    ssm_closure_free(cont->h);
    /*
    ssm_closure_apply_auto(cont->h, h_arg, act, act->priority, act->depth,
                           cont->__ret);
    */
    // Since we know h must reduce, we can also yield unconditionally:
    act->pc = 9;
    return;
  case 9:;
    /*
    if (ssm_has_children(act)) {
      act->pc = 9;
      return;
    case 9:;
    }
    */

    // Value types, no need to drop
    /*
    ssm_drop(cont->c);
    ssm_drop(cont->b);
    ssm_drop(cont->b);
    ssm_drop(cont->a);
    */

    // Unused
    /*
    ssm_drop(cont->a1);
    ssm_drop(cont->a2);
    */

    // Moved via ssm_closure_apply_final()
    /*
    ssm_drop(cont->h);
    ssm_drop(cont->g);
    ssm_drop(cont->f);
    */

    // Unused
    /*
    ssm_drop(cont->foo);
    */
    break;
  }
  ssm_leave(&cont->act, sizeof(*cont));
}

#endif

ssm_value_t ret = {.packed_val = 6969};
void ssm_program_init(void) {
  ssm_activate(ssm_enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY,
                              SSM_ROOT_DEPTH, NULL, &ret));
}

void ssm_program_exit(void) {
  printf("Return value: %d\n", ssm_unmarshal(ret));
  ssm_drop(ret);
}
