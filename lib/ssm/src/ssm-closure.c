/** @file ssm-closure.c
 *  @brief SSM closure management and allocation.
 *
 *  @author Hans Montero (hmontero1205)
 */
#include <assert.h>
#include <ssm-internal.h>
#include <ssm.h>
#include <stdio.h>
#include <stdlib.h>

ssm_value_t ssm_new_closure(ssm_func_t f, uint8_t arg_cap) {
  struct ssm_closure1 *closure = container_of(
      ssm_mem_alloc(ssm_closure_size(arg_cap)), struct ssm_closure1, mm);
  closure->mm.ref_count = 1;
  closure->mm.kind = SSM_CLOSURE_K;
  closure->mm.val_count = 0;
  closure->mm.tag = arg_cap;
  closure->f = f;
  return (ssm_value_t){.heap_ptr = &closure->mm};
}

ssm_value_t ssm_closure_clone(ssm_value_t closure) {
  ssm_value_t new_closure =
      ssm_new_closure(ssm_closure_func(closure), ssm_closure_arg_cap(closure));
  ssm_closure_arg_count(new_closure) = ssm_closure_arg_count(closure);

  for (size_t i = 0; i < ssm_closure_arg_count(closure); i++) {
    ssm_dup(ssm_closure_arg(closure, i));
    ssm_closure_arg(new_closure, i) = ssm_closure_arg(closure, i);
  }

  return new_closure;
}

void ssm_closure_apply(ssm_value_t closure, ssm_value_t arg, ssm_act_t *parent,
                       ssm_priority_t prio, ssm_depth_t depth,
                       ssm_value_t *ret) {
  if (ssm_closure_arg_cap(closure) == ssm_closure_arg_count(closure) + 1) {
    // Call ssm_dup() on all previously applied arguments of closure, since they
    // will be shared by the child process.
    ssm_dups(ssm_closure_arg_count(closure), ssm_closure_argv(closure));
    ssm_closure_push(closure, arg);
    ssm_closure_activate(closure, parent, prio, depth, ret);
    ssm_closure_pop(closure);
  } else {
    // No need to call ssm_dups() since ssm_closure_clone() already dups
    // all the arguments.
    *(ret) = ssm_closure_clone(closure);
    ssm_closure_push(*(ret), arg);
  }
}

void ssm_closure_apply_final(ssm_value_t closure, ssm_value_t arg,
                             ssm_act_t *parent, ssm_priority_t prio,
                             ssm_depth_t depth, ssm_value_t *ret) {
  SSM_ASSERT(!ssm_is_shared(closure));

  if (ssm_closure_arg_cap(closure) == ssm_closure_arg_count(closure) + 1) {
    ssm_closure_push(closure, arg);
    ssm_closure_activate(closure, parent, prio, depth, ret);

    // We can directly call ssm_mem_free() because we already know (1) closure
    // is a heap object and (2) this is the last time it will be used.
    //
    // We don't need to call ssm_drop() on any of its arguments since those are
    // moved into the newly activated process (also why we didn't need to call
    // ssm_dup() before activating the process).
    ssm_closure_free(closure);
  } else {
    // Just modify the closure in-place.
    ssm_closure_push(closure, arg);
    *(ret) = (closure);
  }
}
