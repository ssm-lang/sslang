#include "ssm-examples.h"
#include "ssm.h"
#include <stdio.h>

/* (print statements added in C implementation)

type List = Cons Int List | Nil

list = [0, 1, 2]

inc_offset (offset: Int) (item: Int) -> Int =
  item + offset

print_list (l: List) =
  match l
    Cons i l' = {{ print $ show i ++ "::" }}
                print_list l'
    Nil = {{ print "[]\r\n" }}

map (l: List) (inc: Int -> Int) -> List =
  match l
    Nil = Nil
    Cons i l' = Cons (inc i) (map l')

main =
  print_list list
  let inc_func = inc_offset 1
      list' = map list inc_func
  print_list list'

should print:

  0::1::2::[]
  1::2::3::[]
 */

enum List { Nil = 0, Cons, List_variants };
uint8_t List_size[List_variants] = {
    [Nil] = 0,
    [Cons] = 2,
};

ssm_value_t list;

typedef struct {
  ssm_act_t act;
  ssm_value_t __tmp1;
  ssm_value_t offset;
  ssm_value_t item;
  ssm_value_t *__ret;
} act_inc_offset_t;

ssm_stepf_t step_inc_offset;

ssm_act_t *enter_inc_offset(ssm_act_t *parent, ssm_priority_t priority,
                            ssm_depth_t depth, ssm_value_t *argv,
                            ssm_value_t *__ret) {
  act_inc_offset_t *cont =
      container_of(ssm_enter(sizeof(act_inc_offset_t), step_inc_offset, parent,
                             priority, depth),
                   act_inc_offset_t, act);
  cont->offset = argv[0];
  cont->item = argv[1];
  cont->__ret = __ret;
  return &cont->act;
}

void step_inc_offset(ssm_act_t *act) {
  act_inc_offset_t *cont = container_of(act, act_inc_offset_t, act);

  *cont->__ret =
      ssm_marshal(ssm_unmarshal(cont->item) + ssm_unmarshal(cont->offset));
  ssm_leave(&cont->act, sizeof(act_inc_offset_t));
}

typedef struct {
  ssm_act_t act;
  ssm_value_t l;
  ssm_value_t inc;
  ssm_value_t i_;
  ssm_value_t l_;
  ssm_value_t __tmp0;
  ssm_value_t __tmp1;
  ssm_value_t *__ret;
} act_map_t;

ssm_stepf_t step_map;

ssm_act_t *enter_map(ssm_act_t *parent, ssm_priority_t priority,
                     ssm_depth_t depth, ssm_value_t *argv, ssm_value_t *__ret) {
  act_map_t *cont = container_of(
      ssm_enter(sizeof(act_map_t), step_map, parent, priority, depth),
      act_map_t, act);
  cont->l = argv[0];
  cont->inc = argv[1];
  cont->__ret = __ret;
  return &cont->act;
}

void step_map(ssm_act_t *act) {
  act_map_t *cont = container_of(act, act_map_t, act);

  switch (act->pc) {
  case 0:
    switch (ssm_tag(cont->l)) {
    case Nil:
      goto match_Nil_0;
    case Cons:
      goto match_Cons_0;
    }
    SSM_ASSERT(0);
  match_Nil_0:
    *cont->__ret = ssm_marshal(Nil);
    break;
  match_Cons_0:;
    cont->i_ = ssm_adt_field(cont->l, 0);
    cont->l_ = ssm_adt_field(cont->l, 1);
    ssm_dup(cont->i_);
    ssm_dup(cont->l_);

    ssm_closure_apply_auto(cont->inc, cont->i_, act, act->priority, act->depth,
                           &cont->__tmp0);
    if (ssm_has_children(act)) {
      act->pc = 1;
      return;
    case 1:;
    }

    ssm_dup(cont->l_);
    ssm_dup(cont->inc);
    ssm_value_t argv[2] = {cont->l_, cont->inc};
    ssm_activate(
        enter_map(act, act->priority, act->depth, argv, &cont->__tmp1));
    act->pc = 2;
    return;
  case 2:;
    ssm_drop(cont->i_);
    ssm_drop(cont->l_);
    *cont->__ret = ssm_new_adt(List_size[Cons], Cons);
    ssm_adt_field(*cont->__ret, 0) = cont->__tmp0;
    ssm_adt_field(*cont->__ret, 1) = cont->__tmp1;
    break;
  }
  ssm_drop(cont->inc);
  ssm_drop(cont->l);
  ssm_leave(&cont->act, sizeof(act_map_t));
}

typedef struct {
  ssm_act_t act;
  ssm_value_t l;
  ssm_value_t __tmp0;
  ssm_value_t __tmp1;
  ssm_value_t *__ret;
} act_print_list_t;

ssm_stepf_t step_print_list;

ssm_act_t *enter_print_list(ssm_act_t *parent, ssm_priority_t priority,
                            ssm_depth_t depth, ssm_value_t *argv,
                            ssm_value_t *ret) {
  act_print_list_t *cont =
      container_of(ssm_enter(sizeof(act_print_list_t), step_print_list, parent,
                             priority, depth),
                   act_print_list_t, act);
  cont->l = argv[0];
  cont->__ret = ret;
  return &cont->act;
}

void step_print_list(ssm_act_t *act) {
  act_print_list_t *cont = container_of(act, act_print_list_t, act);
  switch (act->pc) {
  case 0:
    switch (ssm_tag(cont->l)) {
    case Nil:
      goto match_Nil_0;
    case Cons:
      goto match_Cons_0;
    }
    SSM_ASSERT(0);
  match_Nil_0:
    printf("[]\r\n");
    break;
  match_Cons_0:;
    ssm_value_t __i = ssm_adt_field(cont->l, 0);
    ssm_value_t __l = ssm_adt_field(cont->l, 1);
    ssm_dup(__l);
    ssm_drop(cont->l);

    printf("%d::", ssm_unmarshal(__i));

    // NOTE: unconditional activation; no need to allocate any closure.
    ssm_activate(enter_print_list(act, act->priority, act->depth, &__l, NULL));
    act->pc = 1;
    return;
  case 1:;
    // TODO: optimization available: tail-call recursion
    if (cont->__ret)
      *cont->__ret = ssm_marshal(0);
    break;
  }
  ssm_leave(&cont->act, sizeof(act_print_list_t));
}

typedef struct {
  ssm_act_t act;
  ssm_value_t list;
  ssm_value_t list_;
  ssm_value_t inc_func;
  ssm_value_t *__ret;
} act_main_t;

ssm_stepf_t step_main;

// Create a new activation record for main
ssm_act_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth, ssm_value_t *argv,
                          ssm_value_t *__ret) {
  act_main_t *cont = container_of(
      ssm_enter(sizeof(act_main_t), step_main, parent, priority, depth),
      act_main_t, act);

  cont->list = list;
  cont->__ret = __ret;
  return &cont->act;
}

void step_main(struct ssm_act *act) {
  act_main_t *cont = container_of(act, act_main_t, act);

  // Unlike map, this function is aggressively optimized, with justifications
  // commented inline.
  switch (act->pc) {
  case 0:;
    // NOTE: we can optimize out any closure allocation or application here
    // since we are fully applying a global function, eliminating the need for
    // producing an intermediate closure, or checking whether we need to yield.
    //
    // We need to call ssm_dup() on list because we still reference list later.
    ssm_dup(cont->list);
    ssm_activate(
        enter_print_list(act, act->priority, act->depth, &cont->list, NULL));
    act->pc = 1;
    return;
  case 1:;
    ssm_value_t inc = ssm_new_closure(&enter_inc_offset, 2);

    // NOTE: we can optimize out ssm_closure_apply() here since we know
    // (1) we uniquely own inc, (2) we never use (unapplied) inc again, and
    // (3) pushing 69 will not lead to reduction.
    ssm_closure_push(inc, ssm_marshal(69));

    // NOTE: we can optimize out any closure allocation or application here
    // since we are fully applying a global function, eliminating the need for
    // producing an intermediate closure, or checking whether we need to yield.
    //
    // We also don't need to call ssm_dup() on list or inc because we are moving
    // both into the child process executing map (via argv).
    ssm_value_t argv[2] = {cont->list, inc};
    ssm_activate(enter_map(act, act->priority, act->depth, argv, &cont->list_));
    act->pc = 2;
    return;
  case 2:
    // NOTE: No need to ssm_dup() list_ because it is moved into print_list.
    ssm_activate(
        enter_print_list(act, act->priority, act->depth, &cont->list_, NULL));
    act->pc = 3;
    return;
  case 3:
    if (cont->__ret)
      *cont->__ret = ssm_marshal(0);
    break;
  }
  ssm_leave(&cont->act, sizeof(*cont));
}

void ssm_program_init(void) {
  ssm_value_t v;
  v = ssm_marshal(Nil);
  list = v;
  int i = ssm_init_args && ssm_init_args[0] ? atoi(ssm_init_args[0]) : 3;
  for (; i >= 1; i--) {
    v = ssm_new_adt(List_size[Cons], Cons);
    ssm_adt_field(v, 0) = ssm_marshal(i);
    ssm_adt_field(v, 1) = list;
    list = v;
  }
  ssm_dup(list); // main captures reference to global by closure
  ssm_activate(ssm_enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY,
                              SSM_ROOT_DEPTH, NULL, NULL));
}

void ssm_program_exit(void) { ssm_drop(list); }
