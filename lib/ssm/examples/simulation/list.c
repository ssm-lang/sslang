#include "ssm-examples.h"
#include <stdio.h>

/* (print statements added in C implementation)

type List = Cons Int List | Nil

list = [0, 1, 2]

print_list (l: List) =
  match l
    Cons i l' = {{ print $ show i ++ "::" }}
                print_list l'
    Nil = {{ print "[]\r\n" }}

map_inc (l: List) -> List =
  match l
    Nil = Nil
    Cons i l' = Cons (i + 1) (map_inc l')

main =
  print_list list
  let list' = map_inc (map_inc list)
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
  ssm_value_t l;
  ssm_value_t __tmp0;
  ssm_value_t __tmp1;
  ssm_value_t *__ret;
} act_map_inc_t;

ssm_stepf_t step_map_inc;

ssm_act_t *enter_map_inc(ssm_act_t *parent, ssm_priority_t priority,
                         ssm_depth_t depth, ssm_value_t l, ssm_value_t *__ret) {
  act_map_inc_t *cont = container_of(
      ssm_enter(sizeof(act_map_inc_t), step_map_inc, parent, priority, depth),
      act_map_inc_t, act);
  cont->l = l;
  cont->__ret = __ret;
  return &cont->act;
}

void step_map_inc(ssm_act_t *act) {
  act_map_inc_t *cont = container_of(act, act_map_inc_t, act);

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
    ssm_value_t __i = ssm_adt_field(cont->l, 0);
    ssm_value_t __l = ssm_adt_field(cont->l, 1);
    ssm_dup(__l);

    ssm_drop(cont->l);

    cont->__tmp0 = ssm_marshal(ssm_unmarshal(__i) + 1);

    ssm_activate(
        enter_map_inc(act, act->priority, act->depth, __l, &cont->__tmp1));
    act->pc = 1;
    return;
  case 1:;
    *cont->__ret = ssm_new_adt(List_size[Cons], Cons);
    ssm_adt_field(*cont->__ret, 0) = cont->__tmp0;
    ssm_adt_field(*cont->__ret, 1) = cont->__tmp1;
    break;
  }
  ssm_leave(&cont->act, sizeof(act_map_inc_t));
}

typedef struct {
  ssm_act_t act;
  ssm_value_t l;
  ssm_value_t __tmp0;
  ssm_value_t __tmp1;
} act_print_list_t;

ssm_stepf_t step_print_list;

ssm_act_t *enter_print_list(ssm_act_t *parent, ssm_priority_t priority,
                            ssm_depth_t depth, ssm_value_t l) {
  act_print_list_t *cont =
      container_of(ssm_enter(sizeof(act_print_list_t), step_print_list, parent,
                             priority, depth),
                   act_print_list_t, act);
  cont->l = l;
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

    ssm_activate(enter_print_list(act, act->priority, act->depth, __l));
    act->pc = 1;
    return;
  case 1:
    break;
  }
  ssm_leave(&cont->act, sizeof(act_print_list_t));
}

typedef struct {
  ssm_act_t act;
  ssm_value_t list;
  ssm_value_t list_;
} act_main_t;

ssm_stepf_t step_main;

// Create a new activation record for main
ssm_act_t *ssm_enter_main(struct ssm_act *parent, ssm_priority_t priority,
                          ssm_depth_t depth) {
  act_main_t *cont = container_of(
      ssm_enter(sizeof(act_main_t), step_main, parent, priority, depth),
      act_main_t, act);

  cont->list = list;
  return &cont->act;
}

void step_main(struct ssm_act *act) {
  act_main_t *cont = container_of(act, act_main_t, act);

  switch (act->pc) {
  case 0:
    ssm_dup(cont->list);
    ssm_activate(enter_print_list(act, act->priority, act->depth, cont->list));
    act->pc = 1;
    return;
  case 1:
    ssm_activate(enter_map_inc(act, act->priority, act->depth, cont->list,
                               &cont->list_));
    act->pc = 2;
    return;
  case 2:
    ssm_activate(enter_print_list(act, act->priority, act->depth, cont->list_));
    act->pc = 3;
    return;
  case 3:
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
  ssm_activate(
      ssm_enter_main(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));
}

void ssm_program_exit(void) { ssm_drop(list); }
