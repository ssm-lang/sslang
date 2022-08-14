#include "ssm.h"
typedef char unit;

typedef struct {
            ssm_act_t act;
            ssm_value_t t;
            ssm_value_t *__return_val;
        } act_ms_t;
ssm_act_t *__enter_ms(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                      ssm_value_t *__return_val);
extern struct ssm_closure1 __closure_ms;
void __step_ms(ssm_act_t *actg);
typedef struct {
            ssm_act_t act;
            ssm_value_t n;
            ssm_value_t led;
            ssm_value_t *__return_val;
            ssm_value_t __tmp_0;
            ssm_value_t __tmp_1;
            ssm_value_t __tmp_2;
            ssm_value_t __tmp_3;
            ssm_value_t __tmp_7;
            ssm_value_t __tmp_8;
            ssm_value_t __tmp_9;
            ssm_value_t n_;
            ssm_trigger_t __trig_1;
        } act_blink_t;
ssm_act_t *__enter_blink(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                         ssm_value_t *__return_val);
extern struct ssm_closure1 __closure_blink;
void __step_blink(ssm_act_t *actg);
typedef struct {
            ssm_act_t act;
            ssm_value_t cout;
            ssm_value_t c;
            ssm_value_t *__return_val;
            ssm_trigger_t __trig_1;
        } act_main_putc_anon0_t;
ssm_act_t *__enter_main_putc_anon0(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                                   ssm_value_t *__return_val);
extern struct ssm_closure1 __closure_main_putc_anon0;
void __step_main_putc_anon0(ssm_act_t *actg);
typedef struct {
            ssm_act_t act;
            ssm_value_t putc;
            ssm_value_t led;
            ssm_value_t *__return_val;
            ssm_value_t __tmp_0;
            ssm_value_t __tmp_1;
            ssm_value_t __tmp_2;
            ssm_trigger_t __trig_1;
        } act_main_display_anon1_t;
ssm_act_t *__enter_main_display_anon1(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth,
                                      ssm_value_t *__argv, ssm_value_t *__return_val);
extern struct ssm_closure1 __closure_main_display_anon1;
void __step_main_display_anon1(ssm_act_t *actg);
typedef struct {
            ssm_act_t act;
            ssm_value_t cin;
            ssm_value_t cout;
            ssm_value_t *__return_val;
            ssm_value_t __tmp_0;
            ssm_value_t __tmp_1;
            ssm_value_t __tmp_2;
            ssm_value_t __tmp_3;
            ssm_value_t __tmp_4;
            ssm_value_t __tmp_5;
            ssm_value_t display;
            ssm_value_t led;
            ssm_value_t putc;
        } act_main_t;
ssm_act_t *__enter_main(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                        ssm_value_t *__return_val);
extern struct ssm_closure1 __closure_main;
void __step_main(ssm_act_t *actg);
ssm_act_t *__enter_ms(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                      ssm_value_t *__return_val)
{
    ssm_act_t *actg = ssm_enter(sizeof(act_ms_t), __step_ms, caller, priority, depth);
    act_ms_t *acts = container_of(actg, act_ms_t, act);
    
    acts->t = __argv[0];
    acts->__return_val = __return_val;
    return actg;
}
struct ssm_closure1 __closure_ms = {.mm ={.ref_count =1, .kind =3, .val_count =0, .tag =1}, .f =__enter_ms, .argv =
                                    {{0}}};
void __step_ms(ssm_act_t *actg)
{
    act_ms_t *acts = container_of(actg, act_ms_t, act);
    
    switch (actg->pc) {
        
      case 0:
        ;
        
      default:
        break;
    }
    if (acts->__return_val)
        *acts->__return_val = ssm_marshal((uint32_t) ((uint32_t) ssm_unmarshal(ssm_marshal((uint32_t) 1000000)) *
                                                      (uint32_t) ssm_unmarshal(acts->t)));
    
  __leave_step:
    ssm_leave(actg, sizeof(act_ms_t));
}
ssm_act_t *__enter_blink(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                         ssm_value_t *__return_val)
{
    ssm_act_t *actg = ssm_enter(sizeof(act_blink_t), __step_blink, caller, priority, depth);
    act_blink_t *acts = container_of(actg, act_blink_t, act);
    
    acts->n = __argv[0];
    acts->led = __argv[1];
    acts->__return_val = __return_val;
    acts->__trig_1.act = actg;
    return actg;
}
struct ssm_closure1 __closure_blink = {.mm ={.ref_count =1, .kind =3, .val_count =0, .tag =2}, .f =__enter_blink,
                                       .argv ={{0}}};
void __step_blink(ssm_act_t *actg)
{
    act_blink_t *acts = container_of(actg, act_blink_t, act);
    
    switch (actg->pc) {
        
      case 0:
        ;
        acts->__tmp_0 = ssm_new_sv(acts->n);
        acts->n_ = acts->__tmp_0;
        for (; ;) {
            acts->__tmp_1 = ssm_deref(acts->n_);
            acts->__tmp_2 = ssm_marshal((uint32_t) ((int32_t) ((uint32_t) ssm_unmarshal(acts->__tmp_1) << 1) >
                                                    (int32_t) ((uint32_t) ssm_unmarshal(ssm_marshal((uint32_t) 0)) <<
                                                               1)));
            switch (ssm_tag(acts->__tmp_2)) {
                
              case 0:
                ;
                goto __label_5;
                
              default:
                ;
                goto __label_6;
            }
            
          __label_5:
            ;
            break;
            acts->__tmp_3 = ssm_marshal((uint32_t) 0xdeadbeef);
            goto __label_4;
            
          __label_6:
            ;
            acts->__tmp_3 = ssm_marshal((uint32_t) 1);
            goto __label_4;
            
          __label_4:
            ;
            ssm_closure_apply((ssm_value_t) {.heap_ptr = &__closure_ms.mm}, ssm_marshal((uint32_t) 50), actg,
                              actg->priority, actg->depth, &acts->__tmp_7);
            if (ssm_has_children(actg)) {
                actg->pc = 1;
                return;
                
              case 1:
                ;
                ;
            }
            ssm_later(acts->led, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_7), ssm_marshal((uint32_t) 1));
            ssm_sensitize(acts->led, &acts->__trig_1);
            actg->pc = 2;
            return;
            
          case 2:
            ;
            ssm_desensitize(&acts->__trig_1);
            ssm_closure_apply((ssm_value_t) {.heap_ptr = &__closure_ms.mm}, ssm_marshal((uint32_t) 50), actg,
                              actg->priority, actg->depth, &acts->__tmp_8);
            if (ssm_has_children(actg)) {
                actg->pc = 3;
                return;
                
              case 3:
                ;
                ;
            }
            ssm_later(acts->led, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_8), ssm_marshal((uint32_t) 0));
            ssm_sensitize(acts->led, &acts->__trig_1);
            actg->pc = 4;
            return;
            
          case 4:
            ;
            ssm_desensitize(&acts->__trig_1);
            acts->__tmp_9 = ssm_deref(acts->n_);
            ssm_assign(acts->n_, actg->priority, ssm_marshal((uint32_t) ((uint32_t) ssm_unmarshal(acts->__tmp_9) -
                                                                         (uint32_t) ssm_unmarshal(ssm_marshal((uint32_t) 1)))));
        }
        
      default:
        break;
    }
    if (acts->__return_val)
        *acts->__return_val = ssm_marshal((uint32_t) 0);
    
  __leave_step:
    ssm_leave(actg, sizeof(act_blink_t));
}
ssm_act_t *__enter_main_putc_anon0(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                                   ssm_value_t *__return_val)
{
    ssm_act_t *actg = ssm_enter(sizeof(act_main_putc_anon0_t), __step_main_putc_anon0, caller, priority, depth);
    act_main_putc_anon0_t *acts = container_of(actg, act_main_putc_anon0_t, act);
    
    acts->cout = __argv[0];
    acts->c = __argv[1];
    acts->__return_val = __return_val;
    acts->__trig_1.act = actg;
    return actg;
}
struct ssm_closure1 __closure_main_putc_anon0 = {.mm ={.ref_count =1, .kind =3, .val_count =0, .tag =2}, .f =
                                                 __enter_main_putc_anon0, .argv ={{0}}};
void __step_main_putc_anon0(ssm_act_t *actg)
{
    act_main_putc_anon0_t *acts = container_of(actg, act_main_putc_anon0_t, act);
    
    switch (actg->pc) {
        
      case 0:
        ;
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(ssm_marshal((uint32_t) 1)), acts->c);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 1;
        return;
        
      case 1:
        ;
        ssm_desensitize(&acts->__trig_1);
        
      default:
        break;
    }
    if (acts->__return_val)
        *acts->__return_val = ssm_marshal((uint32_t) 0);
    
  __leave_step:
    ssm_leave(actg, sizeof(act_main_putc_anon0_t));
}
ssm_act_t *__enter_main_display_anon1(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth,
                                      ssm_value_t *__argv, ssm_value_t *__return_val)
{
    ssm_act_t *actg = ssm_enter(sizeof(act_main_display_anon1_t), __step_main_display_anon1, caller, priority, depth);
    act_main_display_anon1_t *acts = container_of(actg, act_main_display_anon1_t, act);
    
    acts->putc = __argv[0];
    acts->led = __argv[1];
    acts->__return_val = __return_val;
    acts->__trig_1.act = actg;
    return actg;
}
struct ssm_closure1 __closure_main_display_anon1 = {.mm ={.ref_count =1, .kind =3, .val_count =0, .tag =2}, .f =
                                                    __enter_main_display_anon1, .argv ={{0}}};
void __step_main_display_anon1(ssm_act_t *actg)
{
    act_main_display_anon1_t *acts = container_of(actg, act_main_display_anon1_t, act);
    
    switch (actg->pc) {
        
      case 0:
        ;
        for (; ;) {
            ssm_sensitize(acts->led, &acts->__trig_1);
            actg->pc = 1;
            return;
            
          case 1:
            ;
            ssm_desensitize(&acts->__trig_1);
            acts->__tmp_0 = ssm_deref(acts->led);
            ssm_closure_apply(acts->putc, ssm_marshal((uint32_t) ((uint32_t) ssm_unmarshal(acts->__tmp_0) +
                                                                  (uint32_t) ssm_unmarshal(ssm_marshal((uint32_t) 48)))),
                              actg, actg->priority, actg->depth, &acts->__tmp_1);
            if (ssm_has_children(actg)) {
                actg->pc = 2;
                return;
                
              case 2:
                ;
                ;
            }
            ssm_closure_apply(acts->putc, ssm_marshal((uint32_t) 10), actg, actg->priority, actg->depth,
                              &acts->__tmp_2);
            if (ssm_has_children(actg)) {
                actg->pc = 3;
                return;
                
              case 3:
                ;
                ;
            }
        }
        
      default:
        break;
    }
    if (acts->__return_val)
        *acts->__return_val = ssm_marshal((uint32_t) 0);
    
  __leave_step:
    ssm_leave(actg, sizeof(act_main_display_anon1_t));
}
ssm_act_t *__enter_main(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                        ssm_value_t *__return_val)
{
    ssm_act_t *actg = ssm_enter(sizeof(act_main_t), __step_main, caller, priority, depth);
    act_main_t *acts = container_of(actg, act_main_t, act);
    
    acts->cin = __argv[0];
    acts->cout = __argv[1];
    acts->__return_val = __return_val;
    return actg;
}
struct ssm_closure1 __closure_main = {.mm ={.ref_count =1, .kind =3, .val_count =0, .tag =2}, .f =__enter_main, .argv =
                                      {{0}}};
void __step_main(ssm_act_t *actg)
{
    act_main_t *acts = container_of(actg, act_main_t, act);
    
    switch (actg->pc) {
        
      case 0:
        ;
        ssm_closure_apply((ssm_value_t) {.heap_ptr = &__closure_main_putc_anon0.mm}, acts->cout, actg, actg->priority,
                          actg->depth, &acts->__tmp_0);
        if (ssm_has_children(actg)) {
            actg->pc = 1;
            return;
            
          case 1:
            ;
            ;
        }
        acts->putc = acts->__tmp_0;
        ssm_closure_apply((ssm_value_t) {.heap_ptr = &__closure_main_display_anon1.mm}, acts->putc, actg,
                          actg->priority, actg->depth, &acts->__tmp_1);
        if (ssm_has_children(actg)) {
            actg->pc = 2;
            return;
            
          case 2:
            ;
            ;
        }
        acts->display = acts->__tmp_1;
        acts->__tmp_2 = ssm_new_sv(ssm_marshal((uint32_t) 0));
        acts->led = acts->__tmp_2;
        if (actg->depth < 1)
            SSM_THROW(SSM_EXHAUSTED_PRIORITY);
        ssm_closure_apply((ssm_value_t) {.heap_ptr = &__closure_blink.mm}, ssm_marshal((uint32_t) 10), actg,
                          actg->priority, actg->depth, &acts->__tmp_3);
        if (ssm_has_children(actg)) {
            actg->pc = 3;
            return;
            
          case 3:
            ;
            ;
        }
        ssm_closure_apply(acts->__tmp_3, acts->led, actg, actg->priority + 0 * (1 << actg->depth - 1), actg->depth - 1,
                          &acts->__tmp_4);
        ssm_closure_apply(acts->display, acts->led, actg, actg->priority + 1 * (1 << actg->depth - 1), actg->depth - 1,
                          &acts->__tmp_5);
        actg->pc = 4;
        return;
        
      case 4:
        ;
        
      default:
        break;
    }
    if (acts->__return_val)
        *acts->__return_val = ssm_marshal((uint32_t) 0);
    
  __leave_step:
    ssm_leave(actg, sizeof(act_main_t));
}

