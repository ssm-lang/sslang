#include "ssm.h"
typedef char unit;

typedef struct {
            ssm_act_t act;
            ssm_value_t cint;
            ssm_value_t cout;
            ssm_value_t *__return_val;
            ssm_value_t __tmp_0;
            ssm_value_t __tmp_1;
            ssm_value_t __tmp_10;
            ssm_value_t __tmp_11;
            ssm_value_t __tmp_12;
            ssm_value_t __tmp_13;
            ssm_value_t __tmp_2;
            ssm_value_t __tmp_3;
            ssm_value_t __tmp_4;
            ssm_value_t __tmp_5;
            ssm_value_t __tmp_6;
            ssm_value_t __tmp_7;
            ssm_value_t __tmp_8;
            ssm_value_t __tmp_9;
            ssm_value_t anon0_underscore;
            ssm_value_t anon10_underscore;
            ssm_value_t anon11_underscore;
            ssm_value_t anon12_underscore;
            ssm_value_t anon1_underscore;
            ssm_value_t anon2_underscore;
            ssm_value_t anon3_underscore;
            ssm_value_t anon4_underscore;
            ssm_value_t anon5_underscore;
            ssm_value_t anon6_underscore;
            ssm_value_t anon7_underscore;
            ssm_value_t anon8_underscore;
            ssm_value_t anon9_underscore;
            ssm_trigger_t __trig_1;
        } act_main_t;
ssm_act_t *__enter_main(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                        ssm_value_t *__return_val);
extern struct ssm_closure1 __closure_main;
void __step_main(ssm_act_t *actg);
ssm_act_t *__enter_main(ssm_act_t *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_value_t *__argv,
                        ssm_value_t *__return_val)
{
    ssm_act_t *actg = ssm_enter(sizeof(act_main_t), __step_main, caller, priority, depth);
    act_main_t *acts = container_of(actg, act_main_t, act);
    
    acts->cint = __argv[0];
    acts->cout = __argv[1];
    acts->__return_val = __return_val;
    acts->__trig_1.act = actg;
    return actg;
}
struct ssm_closure1 __closure_main = {.mm ={.ref_count =1, .kind =SSM_CLOSURE_K, .info ={.vector ={.count =0, .cap =
                                                                                                   2}}}, .f =
                                      __enter_main, .argv ={{0}}};
void __step_main(ssm_act_t *actg)
{
    act_main_t *acts = container_of(actg, act_main_t, act);
    
    switch (actg->pc) {
        
      case 0:
        ;
        acts->__tmp_0 = ssm_marshal((uint32_t) 10);
        ssm_dup(acts->cout);
        acts->__tmp_1 = ssm_marshal((uint32_t) 72);
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_0), acts->__tmp_1);
        ssm_drop(acts->__tmp_0);
        ssm_drop(acts->__tmp_1);
        ssm_drop(acts->cout);
        acts->anon0_underscore = ssm_marshal((uint32_t) 0);
        ssm_dup(acts->cout);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 1;
        return;
        
      case 1:
        ;
        ssm_desensitize(&acts->__trig_1);
        ssm_drop(acts->cout);
        acts->anon1_underscore = ssm_marshal((uint32_t) 0);
        acts->__tmp_2 = ssm_marshal((uint32_t) 10);
        ssm_dup(acts->cout);
        acts->__tmp_3 = ssm_marshal((uint32_t) 101);
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_2), acts->__tmp_3);
        ssm_drop(acts->__tmp_2);
        ssm_drop(acts->__tmp_3);
        ssm_drop(acts->cout);
        acts->anon2_underscore = ssm_marshal((uint32_t) 0);
        ssm_dup(acts->cout);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 2;
        return;
        
      case 2:
        ;
        ssm_desensitize(&acts->__trig_1);
        ssm_drop(acts->cout);
        acts->anon3_underscore = ssm_marshal((uint32_t) 0);
        acts->__tmp_4 = ssm_marshal((uint32_t) 10);
        ssm_dup(acts->cout);
        acts->__tmp_5 = ssm_marshal((uint32_t) 108);
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_4), acts->__tmp_5);
        ssm_drop(acts->__tmp_4);
        ssm_drop(acts->__tmp_5);
        ssm_drop(acts->cout);
        acts->anon4_underscore = ssm_marshal((uint32_t) 0);
        ssm_dup(acts->cout);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 3;
        return;
        
      case 3:
        ;
        ssm_desensitize(&acts->__trig_1);
        ssm_drop(acts->cout);
        acts->anon5_underscore = ssm_marshal((uint32_t) 0);
        acts->__tmp_6 = ssm_marshal((uint32_t) 10);
        ssm_dup(acts->cout);
        acts->__tmp_7 = ssm_marshal((uint32_t) 108);
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_6), acts->__tmp_7);
        ssm_drop(acts->__tmp_6);
        ssm_drop(acts->__tmp_7);
        ssm_drop(acts->cout);
        acts->anon6_underscore = ssm_marshal((uint32_t) 0);
        ssm_dup(acts->cout);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 4;
        return;
        
      case 4:
        ;
        ssm_desensitize(&acts->__trig_1);
        ssm_drop(acts->cout);
        acts->anon7_underscore = ssm_marshal((uint32_t) 0);
        acts->__tmp_8 = ssm_marshal((uint32_t) 10);
        ssm_dup(acts->cout);
        acts->__tmp_9 = ssm_marshal((uint32_t) 111);
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_8), acts->__tmp_9);
        ssm_drop(acts->__tmp_8);
        ssm_drop(acts->__tmp_9);
        ssm_drop(acts->cout);
        acts->anon8_underscore = ssm_marshal((uint32_t) 0);
        ssm_dup(acts->cout);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 5;
        return;
        
      case 5:
        ;
        ssm_desensitize(&acts->__trig_1);
        ssm_drop(acts->cout);
        acts->anon9_underscore = ssm_marshal((uint32_t) 0);
        acts->__tmp_10 = ssm_marshal((uint32_t) 10);
        ssm_dup(acts->cout);
        acts->__tmp_11 = ssm_marshal((uint32_t) 10);
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_10), acts->__tmp_11);
        ssm_drop(acts->__tmp_10);
        ssm_drop(acts->__tmp_11);
        ssm_drop(acts->cout);
        acts->anon10_underscore = ssm_marshal((uint32_t) 0);
        ssm_dup(acts->cout);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 6;
        return;
        
      case 6:
        ;
        ssm_desensitize(&acts->__trig_1);
        ssm_drop(acts->cout);
        acts->anon11_underscore = ssm_marshal((uint32_t) 0);
        acts->__tmp_12 = ssm_marshal((uint32_t) 10);
        ssm_dup(acts->cout);
        acts->__tmp_13 = ssm_marshal((uint32_t) 0);
        ssm_later(acts->cout, ssm_now() + (uint32_t) ssm_unmarshal(acts->__tmp_12), acts->__tmp_13);
        ssm_drop(acts->__tmp_12);
        ssm_drop(acts->__tmp_13);
        ssm_drop(acts->cout);
        acts->anon12_underscore = ssm_marshal((uint32_t) 0);
        ssm_dup(acts->cout);
        ssm_sensitize(acts->cout, &acts->__trig_1);
        actg->pc = 7;
        return;
        
      case 7:
        ;
        ssm_desensitize(&acts->__trig_1);
        ssm_drop(acts->cout);
        ssm_drop(acts->anon12_underscore);
        ssm_drop(acts->anon11_underscore);
        ssm_drop(acts->anon10_underscore);
        ssm_drop(acts->anon9_underscore);
        ssm_drop(acts->anon8_underscore);
        ssm_drop(acts->anon7_underscore);
        ssm_drop(acts->anon6_underscore);
        ssm_drop(acts->anon5_underscore);
        ssm_drop(acts->anon4_underscore);
        ssm_drop(acts->anon3_underscore);
        ssm_drop(acts->anon2_underscore);
        ssm_drop(acts->anon1_underscore);
        ssm_drop(acts->anon0_underscore);
        ssm_drop(acts->cout);
        ssm_drop(acts->cint);
        
      default:
        break;
    }
    if (acts->__return_val)
        *acts->__return_val = ssm_marshal((uint32_t) 0);
    
  __leave_step:
    ssm_leave(actg, sizeof(act_main_t));
}

