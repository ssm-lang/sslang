s/ACTIVATION_RECORD_FIELDS/SSM_ACT_FIELDS/
s/cv_int_t/ssm_i32_t/g
s/stepf_t/ssm_stepf_t/g
s/rar_t/struct ssm_act/g
s/trigger_t/struct ssm_trigger/g
s/unsensitize/ssm_desensitize/g
s/sensitize/ssm_sensitize/g
s/priority_t/ssm_priority_t/g
s/PRIORITY_AT_ROOT/SSM_ROOT_PRIORITY/g
s/depth_t/ssm_depth_t/g
s/DEPTH_AT_ROOT/SSM_ROOT_DEPTH/g
s/assign_int/ssm_assign_i32/g
s/later_int/ssm_later_i32/g
s/initialize_int/ssm_initialize_i32/g
s/enter/ssm_enter/g
s/leave/ssm_leave/g
s/fork/ssm_activate/g
s/tick/ssm_tick/g
s/now/ssm_now()/g
