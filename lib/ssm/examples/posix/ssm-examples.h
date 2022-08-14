/** @file ssm-examples.h
 *  @brief SSM example boilerplate.
 *
 *  This file defines the boilerplate shared across all examples, including
 *  a definition of ssm_throw() and a main() function that initializes the
 *  memory allocator using ssm_mem_init().
 *
 *  All types in SSM are now represented by #ssm_value_t, so this header file
 *  also defines type names that are more self-documenting.
 */
#ifndef _SSM_EXAMPLES_H
#define _SSM_EXAMPLES_H

#include <ssm-internal.h>
#include <ssm.h>

#include <stdio.h>
#include <string.h>

typedef ssm_value_t i8;
typedef ssm_value_t i16;
typedef ssm_value_t i32;
typedef ssm_value_t i64;
typedef ssm_value_t u8;
typedef ssm_value_t u16;
typedef ssm_value_t u32;
typedef ssm_value_t u64;

typedef ssm_value_t ssm_event_t;
typedef ssm_value_t ssm_bool_t;
typedef ssm_value_t ssm_i8_t;
typedef ssm_value_t ssm_i16_t;
typedef ssm_value_t ssm_i32_t;
typedef ssm_value_t ssm_i64_t;
typedef ssm_value_t ssm_u8_t;
typedef ssm_value_t ssm_u16_t;
typedef ssm_value_t ssm_u32_t;
typedef ssm_value_t ssm_u64_t;

#define EVENT_VALUE (ssm_marshal(0))
#define NANOS 1000000000L

void ssm_program_init(void);
void ssm_program_exit(void);
extern char **ssm_init_args;

ssm_act_t *__enter_stdout_handler(ssm_act_t *parent, ssm_priority_t priority,
                                  ssm_depth_t depth, ssm_value_t *argv,
                                  ssm_value_t *ret);

void __spawn_stdin_handler(ssm_sv_t *ssm_stdin);
void __kill_stdin_handler(void);

#endif /* _SSM_EXAMPLES_H */
