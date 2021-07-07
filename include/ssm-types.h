#ifndef _SSM_TYPES_H
#define _SSM_TYPES_H

/**
 * Shared between type stubs and activation records; the scheduler should be
 * agonistic to anything declared here.
 */

#include "ssm-core.h"
#include "ssm-sv.h"

#ifdef bool
/* This shouldn't be a macro, should be a typedef */
#undef bool
typedef _Bool bool;
#endif

/*** Unit type {{{
 *
 * Unit types are just "pure" events without payloads, and are all that the
 * scheduler is aware of. For the sake of consistency, we "declare" them as
 * a distinct type, except their implementation will entirely alias that of the
 * sched-event API.
 */
typedef struct sv unit_svt;

/* Unit type }}} */

/*** Declaration helpers {{{ */

/**
 * Declare a scheduled variable type whose payload can be directly assigned by
 * value (in C).
 *
 * Note that since C supports pass-by-value for structs, we can use this for
 * any atomic (non-aggregate) data type.
 */
#define DECLARE_SCHED_VARIABLE_SCALAR(payload_t)                               \
  typedef struct {                                                             \
    struct sv sv;                                                              \
    payload_t value;       /* Current value */                                 \
    payload_t later_value; /* Buffered value */                                \
  } payload_t##_svt;                                                           \
  void assign_##payload_t(payload_t##_svt *sv, priority_t prio,                \
                          const payload_t value);                              \
  void later_##payload_t(payload_t##_svt *sv, ssm_time_t then,                 \
                         const payload_t value);                               \
  void initialize_##payload_t(payload_t##_svt *v)

/* Declaration helpers }}} */

/*** Scalar types {{{ */

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

DECLARE_SCHED_VARIABLE_SCALAR(bool);
DECLARE_SCHED_VARIABLE_SCALAR(i8);
DECLARE_SCHED_VARIABLE_SCALAR(i16);
DECLARE_SCHED_VARIABLE_SCALAR(i32);
DECLARE_SCHED_VARIABLE_SCALAR(i64);
DECLARE_SCHED_VARIABLE_SCALAR(u8);
DECLARE_SCHED_VARIABLE_SCALAR(u16);
DECLARE_SCHED_VARIABLE_SCALAR(u32);
DECLARE_SCHED_VARIABLE_SCALAR(u64);

/* Scalar types }}} */

#endif /* _SSM_TYPES_H */
