/**
 * Implementations of type-specific scheduled variables, whose vtable methods
 * are aware of the size and layout of their respective payloads.
 */

#include <ssm-types.h>

#define xstr(s) #s
#define stringify(s) xstr(s)

#ifdef DEBUG
#define DEFINE_DEBUG_VALUE_REPR(payload_t, fmt)                                \
  static struct debug_buffer value_repr_##payload_t(struct sv *sv) {           \
    struct debug_buffer buf;                                                   \
    payload_t##_svt *v = container_of(sv, payload_t##_svt, sv);                \
    snprintf(buf.buf, sizeof(buf.buf), fmt, v->value);                         \
    return buf;                                                                \
  }
#else
#define DEFINE_DEBUG_VALUE_REPR(payload_t, fmt)
#endif

/**
 * Scalar type definition helper macro.
 */
#define DEFINE_SCHED_VARIABLE_SCALAR(payload_t)                                \
  static void update_##payload_t(struct sv *sv) {                              \
    payload_t##_svt *v = container_of(sv, payload_t##_svt, sv);                \
    v->value = v->later_value;                                                 \
  }                                                                            \
  void assign_##payload_t(payload_t##_svt *v, priority_t prio,                 \
                          const payload_t value) {                             \
    v->value = value;                                                          \
    assign_event(&v->sv, prio);                                                \
  }                                                                            \
  void later_##payload_t(payload_t##_svt *v, ssm_time_t then,                  \
                         const payload_t value) {                              \
    v->later_value = value;                                                    \
    later_event(&v->sv, then);                                                 \
  }                                                                            \
  void initialize_##payload_t(payload_t##_svt *v) {                            \
    initialize_event(&v->sv);                                                  \
    v->sv.update = update_##payload_t;                                         \
    DEBUG_SV_SET_VAR_NAME(v->sv.debug, "(unknown var name)");                  \
    DEBUG_SV_SET_TYPE_NAME(v->sv.debug, stringify(payload_t));                 \
    DEBUG_SV_SET_VALUE_REPR(v->sv.debug, value_repr_##payload_t);              \
  }

/**
 * Define implementation for scalar types.
 */
DEFINE_DEBUG_VALUE_REPR(bool, "%d")
DEFINE_DEBUG_VALUE_REPR(i8, "%d")
DEFINE_DEBUG_VALUE_REPR(i16, "%d")
DEFINE_DEBUG_VALUE_REPR(i32, "%d")
DEFINE_DEBUG_VALUE_REPR(i64, "%ld")
DEFINE_DEBUG_VALUE_REPR(u8, "%u")
DEFINE_DEBUG_VALUE_REPR(u16, "%u")
DEFINE_DEBUG_VALUE_REPR(u32, "%u")
DEFINE_DEBUG_VALUE_REPR(u64, "%lu")

DEFINE_SCHED_VARIABLE_SCALAR(bool)
DEFINE_SCHED_VARIABLE_SCALAR(i8)
DEFINE_SCHED_VARIABLE_SCALAR(i16)
DEFINE_SCHED_VARIABLE_SCALAR(i32)
DEFINE_SCHED_VARIABLE_SCALAR(i64)
DEFINE_SCHED_VARIABLE_SCALAR(u8)
DEFINE_SCHED_VARIABLE_SCALAR(u16)
DEFINE_SCHED_VARIABLE_SCALAR(u32)
DEFINE_SCHED_VARIABLE_SCALAR(u64)
