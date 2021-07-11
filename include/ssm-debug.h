#ifndef _SSM_DEBUG_H
#define _SSM_DEBUG_H

/**
 * Platform-agnostic debug interface for SSM.
 *
 * TODO: lots of tidying up to get rid of ugly CPP ifdefs etc.
 */

// FIXME!
#undef DEBUG

#ifdef DEBUG
#include <stdio.h> /* TODO: should only be included for debug */

/* FIXME: This is kind of an ugly hack. */
struct debug_buffer {
  char buf[32];
};

/** SV debug information. */
struct debug_sv {
  const char *var_name;
  const char *type_name;
  struct debug_buffer (*value_repr)(struct ssm_sv *);
};

/** Debug information for activation records. */
struct debug_act {
  const char *act_name;
};

#define DEBUG_ACT_SET_ACT_NAME(actd, name) ((actd).act_name = (name))
#define DEBUG_SV_SET_VAR_NAME(svd, name) ((svd).var_name = (name))
#define DEBUG_SV_SET_TYPE_NAME(svd, name) ((svd).type_name = (name))
#define DEBUG_SV_SET_VALUE_REPR(svd, vr) ((svd).value_repr = (vr))

#define DEBUG_ACT_GET_ACT_NAME(actd) (actd).act_name
#define DEBUG_SV_GET_VAR_NAME(svd) (svd).var_name
#define DEBUG_SV_GET_TYPE_NAME(svd) (svd).type_name
#define DEBUG_SV_GET_VALUE_REPR(svd, sv) (svd).value_repr(sv).buf

#else

#define DEBUG_ACT_SET_ACT_NAME(actd, name)                                     \
  do {                                                                         \
  } while (0)
#define DEBUG_SV_SET_VAR_NAME(svd, name)                                       \
  do {                                                                         \
  } while (0)
#define DEBUG_SV_SET_TYPE_NAME(svd, name)                                      \
  do {                                                                         \
  } while (0)
#define DEBUG_SV_SET_VALUE_REPR(svd, vr)                                       \
  do {                                                                         \
  } while (0)

#define DEBUG_ACT_GET_ACT_NAME(actd) "(no DEBUG; act name unavailable)"
#define DEBUG_SV_GET_VAR_NAME(svd) "(no DEBUG; var name unavailable)"
#define DEBUG_SV_GET_TYPE_NAME(svd) "(no DEBUG; type name unavailable)"
#define DEBUG_SV_GET_VALUE_REPR(svd, sv) "(no DEBUG; value unavailable)"

#endif /* ifdef DEBUG */

#ifndef DEBUG_TRACE
#define DEBUG_TRACE(...)                                                       \
  do {                                                                         \
  } while (0)
#endif

#ifndef DEBUG_PRINT
#define DEBUG_PRINT(...)                                                       \
  do {                                                                         \
  } while (0)
#endif

#ifndef DEBUG_ASSERT
#define DEBUG_ASSERT(assertion, ...)                                           \
  do {                                                                         \
  } while (0)
#endif

#endif /* ifndef _SSM_DEBUG_H */
