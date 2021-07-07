#ifndef _SSM_CORE_H
#define _SSM_CORE_H

/**
 * Header file shared across all SSM components.
 */

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h> /* For offsetof */
#include <stdint.h>
#include <stdlib.h>

/*** Forward struct declarations ***/
struct sv;      /* Defined in ssm-sv.h */
struct trigger; /* Defined in ssm-act.h */
struct act;     /* Defined in ssm-act.h */

/** Logical microsecond timestamps: assumed never to overflow. */
typedef uint64_t ssm_time_t;

/** ssm_time_t counts microseconds. */
#define TICKS_PER_SECOND (1000 * 1000)

/** Scheduling an event for this time is the same as unscheduling it. */
#define NO_EVENT_SCHEDULED ULONG_MAX

/** Thread priority */
typedef uint32_t priority_t;

/** The priority at the entry point of an SSM program. */
#define PRIORITY_AT_ROOT 0

/** Index of least significant priority bit */
typedef uint8_t depth_t;

/** The depth at the entry point of an SSM program. */
#define DEPTH_AT_ROOT 32

/**
 * The logical time of the current instant.
 * Defined and maintained by ssm-sched.c. Should not be modified by anyone else.
 *
 * FIXME: hide behind getter and setter.
 */
extern ssm_time_t now;

/**
 * Implementation of container_of that falls back to ISO C99 when GNU C is not
 * available (from https://stackoverflow.com/a/10269925/10497710)
 */
#ifdef __GNUC__
#define member_type(type, member) __typeof__(((type *)0)->member)
#else
#define member_type(type, member) const void
#endif
#define container_of(ptr, type, member)                                        \
  ((type *)((char *)(member_type(type, member) *){ptr} -                       \
            offsetof(type, member)))

#endif /* _SSM_CORE_H */
