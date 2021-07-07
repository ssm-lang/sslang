#ifndef _SSM_RUNTIME_H
#define _SSM_RUNTIME_H

/**
 * This header file is implemented by ssm-sched.c, and is meant to be used by
 * the "driver" of the runtime---that which connects the internal scheduler to
 * the outside world.
 *
 * The primary interface is the tick function, which the driver is responsible
 * for calling at the appropriate time. For instance, for a real-time
 * implementation, the driver is responsible for calling tick at the right
 * wall-clock time, according to the next event time returned by the previous
 * call to tick.
 */

/**
 * Used by the runtime to control execution of a program.
 */
#include <ssm-core.h>

/**
 * Execute the system for the current instant, and returns the next event time.
 */
extern void tick(void);

/**
 * Set the current time.
 */
extern void set_now(ssm_time_t);

/**
 * The time of the earliest event in the event queue.
 */
ssm_time_t next_event_time(void);

#endif /* ifndef _SSM_RUNTIME_H */
