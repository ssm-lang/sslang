/** @file ssm-internal.h
 *  @brief The internal interface of the SSM runtime.
 *
 *  This header file contains definitions and declarations that should not be
 *  exposed to user code.
 *
 *  @author Stephen Edwards (sedwards-lab)
 *  @author John Hui (j-hui)
 */
#ifndef _SSM_SCHED_H
#define _SSM_SCHED_H

#include <ssm-platform.h>
#include <ssm.h>

/** @ingroup error
 *  @brief Throw an internal error.
 *
 *  @platformonly
 *
 *  @param cond the condition to assert.
 */
#define SSM_ASSERT(cond)                                                       \
  do                                                                           \
    if (!(cond))                                                               \
      SSM_THROW(SSM_INTERNAL_ERROR);                                           \
  while (0)

/** @ingroup time
 *  @brief The time of the next event in the event queue.
 *
 *  Used to determine whether and when to invoke ssm_tick().
 *
 *  @platformonly
 *
 *  @returns the next event time, or #SSM_NEVER if the event queue is empty.
 */
ssm_time_t ssm_next_event_time(void);

/** @ingroup act
 *  @brief Whether there are still active processes in the activation queue.
 *
 *  @platformonly
 *
 *  @returns true if there is at least one active process, false otherwise.
 */
bool ssm_active(void);

/** @ingroup time
 *  @brief Reset the scheduler.
 *
 *  Set #now to 0; clear the event and activation record queues.
 *
 *  This does not need to be called before calling ssm_tick() for the first
 *  time; the global state automatically starts initialized.
 *
 *  @platformonly
 */
void ssm_reset(void);

/** @ingroup time
 *  @brief Advance the current model time.
 *
 *  @a next must be later than ssm_now(), and earlier than or equal to
 *  ssm_next_event_time().
 *
 *  Exposed so that platform code can perform external variable updates, to
 *  implement external inputs.
 *
 *  @platformonly
 *
 *  @param next the time to advance to.
 *
 *  @throws SSM_INVALID_TIME  @a next is earlier than or equal to #now.
 *  @throws SSM_NOT_READY     @a next is later than the earliest queued event.
 */
void ssm_set_now(ssm_time_t next);

/** @ingroup sv
 *  @brief Perform a (delayed) update on a variable.
 *
 *  Schedules all routine activation records sensitive to @a sv in the
 *  activation queue.
 *
 *  Should only be called if the variable is scheduled to be updated #now.
 *
 *  Exposed so that platform code can perform external variable updates.
 *
 *  @platformonly
 *
 *  @param sv the variable.
 *
 *  @throws SSM_NOT_READY   @a sv was not scheduled to be updated #now.
 */
void ssm_update(ssm_sv_t *sv);

/** @ingroup sv
 *  @brief Unschedule any pending events on a variable.
 *
 *  Should be called before the variable is dropped.
 *
 *  Nothing happens if the variable does not have a pending event.
 *
 *  @platformonly
 *
 *  @param var  the variable.
 */
void ssm_unschedule(ssm_sv_t *var);

/** @ingroup act
 *  @brief Run the system for the next scheduled instant.
 *
 *  If there is nothing left to run in the current instant, advance #now to the
 *  time of the earliest event in the queue, if any.
 *
 *  Removes every event at the head of the event queue scheduled for #now,
 *  updates each variable's current value from its later value, and schedules
 *  all sensitive triggers in the activation queue. Finally, execute the step
 *  functions of all scheduled routines, in order of priority.
 *
 *  Should only be called if there are activation records to execute #now,
 *  or if #now is earlier than ssm_next_event_time().
 *
 *  @platformonly
 *
 *  @throws SSM_INTERNAL_ERROR  there are stale events in the queue before #now.
 */
void ssm_tick(void);

/**
 * @addtogroup mem
 * @{
 */

/** @brief Initializes the underlying allocator system.
 *
 *  Memory pages are requested from the platform/OS on-demand via the provided
 *  @a alloc_page_handler. This handler must return a pointer to the beginning
 *  of a range of memory of size #SSM_MEM_PAGE_SIZE, and the page @em must be
 *  zeroed out. These pages are assumed to never be freed.
 *
 *  To support arbitrarily large allocations, SSM's allocator uses @a
 *  alloc_mem_handler to allocate memory, and @a free_mem_handler to release it.
 *  There are no requirements on the contents of memory allocated with @a
 *  alloc_mem_handler. These handlers may also assume they will not be invoked
 *  to request memory ranges of less than #SSM_MEM_POOL_MAX bytes.
 *
 *  If the allocator is compiled with valgrind support (i.e., @a USE_VALGRIND is
 *  defined), it will perform a leak-check summary, to checkpoint how much
 *  memory has already been allocated.
 *
 *  @platformonly
 *
 *  @param alloc_page_handler allocates pages.
 *  @param alloc_mem_handler  allocates arbitrarily large.
 *  @param free_mem_handler   frees memory allocated with @a alloc_mem_handler.
 */
void ssm_mem_init(void *(*alloc_page_handler)(void),
                  void *(*alloc_mem_handler)(size_t),
                  void (*free_mem_handler)(void *, size_t));

/** @brief Tears down the underlying allocator system.
 *
 *  If the allocator is compiled with valgrind support (i.e., @a USE_VALGRIND is
 *  defined), it will perform a full leak-check summary, to report how much
 *  memory has been leaked since ssm_mem_init().
 *
 *  @TODO this doesn't actually call @a free_page_handler yet. It still needs to
 *        be implemented, perhaps with the help of a superblock header to keep
 *        track of all pages allocated for each mempool
 *
 *  @platformonly
 *
 *  @param free_page_handler  frees pages allocated with @a alloc_page_handler.
 */
void ssm_mem_destroy(void (*free_page_handler)(void *));

#ifndef SSM_MEM_POOL_MIN
/** @brief Block size of the smallest memory pool.
 *
 *  Must be strictly greater than the word size, i.e., the size of #ssm_word_t.
 *
 *  @platformonly
 */
#define SSM_MEM_POOL_MIN 16
#endif

#if SSM_MEM_POOL_MIN < SSM_POINTER_SIZE
#error SSM_MEM_POOL_MIN must be larger than word size.
#endif

#ifndef SSM_MEM_POOL_FACTOR_BASE2
/** @brief Factor between each successive memory pool size, in base 2.
 *
 *  Must be strictly greater than 0.
 *
 *  @platformonly
 */
#define SSM_MEM_POOL_FACTOR_BASE2 2
#endif

#if SSM_MEM_POOL_FACTOR_BASE2 < 1
#error SSM_MEM_POOL_FACTOR_BASE2 must be strictly greater than 0.
#endif

#ifndef SSM_MEM_POOL_COUNT
/** @brief Number of memory pools.
 *
 *  Must be strictly greater than 0.
 *
 *  @platformonly
 */
#define SSM_MEM_POOL_COUNT 4
#endif

#if SSM_MEM_POOL_COUNT < 1
#error SSM_MEM_POOL_COUNT must be strictly greater than 0.
#endif

/** @brief Compute the size of a memory pool.
 *
 *  @platformonly
 *
 *  @param pool   the 0-index of the memory pool.
 *  @returns      the size of the memory pool at position @a pool.
 */
#define SSM_MEM_POOL_SIZE(pool)                                                \
  (SSM_MEM_POOL_MIN << (SSM_MEM_POOL_FACTOR_BASE2 * pool))

/** @brief The size of the largest memory pool.
 *
 *  @platformonly
 */
#define SSM_MEM_POOL_MAX SSM_MEM_POOL_SIZE(SSM_MEM_POOL_COUNT - 1)

#ifndef SSM_MEM_PAGE_SIZE
/** @brief The size of a memory page; must be greater than #SSM_MEM_POOL_MAX.
 *
 *  @platformonly
 */
#define SSM_MEM_PAGE_SIZE SSM_MEM_POOL_SIZE(SSM_MEM_POOL_COUNT)
#endif

/** @} */

#ifdef USE_VALGRIND
#include <valgrind/memcheck.h>
#endif

#endif /* _SSM_SCHED_H */
