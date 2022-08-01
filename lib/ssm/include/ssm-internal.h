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

/** @ingroup adt
 *  @brief Compute the size of a heap-allocated ADT.
 *  @todo document
 */
#define ssm_adt_size(vc)                                                       \
  (sizeof(struct ssm_adt1) + sizeof(ssm_value_t) * ((vc)-1))

#define ssm_closure_size(vc)                                                   \
  (sizeof(struct ssm_closure1) + (sizeof(ssm_value_t) * ((vc)-1)))

/**
 * @addtogroup mem
 * @{
 */

/** @brief The different kinds of heap objects, enumerated.
 *
 *  Types enumerated here that are not ADTs are chosen because they cannot be
 *  easily or efficiently expressed as a product of words. For instance, 64-bit
 *  timestamps cannot be directly stored in the payload of a regular heap
 *  object, where even-numbered timestamps may be misinterpreted as pointers.
 */
enum ssm_kind {
  SSM_ADT_K = 0, /**< ADT object, e.g., #ssm_adt1 */
  SSM_TIME_K,    /**< 64-bit timestamps, #ssm_time_t */
  SSM_SV_K,      /**< Scheduled variables, #ssm_sv_t */
  SSM_CLOSURE_K, /**< Closure object, #ssm_closure1 */
};

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

/**
 * @addtogroup platform_time
 * @{
 */

/** @brief Platform-dependent time representation, supporting 32-bit timers.
 *
 *  Platforms that only have 32-bit timers can emulate 64-bit timers using
 *  another 32-bit to keep track of the higher order bits:
 *
 *      uint32_t hardware_timer_read(void);
 *      volatile uint32_t raw_time_hi; // incremented on 32-bit timer wraparound
 *
 *  To account for the higher order bits wrapping around the higher order bits
 *  must be read twice, before and after reading the 32-bit timer:
 *
 *      uint32_t hi0 = raw_time_hi;
 *      compiler_barrier();
 *      uint32_t lo = hardware_timer_read();
 *      compiler_barrier();
 *      uint32_t hi1 = raw_time_hi;
 *
 *  The wraparound event can be observed by looking for any discrepancy between
 *  @a hi0 and @a hi1. In case of wraparound, the most significant bit of @a lo
 *  will indicate which @a raw_time_hi read should be used:
 *
 *      uint64_t time;
 *      if (hi0 == hi1)
 *        // no wraparound happened; hi0 and hi1 are interchangeable
 *        time = ((uint64_t) hi0 << 32) + lo;
 *      else if ((lo & (1 << 31)) == 0)
 *        // wraparound happened after reading lo; use hi1
 *        time = ((uint64_t) hi1 << 32) + lo;
 *      else
 *        // wraparound happened before reading lo; use hi0
 *        time = ((uint64_t) hi0 << 32) + lo;
 *
 *  However, computing @a time this way is costly and may waste precious cycles,
 *  especially during input-handling interrupt service routines; further cycles
 *  are wasted scaling this 64-bit counter value to #ssm_time_t nanoseconds. To
 *  avoid this latency, the input handler can save all three "raw" 32-bit
 *  values, and defer computation to beyond the input handler.
 *
 *  For this reason, #ssm_platform_time_t is defined as a union type that
 *  supports multiple, platform-specific representations of time. For platforms
 *  with 32-bit timers, the input handler can store the three raw values in the
 *  @a raw_time32 field. For platforms with 64-bit timers, the unscaled 64-bit
 *  timer value can be stored in the @a raw_time64 field. Finally, the 64-bit
 *  timestamp, in nanoseconds, is stored in the @a ssm_time field.
 *
 *  @TODO these comments trail off/aren't coherent from here onwards.
 *
 *  to store these three 32-bit values, as well as an
 *  ssm_raw_time32_calc() macro to compute the 64-bit counter value from those
 *  three values.
 *
 *  For platforms with access to a 64-bit timer, it may make more
 *
 *  It is the role of the platform
 *  to determine whether.
 *
 *  In those contexts, this computation
 *  can be deferred by directly storing the three 32-bit reads into the @a
 *  raw_time field of an #ssm_platform_time_t, and computing the actual
 *  instead storing the result into an #ssm_platform_time_t, and later using the
 *  provided ssm_raw_time_calc32() macro to compute the measured #ssm_time_t.
 *
 *  On platforms which have a 64-bit timer, #ssm_platform_time_t just instead
 *  wraps an #ssm_time_t, and ssm_raw_time_calc() macro expands into a
 *  zero-overhead struct access.
 *
 *  In any case, an #ssm_platform_time_t is embedded in #ssm_input packets. How
 *  these time are read is platform-dependent, but should follow the read
 *  pattern outlined above.
 *
 *  @platformonly
 */
typedef union {
  struct {
    uint32_t hi0; /**< First read from higher order bits. */
    uint32_t lo;  /**< Read from 32-bit timer for lower order bits. */
    uint32_t hi1; /**< Second read from higher order bits. */
  } raw_time32;
  uint64_t raw_time64;
  ssm_time_t ssm_time;
} ssm_platform_time_t;

/** @brief Helper macro to obtain a 64-bit counter value from two 32-bit values.
 *
 *  @note the result produced by this macro is not scaled, i.e., it just
 *  produces a measure of platform-specific 64-bit "ticks".
 *
 *  @platformonly
 *
 *  @param hi word containing the higher 32 bits.
 *  @param lo word containing the lower 32 bits.
 *  @returns  the 64-bit value represented by @a hi and @a lo.
 */
#define ssm_raw_time_combine(hi, lo) ((((uint64_t)(hi)) << 32) + (lo))

/** @brief Compute a #ssm_time_t from an #ssm_platform_time_t.
 *
 *  When SSM_TIMER64_PRESENT is defined, this is macro becomes a zero-overhead
 *  struct access.
 *
 *  @a hi0 must be equal to or one greater than @a hi1; otherwise the behavior
 *  of this macro is undefined.
 *
 *  @param t  the #ssm_platform_time_t containing the raw timestamp.
 *  @returns  the #ssm_time_t computed from @a t.
 */
#define ssm_raw_time32_calc(t)                                                 \
  (ssm_platform_time_t) {                                                      \
    .raw_time64 =                                                              \
        (t).raw_time32.hi0 == (t).raw_time32.hi1                               \
            ? ssm_raw_time_combine((t).raw_time32.hi0, (t).raw_time32.lo)      \
        : (t).lo & (0x1u << 31)                                                \
            ? ssm_raw_time_combine((t).raw_time32.hi0, (t).raw_time32.lo)      \
            : ssm_raw_time_combine((t).raw_time32.hi1, (t).raw_time32.lo)      \
  }

#define ssm_raw_time64_scale(t, s)                                             \
  (ssm_platform_time_t) { .ssm_time = (ssm_time_t)((t).raw_time64 * (s)) }

/** @} */

/**
 * @addtogroup input
 * @{
 */

/** @brief Input event produced by an input handler.
 *
 */
struct ssm_input {
  ssm_sv_t *sv;
  ssm_value_t payload;
  ssm_platform_time_t time;
};

#ifndef SSM_INPUT_RB_SIZE
#define SSM_INPUT_RB_SIZE 32
#endif

extern struct ssm_input ssm_input_rb[SSM_INPUT_RB_SIZE];

#define ssm_input_idx(i) ((i) % SSM_INPUT_RB_SIZE)
#define ssm_input_get(i) (&ssm_input_rb[ssm_input_idx(i)])

#define ssm_input_read_ready(r, w) (ssm_input_idx(r) != ssm_input_idx(w))
#define ssm_input_write_ready(r, w) (ssm_input_read_ready(r, w + 1))

/** @brief Consume #ssm_input packet, to prepare to ssm_tick().
 *
 *  Consumes zero or more input packets from #ssm_input_rb.
 *
 *  This should only be called when there are a non-zero number of packets to
 *  consume, i.e., when @a r is less than @a w.
 *
 *  @platformonly
 *
 *  @param r  logical index of the front of #ssm_input_rb, i.e., the read head.
 *  @param w  logical index of the end of #ssm_input_rb, i.e., the write head.
 *  @returns  the number of input packets consume.
 */
size_t ssm_input_consume(size_t r, size_t w);

/** @} */

#endif /* _SSM_SCHED_H */
