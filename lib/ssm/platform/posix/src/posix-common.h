#ifndef POSIX_COMMON_H
#define POSIX_COMMON_H

#include <ssm-internal.h>

#include <ctype.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>
#include <string.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h>

// #define DBG(...) fprintf(stderr, __VA_ARGS__)
#define DBG(...)                                                               \
  do {                                                                         \
  } while (0)

#define NANOS 1000000000L

#define timespec_diff(a, b)                                                    \
  (a).tv_nsec - (b).tv_nsec < 0                                                \
      ? (struct timespec){.tv_sec = (a).tv_sec - (b).tv_sec - 1,               \
                          .tv_nsec = (a).tv_nsec - (b).tv_nsec + NANOS}        \
      : (struct timespec) {                                                    \
    .tv_sec = (a).tv_sec - (b).tv_sec, .tv_nsec = (a).tv_nsec - (b).tv_nsec    \
  }

#define timespec_add(a, b)                                                     \
  (a).tv_nsec + (b).tv_nsec > NANOS                                            \
      ? (struct timespec){.tv_sec = (a).tv_sec + (b).tv_sec + 1,               \
                          .tv_nsec = (a).tv_nsec + (b).tv_nsec - NANOS}        \
      : (struct timespec) {                                                    \
    .tv_sec = (a).tv_sec + (b).tv_sec, .tv_nsec = (a).tv_nsec + (b).tv_nsec    \
  }

#define timespec_lt(a, b)                                                      \
  (a).tv_sec == (b).tv_sec ? (a).tv_nsec < (b).tv_nsec : (a).tv_sec < (b).tv_sec

#define timespec_time(t) (((t).tv_sec * NANOS) + (t).tv_nsec)

#define timespec_of(ns)                                                        \
  (struct timespec) { .tv_sec = (ns) / NANOS, .tv_nsec = (ns) % NANOS }

typedef int fd_sem_t[2];

#define fd_sem_write(fd_sem) write(fd_sem[1], "a", 1)
#define fd_sem_read(fd_sem)                                                    \
  do {                                                                         \
    char buf;                                                                  \
    read(fd_sem[0], &buf, 1);                                                  \
  } while (0)

extern int ssm_sem_fd[2];
extern atomic_size_t rb_r;
extern atomic_size_t rb_w;
extern pthread_mutex_t rb_lk;

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

#endif /* POSIX_COMMON_H */
