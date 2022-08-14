#ifndef _PLATFORM_ZEPHYR_TIMER_H
#define _PLATFORM_ZEPHYR_TIMER_H

#include <stdint.h>
#include <drivers/counter.h>

/** @brief Absolute time; never to overflow. */
typedef uint64_t ssm_time_t;

/** @brief Callback that can be registered with ssm_timer_set_alarm(). */
typedef void (*ssm_timer_callback_t)(uint64_t time, void *user_data);

/** @brief Initialize and start the timer.
 *
 *  @returns zero on success; error code otherwise.
 */
int ssm_timer_start(void);

/** @brief Register an alarm that will invoke some callback.
 *
 *  @param wake_time  absolute time at which alarm expires.
 *  @param cb         callback to be invoked ot @a wake_time.
 *  @param user_data  data given to @a cb at @a wake_time.
 *
 *  @retval 0         if successful.
 *  @retval -EALREADY if alarm was already set.
 *  @retval -ENOTSUP  if device not supported.
 *  @retval -EINVAL   if alarm settings are invalid.
 *  @retval -ETIME    if absolute alarm was set too late.
 */
int ssm_timer_set_alarm(ssm_time_t wake_time, ssm_timer_callback_t cb,
                        void *user_data);

/** @brief Cancel an alarm previously set using ssm_timer_set_alarm().
 *
 *  @returns zero on success; error code otherwise.
 */
int ssm_timer_cancel(void);

/** @brief Read the lower
 *
 *  Must only be called after successfully calling ssm_timer_start().
 *
 *  @returns the current 64-bit timestamp.
 */
ssm_time_t ssm_timer_read(void);

/** @brief Read the lower 32 bits of the current time.
 *
 *  On many platforms, this can be faster and more efficient than reading all
 *  64 bits.
 *
 *  Must only be called after successfully calling ssm_timer_start().
 *
 *  @returns lower 32 bits of current time.
 */
uint32_t ssm_timer_read32(void);

/** @brief Number of bits used by lower-order counter. */
#define SSM_TIMER_RAW_BITS 32

/** @brief Higher order bits of non-native 64-bit timer. */
extern volatile uint32_t __ssm_timer_hi;

/** @brief Raw 32-bit timer values, when native 64-bit timers unavailable. */
typedef struct ssm_raw_time {
  uint32_t hi0; /**< First read of higher 32 bits of timer. */
  uint32_t lo;  /**< Lower 32 bits of timer. */
  uint32_t hi1; /**< Second read of higher 32 bits of timer. */
} ssm_raw_time_t;

/** @brief Compute most significant bit of a lower-order bits. */
#define __ssm_timer_lo_msb(lo) ((lo) & (0x1u << (SSM_TIMER_RAW_BITS - 1)))

/** @brief Shift higher-order 32-bit value to the top half of a 64-bit value. */
#define __ssm_timer_hi_shift(hi) (((uint64_t)(hi)) << SSM_TIMER_RAW_BITS)

/** @brief Compute the 64-bit value from higher- and lower-order bits. */
#define __ssm_timer_combine(hi, lo) (__ssm_timer_hi_shift(hi) + (lo))

/** @brief Compute the 64-bit #ssm_time_t from #ssm_raw_time_t.
 *
 *  @param t  #ssm_raw_time_t to compute time from.
 *  @returns  64-bit timestamp encoded by @a t.
 */
#define ssm_timer_calc(t)                                                      \
  (((t).hi0) == ((t).hi1)     ? __ssm_timer_combine((t).hi0, (t).lo)           \
   : __ssm_timer_lo_msb(t.lo) ? __ssm_timer_combine((t).hi0, (t).lo)           \
                              : __ssm_timer_combine((t).hi1, (t).lo))

/** @brief Read raw timer values to #ssm_raw_t.
 *
 *  On platforms without native 64-bit timers, this will offer lower latency.
 *
 *  Must only be called after successfully calling ssm_timer_start().
 *
 *  The #ssm_raw_time_t value should be interpreted using ssm_timer_calc().
 *
 *  @param tp   pointer to #ssm_raw_t populated with current time reading.
 */
#define ssm_timer_read_raw(tp)                                                 \
  do {                                                                         \
    (tp)->hi0 = __ssm_timer_hi;                                                \
    compiler_barrier();                                                        \
    (tp)->lo = ssm_timer_read32();                                             \
    compiler_barrier();                                                        \
    (tp)->hi1 = __ssm_timer_hi;                                                \
  } while (0)

/** @brief Board-specific Zephyr timer configuration.
 *
 *  @param ssm_timer_dev  timer device to initialize.
 *  @returns              0 on success, non-zero otherwise.
 */
int ssm_timer_board_start(const struct device *ssm_timer_dev);

#endif /* ifndef _PLATFORM_ZEPHYR_TIMER_H */
