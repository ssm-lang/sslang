#ifndef _SSM_PLATFORM_H
#define _SSM_PLATFORM_H

#include <ssm.h>

/**
 * @addtogroup platform
 * @{
 */

/** @brief Platform-defined entry point.
 *
 *  Should be called by @a main function to start running SSM code.
 *
 *  @returns    0 on success, negative errno otherwise.
 */
int ssm_platform_entry(void);

/** @brief Insert an input event into the input queue.
 *
 *  The input event is timestamped with the current (platform-defined)
 *  timestamp. The given value @a val will later be written to the scheduled
 *  variable @a sv once the tick function has caught up with current time.
 *
 *  Note that this function is not re-entrant and should be called with some
 *  kind of lock held to avoid clobbering the input queue.
 *
 *  @param sv   the scheduled variable to be updated.
 *  @param val  the value @a sv should be updated with.
 *  @returns    0 on success, negative errno otherwise.
 */
int ssm_insert_input(ssm_sv_t *sv, ssm_value_t val);

/** @} */

#if 0 // This subsystem isn't really ready yet.
/**
 * @addtogroup device
 * @{
 */

/** @brief Lookup symbol for device with given name. */
#define SSM_DEVICE(name) name

/** @brief Lookup symbol for input device's #ssm_input_t using given name. */
#define SSM_DEVICE_INPUT(name) ssm_input_device_##name

/** @brief Lookup symbol for output device's #ssm_output_t using given name. */
#define SSM_DEVICE_OUTPUT(name) ssm_output_device_##name

/** @brief Lookup symbol determining whether a device is in use.
 *
 *  @param kind   either `INPUT` or `OUTPUT`.
 *  @param name   the name of the device.
 */
#define SSM_DEVICE_IN_USE(kind, name) SSM_##kind##_DEVICE_IN_USE_##name

/** @brief Announce that a device of some name exists and may be used.
 *
 *  Note that this only declares that a device <emph>might</emph> be used.
 *  By default, it is assumed not to be used, using a weak definition.
 *
 *  User programs may declare that they <emph>will</emph> use some device using
 *  SSM_DECLARE_DEVICE_IN_USE(), overriding this macro's weak definition.
 *
 *  @param kind   either `INPUT` or `OUTPUT`.
 *  @param name   the name of the device.
 */
#define SSM_DECLARE_DEVICE_EXISTS(kind, name)                                  \
  bool __attribute__((weak)) SSM_DEVICE_IN_USE(kind, name) = false

/** @brief Announce that a device of some name will (or won't) be used.
 *
 *  Note that this only declares that a device <emph>might</emph> be used.
 *  By default, it is assumed not to be used, using a weak definition.
 *
 *  User programs may declare that they <emph>will</emph> use some device using
 *  SSM_DECLARE_DEVICE_IN_USE(), overriding this macro's weak definition.
 *
 *  @param kind   either `INPUT` or `OUTPUT`.
 *  @param name   the name of the device.
 *  @param in_use boolean indicating whether the device will be used.
 */
#define SSM_DECLARE_DEVICE_IN_USE(kind, name, in_use)                          \
  bool SSM_DEVICE_IN_USE(kind, name) = in_use

/** @brief Declare a Zephyr input device used by the user program.
 *
 *  @param name   the name of the device.
 */
#define SSM_DECLARE_ZEPHYR_INPUT_DEVICE(name)                                  \
  SSM_DECLARE_DEVICE_IN_USE(INPUT, name, true);                                \
  extern ssm_sv_t SSM_DEVICE(name);

/** @brief Declare a Zephyr output device used by the user program.
 *
 *  @param name   the name of the device.
 */
#define SSM_DECLARE_ZEPHYR_OUTPUT_DEVICE(name)                                 \
  SSM_DECLARE_DEVICE_IN_USE(OUTPUT, name, true);                               \
  extern ssm_sv_t SSM_DEVICE(name);

/** @brief Declare a Zephyr I/O device used by the user program.
 *
 *  @param name   the name of the device.
 */
#define SSM_DECLARE_ZEPHYR_IO_DEVICE(name)                                     \
  SSM_DECLARE_DEVICE_IN_USE(INPUT, name, true);                                \
  SSM_DECLARE_DEVICE_IN_USE(OUTPUT, name, true);                               \
  extern ssm_sv_t SSM_DEVICE(name);

/** @} */
#endif // #if 0

#endif /* ifndef _SSM_PLATFORM_H */
