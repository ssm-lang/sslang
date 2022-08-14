#ifndef _PLATFORM_ZEPHYR_DEV_H
#define _PLATFORM_ZEPHYR_DEV_H

#if 0 // This subsystem isn't really ready yet

/**
 * @addtogroup device
 * @{
 */

#include <drivers/gpio.h>
#include <ssm-platform.h>
#include <ssm.h>

/** @brief Zephyr runtime interface for input GPIO devices. */
typedef struct {
  struct gpio_dt_spec spec; /**< Device tree spec of this GPIO device. */
  gpio_flags_t int_flags;   /**< Flags to configure interrupt handler with. */
  struct gpio_callback cb;  /**< Callback invoked when input is received. */
  ssm_sv_t *sv;             /**< Scheduled variable bound to input device. */
} ssm_zephyr_input_gpio_t;

/** @brief Zephyr runtime interface for output GPIO devices. */
typedef struct {
  ssm_act_t act;            /**< Activation record for output handler. */
  ssm_trigger_t trigger;    /**< Trigger to wake up output handler. */
  struct gpio_dt_spec spec; /**< Device tree spec of this output device. */
  ssm_sv_t *sv;             /**< Scheduled variable bound to output device. */
} ssm_zephyr_output_gpio_t;

/** @brief Declare a Zephyr input GPIO device.
 *
 *  This device may or may not be used by the user program; if not, it will not
 *  be configured at runtime.
 *
 *  @a flags should be either `GPIO_INT_EDGE_BOTH` or `GPIO_INT_EDGE_TO_ACTIVE`.
 *
 *  @param name     the name of the device.
 *  @param init_val initial value of the device, of type #ssm_value_t.
 *  @param flags    flags to configure interrupt handler
 */
#define SSM_ZEPHYR_DEFINE_INPUT_DEVICE_GPIO(name, init_val, flags)             \
  BUILD_ASSERT(DT_NODE_HAS_STATUS(DT_ALIAS(name), okay),                       \
               "Input device not defined: " Z_STRINGIFY(name));                \
  SSM_DECLARE_DEVICE_EXISTS(INPUT, name);                                      \
  ssm_sv_t SSM_DEVICE(name) = {.value = init_val};                             \
  ssm_zephyr_input_gpio_t SSM_DEVICE_INPUT(name) = {                           \
      .spec = GPIO_DT_SPEC_GET(DT_ALIAS(name), gpios),                         \
      .int_flags = flags,                                                      \
      .sv = &SSM_DEVICE(name)}

/** @brief Declare a Zephyr output GPIO device.
 *
 *  This device may or may not be used by the user program; if not, it will not
 *  be configured at runtime.
 *
 *  @param name     the name of the device.
 *  @param init_val initial value of the device, of type #ssm_value_t.
 */
#define SSM_ZEPHYR_DEFINE_OUTPUT_DEVICE_GPIO(name, init_val)                   \
  BUILD_ASSERT(DT_NODE_HAS_STATUS(DT_ALIAS(name), okay),                       \
               "Output device not defined: " Z_STRINGIFY(name));               \
  SSM_DECLARE_DEVICE_EXISTS(OUTPUT, name);                                     \
  ssm_sv_t SSM_DEVICE(name) = {.value = init_val};                             \
  ssm_zephyr_output_gpio_t SSM_DEVICE_OUTPUT(name) = {                         \
      .spec = GPIO_DT_SPEC_GET(DT_ALIAS(name), gpios),                         \
      .sv = &SSM_DEVICE(name)}

/** @brief Declare a Zephyr GPIO device.
 *
 *  This device may or may not be used by the user program; if not, it will not
 *  be configured at runtime.
 *
 *  @a flags should be either `GPIO_INT_EDGE_BOTH` or `GPIO_INT_EDGE_TO_ACTIVE`.
 *
 *  @param name     the name of the device.
 *  @param init_val initial value of the device, of type #ssm_value_t.
 *  @param flags    flags to configure interrupt handler
 */
#define SSM_ZEPHYR_DEFINE_IO_DEVICE_GPIO(name, init_val, flags)                \
  BUILD_ASSERT(DT_NODE_HAS_STATUS(DT_ALIAS(name), okay),                       \
               "I/O device not defined: " Z_STRINGIFY(name));                  \
  SSM_DECLARE_DEVICE_EXISTS(INPUT, name);                                      \
  SSM_DECLARE_DEVICE_EXISTS(OUTPUT, name);                                     \
  ssm_sv_t SSM_DEVICE(name) = {.value = init_val};                             \
  ssm_zephyr_input_gpio_t SSM_DEVICE_INPUT(name) = {                           \
      .spec = GPIO_DT_SPEC_GET(DT_ALIAS(name), gpios),                         \
      .int_flags = flags,                                                      \
      .sv = &SSM_DEVICE(name)};                                                \
  ssm_zephyr_output_gpio_t SSM_DEVICE_OUTPUT(name) = {                         \
      .spec = GPIO_DT_SPEC_GET(DT_ALIAS(name), gpios),                         \
      .sv = &SSM_DEVICE(name)}

/** @brief Defined by each board to initialize its I/O devices. */
int ssm_zephyr_initialize_board_devices(ssm_act_t *parent);

/** @brief Initialize a GPIO input device at runtime. */
int ssm_zephyr_initialize_input_device_gpio(ssm_zephyr_input_gpio_t *input);

/** @brief Initialize a GPIO output device at runtime. */
int ssm_zephyr_initialize_output_device_gpio(ssm_act_t *parent,
                                             ssm_priority_t priority,
                                             ssm_depth_t depth,
                                             ssm_zephyr_output_gpio_t *out);

/** @brief Helper macro to initialize a GPIO input device. */
#define SSM_ZEPHYR_INIT_INPUT_DEVICE_GPIO(name)                                \
  do {                                                                         \
    if (!SSM_DEVICE_IN_USE(INPUT, name))                                       \
      break;                                                                   \
    int err =                                                                  \
        ssm_zephyr_initialize_input_device_gpio(&SSM_DEVICE_INPUT(name));      \
    if (err)                                                                   \
      return err;                                                              \
  } while (0)

/** @brief Helper macro to initialize a GPIO output device. */
#define SSM_ZEPHYR_INIT_OUTPUT_DEVICE_GPIO(name)                               \
  do {                                                                         \
  } while (0)

/** @} */

#endif // #if 0

#endif /* _PLATFORM_ZEPHYR_DEV_H */
