/* The blink example: blink an LED at 1 Hz using the SSM runtime driven by
 * a hardware counter.
 *
 * This Zephyr example relies on a counter device that runs freely at 16 MHz
 * and can send an interrupt when an alarm time is reached.
 *
 * An ssm_tick_thread runs ssm_tick(), schedules a future alarm at the
 * next earliest event time, then blocks until a message is sent to
 * its queue and repeats.
 *
 * When an alarm expires, it places an event in the queue, waking up
 * the ssm_tick_thread.
 *
 * The LED is modeled as a schedule variable of standard bool type.
 *
 * The blink routine schedules the led variable to toggle in 500 ms,
 * then waits on the variable and repeats.
 *
 * The led_handler routine waits on the led variable, then copies its
 * state to a GPIO pin and repeats.  The led_handler routine takes the
 * devicetree name of the LED pin as an argument and saves this
 * information locally so it knows which GPIO to control.
 */

#include <zephyr.h>
#include <device.h>
#include <devicetree.h>
#include <drivers/gpio.h>
#include <drivers/counter.h>

#include <ssm.h>


// This should be in drivers/gpio.h, but it was missing from earlier versions
// of Zephyr 2.6.  Remove this function once it causes an error.
static inline int gpio_pin_set_dt(const struct gpio_dt_spec *spec, int value)
{
  return gpio_pin_set(spec->port, spec->pin, value);
}


#if !DT_NODE_HAS_STATUS(DT_ALIAS(led0), okay)
#error "led0 device alias not defined"
#endif

#if !DT_NODE_HAS_STATUS(DT_ALIAS(ssm_timer), okay)
#error "ssm-timer device is not supported on this board"
#endif

#define SSM_TICK_STACKSIZE 4096
#define SSM_TICK_PRIORITY 7

const struct device *ssm_timer_dev = 0;
struct counter_alarm_cfg ssm_timer_cfg;

/* blink(bool &led) =
 * loop
 *   500 ms later led <- true
 *   wait led
 *   500 ms later led <- false
 *   wait led
 */
typedef struct {
  SSM_ACT_FIELDS;
  ssm_trigger_t trigger;
  ssm_bool_t *led;
} blink_act_t;

ssm_stepf_t step_blink;

blink_act_t *enter_blink(ssm_act_t *parent,
			 ssm_priority_t priority,
			 ssm_depth_t depth,
			 ssm_bool_t *led)
{
  blink_act_t *act = (blink_act_t *)
    ssm_enter(sizeof(blink_act_t), step_blink, parent, priority, depth);
  act->led = led;
  act->trigger.act = (ssm_act_t *) act;
  return act;
}

void step_blink(ssm_act_t *sact)
{
  blink_act_t *act = (blink_act_t *)sact;
  switch (act->pc) {
  case 0:
    ssm_sensitize(&(act->led->sv), &act->trigger);
    for (;;) {
      ssm_later_bool(act->led, ssm_now() + SSM_MILLISECOND * 500, true);
      act->pc = 1;
      return;
    case 1:
      ssm_later_bool(act->led, ssm_now() + SSM_MILLISECOND * 500, false);
      act->pc = 2;
      return;
    case 2:
      ;
    }
  }
}

/* led_handler(bool &led, gpio_spec &spec) =
 *
 * gpio_pin_configure(spec, GPIO_OUTPUT_ACTIVE)
 * loop
 *   wait led
 *   gpio_pin_set(spec, led)
 */
typedef struct {
  SSM_ACT_FIELDS;
  ssm_trigger_t trigger;
  ssm_bool_t *led;
  const struct gpio_dt_spec *spec;
} led_handler_act_t;

ssm_stepf_t step_led_handler;

led_handler_act_t *enter_led_handler(ssm_act_t *parent,
				     ssm_priority_t priority,
				     ssm_depth_t depth,
				     ssm_bool_t *led_var,
				     const struct gpio_dt_spec *led_spec)
{
  led_handler_act_t *act = (led_handler_act_t *)
    ssm_enter(sizeof(led_handler_act_t), step_led_handler, parent,
	      priority, depth);
  act->led = led_var;
  act->spec = led_spec;

  act->trigger.act = (ssm_act_t *) act;

  return act;
}

void step_led_handler(ssm_act_t *sact)
{
  led_handler_act_t *act = (led_handler_act_t *) sact;
  switch (act->pc) {
  case 0:
    gpio_pin_configure_dt(act->spec, GPIO_OUTPUT_ACTIVE);
    ssm_sensitize(&(act->led->sv), &act->trigger);
    act->pc = 1;
    return;

  case 1:
    gpio_pin_set_dt(act->spec, act->led->value);
    printk("led: %d\r\n", act->led->value ? 1 : 0);
    return;
  }
  ssm_leave((ssm_act_t *) act, sizeof(led_handler_act_t));
}

/* Types of events from the environment */
typedef enum { SSM_TIMEOUT } ssm_event_type_t;

/* An event from the environment, e.g., a timeout */
typedef struct {
  ssm_event_type_t type;
} ssm_env_event_t;

K_MSGQ_DEFINE(ssm_env_queue, sizeof(ssm_env_event_t), 100, 1);


/* Send a timeout event to wake up the ssm_tick_thread */
void send_timeout_event(const struct device *dev, uint8_t chan, uint32_t ticks,
			void *user_data)
{
  static ssm_env_event_t timeout_msg = { .type = SSM_TIMEOUT };

  k_msgq_put(&ssm_env_queue, &timeout_msg, K_NO_WAIT);
}

/*** Thread responsible for receiving events, calling ssm_tick(),
 * and starting the timeout counter
 */
void ssm_tick_thread_body(void *p1, void *p2, void *p3)
{
  ssm_env_event_t msg;

  for (;;) {
    ssm_tick();
    printk("now %llu\r\n", ssm_now());

    ssm_time_t wake = ssm_next_event_time();
    if (wake != SSM_NEVER) {
      const struct counter_config_info *config =
          (const struct counter_config_info *)ssm_timer_dev->config;
      ssm_timer_cfg.ticks = wake * config->freq / NSEC_PER_SEC;
      int r = counter_set_channel_alarm(ssm_timer_dev, 0, &ssm_timer_cfg);
      switch (r) {
      case -ENOTSUP:
	printk("counter_set_channel_alarm failed: ENOTSUP\r\n");
	break;
      case -EINVAL:
	printk("counter_set_channel_alarm failed: EINVAL\r\n");
	break;
      case -ETIME:
	printk("counter_set_channel_alarm failed: ETIME\r\n");
	break;
      case -EBUSY:
	printk("counter_set_channel_alarm failed: EBUSY\r\n");
	break;
      default:
	printk("counter_set_channel_alarm failed: %d\r\n", r);
	break;
      case 0:
	;
      }
    }

    k_msgq_get(&ssm_env_queue, &msg, K_FOREVER); // Block for the next event
    switch( msg.type ) {
    case SSM_TIMEOUT:
      break;
    }
  }
}

K_THREAD_STACK_DEFINE(ssm_tick_thread_stack, SSM_TICK_STACKSIZE);
struct k_thread ssm_tick_thread;

// main() terminates, so this variable can't be local to it
ssm_bool_t led;

// The devicetree spec for the GPIO pin, which is passed to enter_led_handler.
// GPIO_DT_SPEC_GET is a macro that expands to a static initializer,
// so it may only be used to initialize a variable such as this global.
//
// This requires the devicetree to have in it something like
//
//         aliases {
//                led0 = &led0;
//         };
//
//         led0: led_0 {
//                gpios = <&gpio0 13 GPIO_ACTIVE_LOW>;
//         };
//
// See, e.g., boards/arm/nrf52840dk_nrf52840/nrf52840dk_nrf52840.dts
const struct gpio_dt_spec led_spec = GPIO_DT_SPEC_GET(DT_ALIAS(led0), gpios);

/* main() =
 * bool led
 * gpio_dt_spec led_spec <- GPIO_DT_SPEC_GET(DT_ALIAS(led0), gpios)
 * blink(led) || led_handler(led, led_spec)
 */
void main()
{
  printk("Sleeping for a second for you to start a terminal\r\n");
  k_sleep(K_SECONDS(1));
  printk("Blink example starting...\r\n");

  // Initialize the counter device
  if (!(ssm_timer_dev = device_get_binding(DT_LABEL(DT_ALIAS(ssm_timer))))) {
    printk("device_get_binding failed with ssm-timer\r\n");
  } else {
    printk("ssm-timer running at %d Hz\r\n",
	   counter_get_frequency(ssm_timer_dev));
    ssm_timer_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE |
                          COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
    ssm_timer_cfg.callback = send_timeout_event;
    ssm_timer_cfg.user_data = &ssm_timer_cfg;

    if (counter_set_guard_period(ssm_timer_dev, UINT_MAX/2,
				 COUNTER_GUARD_PERIOD_LATE_TO_SET))
      printk("counter_set_guard_period failed\r\n");
    if (counter_start(ssm_timer_dev))
      printk("counter_start failed\r\n");
  }

  ssm_initialize_bool(&led);

  // Fork the blink and led_handler routines
  {
    ssm_depth_t new_depth = SSM_ROOT_DEPTH - 1;
    ssm_priority_t new_priority = SSM_ROOT_PRIORITY;
    ssm_priority_t pinc = 1 << new_depth;
    ssm_activate((ssm_act_t *) enter_blink(&ssm_top_parent, new_priority,
					   new_depth, &led));
    new_priority += pinc;
    ssm_activate((ssm_act_t *) enter_led_handler(&ssm_top_parent,
						 new_priority, new_depth, &led,
						 &led_spec));
  }

  // Spin up the ssm_tick_thread
  k_thread_create(&ssm_tick_thread, ssm_tick_thread_stack,
		  K_THREAD_STACK_SIZEOF(ssm_tick_thread_stack),
		  ssm_tick_thread_body, 0, 0, 0,
		  SSM_TICK_PRIORITY, 0, K_NO_WAIT);
}
