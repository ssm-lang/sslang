#include <zephyr.h>
#include <device.h>
#include <devicetree.h>
#include <drivers/gpio.h>
#include <drivers/counter.h>

#include <ssm.h>

#if !DT_NODE_HAS_STATUS(DT_ALIAS(led0), okay)
#error "led0 device alias not defined"
#endif

#if !DT_NODE_EXISTS(DT_ALIAS(ssm_timer))
#error "ssm-timer device alias not defined, e.g., in the device tree overlay"
#endif

#define SSM_TICK_STACKSIZE 4096
#define SSM_TICK_PRIORITY 7

/* Two output ideas:
 *
 * 1. Special type with its own update/assign function that performs the output
 *    operation with every assignment.  Allows/enables multiple write events
 *    per instant.
 * 2. Handler process that stays triggered on an I/O variable and simply
 *    copies the payload of the variable in question to the I/O device.
 *    Naturally restricts outputs to single event per instant unless the
 *    user writes the handler themselves.
 *
 *    mymain(led0, led1) ||
 *    led_handler(led0) ||
 *    led_handler(led1)
 *
 *    led0,led0handler = getled(0)  <-- Library creates the led0handler function
 *    mymain(led0) || led0handler
 */

const struct device *ssm_timer_dev = 0;
struct counter_alarm_cfg ssm_timer_cfg;

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
  printk("act->led %d act->led->sv.triggers %d\r\n", (int) act->led, (int) act->led->sv.triggers);
  printk("act->led->sv %d %d %d %d %d\r\n",
	 (int) act->led,
	 (int) act->led->sv.update,
	 (int) act->led->sv.triggers,
	 (int) act->led->sv.later_time,
	 (int) act->led->sv.last_updated);

  return act;
}

void step_blink(ssm_act_t *sact)
{
  printk("step_blink\r\n");
  blink_act_t *act = (blink_act_t *)sact;
  switch (act->pc) {
  case 0:
    
    printk("step_blink act->led %d act->led->sv.triggers %d\r\n", (int) act->led, (int) act->led->sv.triggers);
    printk("act->led->sv %d %d %d %d %d\r\n",
	   (int) act->led,
	   (int) act->led->sv.update,
	   (int) act->led->sv.triggers,
	   (int) act->led->sv.later_time,
	   (int) act->led->sv.last_updated);

    ssm_sensitize(&(act->led->sv), &act->trigger);
    printk("ssm_sensitize\r\n");

    for (;;) {
      ssm_later_bool(act->led, ssm_now() + 1, true);
      act->pc = 1;
      printk("step_blink return\r\n");
      return;
    case 1:
      ssm_later_bool(act->led, ssm_now() + 1, false);
      act->pc = 2;
      return;
    case 2:
      ;
    }
  }  
}

typedef struct {
  SSM_ACT_FIELDS;
  ssm_trigger_t trigger;
  ssm_bool_t *led;
  const struct device *port;
  gpio_pin_t pin;
} led_handler_act_t;

#define enter_led_handler(parent, priority, depth, var, id) \
  enter_led_handler_(parent, priority, depth, var,          \
		     DT_GPIO_LABEL(id, gpios),	            \
		     DT_GPIO_PIN(id, gpios),	            \
		     DT_GPIO_FLAGS(id, gpios))

ssm_stepf_t step_led_handler;

led_handler_act_t *enter_led_handler_(ssm_act_t *parent,
				      ssm_priority_t priority,
				      ssm_depth_t depth,
				      ssm_bool_t *led_var,
				      const char *label,
				      gpio_pin_t pin,
				      gpio_flags_t flags)
{
  led_handler_act_t *act = (led_handler_act_t *)
    ssm_enter(sizeof(led_handler_act_t), step_led_handler, parent,
	      priority, depth);
  act->led = led_var;
  act->trigger.act = (ssm_act_t *) act;

  if (0 != (act->port = device_get_binding(label))) {
    act->pin = pin;
    if (0 > gpio_pin_configure(act->port, act->pin, GPIO_OUTPUT_ACTIVE | flags))
      act->port = 0;
  }
  return act;
}

void step_led_handler(ssm_act_t *sact)
{
  printk("step_led_handler\r\n");
  led_handler_act_t *act = (led_handler_act_t *) sact;
  switch (act->pc) {
  case 0:
    if (act->port == NULL) break;
    ssm_sensitize(&(act->led->sv), &act->trigger);
    act->pc = 1;
    return;

  case 1:
    gpio_pin_set(act->port, act->pin, act->led->value);
    return;
  }
  ssm_leave((ssm_act_t *) act, sizeof(led_handler_act_t));
}

/*** Types of events from the environment
 */
typedef enum { SSM_TIMEOUT } ssm_event_type_t;

/*** An event from the environment, e.g., a timeout
 */
typedef struct {
  ssm_event_type_t type;
} ssm_env_event_t;

K_MSGQ_DEFINE(ssm_env_queue, sizeof(ssm_env_event_t), 100, 1);


void send_timeout_event(const struct device *dev, uint8_t chan, uint32_t ticks,
			void *user_data)
{
  printk("send_timeout_event\r\n");
  static ssm_env_event_t timeout_msg = { .type = SSM_TIMEOUT };

  k_msgq_put(&ssm_env_queue, &timeout_msg, K_NO_WAIT);
}

/*** Thread responsible for receiving events, calling ssm_tick(),
 * and starting the timeout counter
 *
 */

void tick_thread_body(void *p1, void *p2, void *p3)
{
  printk("tick_thread_body\r\n");
  ssm_env_event_t msg;
  
  for (;;) {
    k_msgq_get(&ssm_env_queue, &msg, K_FOREVER);
    printk("got a message\r\n");
    switch( msg.type ) {
    case SSM_TIMEOUT:
      break;      
    }

    printk("Running ssm_tick\r\n");
    ssm_tick();

    printk("now updated to %d\r\n", (int) ssm_now());

    ssm_time_t wake = ssm_next_event_time();
    printk("ssm_next_event_time = %d\r\n", (int) wake);
    if (wake != SSM_NEVER) {
      ssm_timer_cfg.ticks = wake;
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
	// FIXME: it's returning this; not clear the counter is right.
      default:
	printk("counter_set_channel_alarm failed: %d\r\n", r);
	break;
      case 0:
	;
      }
    }
  }
}

K_THREAD_STACK_DEFINE(ssm_tick_thread_stack, SSM_TICK_STACKSIZE);
struct k_thread ssm_tick_thread;

// Warning: putting this inside main fails because the function may
// terminate and the stack space reused
ssm_bool_t led;

void main()
{
  printk("Sleeping for a second for you to start a terminal\r\n");
  k_sleep(K_SECONDS(1));
  printk("Starting...\r\n");

  
  //  if (!(ssm_timer_dev = device_get_binding(DT_LABEL(DT_ALIAS(ssm_timer))))) {
  if (!(ssm_timer_dev = device_get_binding(DT_LABEL(DT_INST(0, st_stm32_rtc))))) {
    printk("device_get_binding failed with ssm-timer\r\n");
  } else {
    printk("ssm-timer running at %d Hz\r\n",
	   counter_get_frequency(ssm_timer_dev));
    ssm_timer_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE |
                          COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
    ssm_timer_cfg.callback = send_timeout_event;
    ssm_timer_cfg.user_data = &ssm_timer_cfg;

    if (counter_set_channel_alarm(ssm_timer_dev, 0, &ssm_timer_cfg)) {
      printk("counter_set_channel_alarm failed\r\n");
    } else {
      /*
      if (counter_set_guard_period(ssm_timer_dev, UINT_MAX/2,
				   COUNTER_GUARD_PERIOD_LATE_TO_SET)) {
	printk("counter_set_guard_period failed\r\n");
      }
      */
      counter_start(ssm_timer_dev);
    }    
  }
  
  ssm_initialize_bool(&led);

  printk("led triggers: %d\r\n", (int) led.sv.triggers);

  
  {
    ssm_depth_t new_depth = SSM_ROOT_DEPTH - 1;
    ssm_priority_t new_priority = SSM_ROOT_PRIORITY;
    ssm_priority_t pinc = 1 << new_depth;
    ssm_activate((ssm_act_t *) enter_blink(&ssm_top_parent, new_priority,
					   new_depth, &led));
    new_priority += pinc;
    ssm_activate((ssm_act_t *) enter_led_handler(&ssm_top_parent,
						 new_priority, new_depth, &led,
						 DT_ALIAS(led0)));
  }

  /*
  k_thread_create(&ssm_tick_thread, ssm_tick_thread_stack,
		  K_THREAD_STACK_SIZEOF(ssm_tick_thread_stack),
		  tick_thread_body, 0, 0, 0,		  
		  SSM_TICK_PRIORITY, 0, K_NO_WAIT);

  send_timeout_event(ssm_timer_dev, 0, 0, 0);
  */
 
  do {
    ssm_tick();
    k_sleep(K_SECONDS(0.5)); // FIXME
  } while (ssm_next_event_time() != SSM_NEVER);
}
