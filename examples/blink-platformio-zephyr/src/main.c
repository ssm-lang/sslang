#include <zephyr.h>
#include <device.h>
#include <devicetree.h>
#include <drivers/gpio.h>

#include <ssm.h>

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
    ssm_sensitize((ssm_sv_t *) act->led, &act->trigger);

    for (;;) {
      ssm_later_bool(act->led, SSM_MILLISECOND * 500, true);
      act->pc = 1;
      return;
    case 1:
      ssm_later_bool(act->led, SSM_MILLISECOND * 500, false);
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
  led_handler_act_t *act = (led_handler_act_t *) sact;
  switch (act->pc) {
  case 0:
    if (act->port == NULL) break;
    ssm_sensitize((ssm_sv_t *)act->led, &act->trigger);
    act->pc = 1;
    return;

  case 1:
    gpio_pin_set(act->port, act->pin, act->led->value);
    return;
  }
  ssm_leave((ssm_act_t *) act, sizeof(led_handler_act_t));
}

void main()
{
  printk("Sleeping for a second for you to start a terminal\r\n");
  k_sleep(K_SECONDS(1));
  printk("Starting...\r\n");

  ssm_bool_t led;
  ssm_initialize_bool(&led);

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

  do {
    ssm_tick();
    k_sleep(K_SECONDS(0.5)); // FIXME
  } while (ssm_next_event_time() != SSM_NEVER);
}
