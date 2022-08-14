int __dumy_decl = 0;

#if 0 // This subsystem isn't really ready yet

#include <platform/zephyr-dev.h>
#include <ssm-internal.h>
#include <ssm-platform.h>

#include <devicetree.h>
#include <drivers/gpio.h>
#include <sys/util_macro.h>

extern atomic_t rb_r;
extern atomic_t rb_w;

static void input_event_handler(const struct device *port,
                                struct gpio_callback *cb,
                                gpio_port_pins_t pins) {
  ssm_zephyr_input_gpio_t *input =
      container_of(cb, ssm_zephyr_input_gpio_t, cb);
  uint32_t key = irq_lock();
  ssm_insert_input(input->sv, ssm_marshal(gpio_pin_get(port, input->spec.pin)));

  irq_unlock(key);
}

int ssm_zephyr_initialize_input_device_gpio(ssm_zephyr_input_gpio_t *input) {
  int err;

  if ((err = gpio_pin_configure(input->spec.port, input->spec.pin,
                                GPIO_INPUT | input->spec.dt_flags)))
    return err;

  if ((err = gpio_pin_interrupt_configure(input->spec.port, input->spec.pin,
                                          input->int_flags)))
    return err;

  gpio_init_callback(&input->cb, input_event_handler, BIT(input->spec.pin));

  err = gpio_add_callback(input->spec.port, &input->cb);
  if (err)
    return err;

  return 0;
}

/**** OUTPUT ******************************************************************/

static void step_output_gpio_handler(ssm_act_t *actg) {
  ssm_zephyr_output_gpio_t *out =
      container_of(actg, ssm_zephyr_output_gpio_t, act);
  ssm_sv_t *sv = out->sv;

  switch (actg->pc) {
  case 0:
    out->trigger.act = actg;
    ssm_sv_sensitize(sv, &out->trigger);
    actg->pc = 1;
    return;

  case 1:
    /* TODO: this only works for bools */
    gpio_pin_set(out->spec.port, out->spec.pin, ssm_unmarshal(sv->value));
    return;
  }

  // Unreachable
  ssm_desensitize(&out->trigger);

  ssm_act_t *parent = actg->caller;
  if (--parent->children == 0)
    parent->step(parent);
}

int ssm_zephyr_initialize_output_device_gpio(ssm_act_t *parent,
                                             ssm_priority_t priority,
                                             ssm_depth_t depth,
                                             ssm_zephyr_output_gpio_t *out) {
  int err;
  if ((err = gpio_pin_configure(out->spec.port, out->spec.pin,
                                GPIO_OUTPUT_ACTIVE | out->spec.dt_flags)))
    return err;

  if ((err = gpio_pin_set(out->spec.port, out->spec.pin,
                          ssm_unmarshal(out->sv->value))))
    return err;

  out->act = (ssm_act_t){
      .step = step_output_gpio_handler,
      .caller = parent,
      .pc = 0,
      .children = 0,
      .priority = priority,
      .depth = depth,
      .scheduled = false,
  };
  ++parent->children;
  return 0;
}
#endif // #if 0
