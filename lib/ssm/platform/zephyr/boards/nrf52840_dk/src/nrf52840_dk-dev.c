#include <platform/zephyr-dev.h>

#if 0 // This subsystem isn't ready yet

SSM_ZEPHYR_DEFINE_INPUT_DEVICE_GPIO(sw0, ssm_marshal(0),
                                    GPIO_INT_EDGE_TO_ACTIVE);
SSM_ZEPHYR_DEFINE_INPUT_DEVICE_GPIO(sw1, ssm_marshal(0),
                                    GPIO_INT_EDGE_TO_ACTIVE);
SSM_ZEPHYR_DEFINE_INPUT_DEVICE_GPIO(sw2, ssm_marshal(0),
                                    GPIO_INT_EDGE_TO_ACTIVE);
SSM_ZEPHYR_DEFINE_INPUT_DEVICE_GPIO(sw3, ssm_marshal(0),
                                    GPIO_INT_EDGE_TO_ACTIVE);

SSM_ZEPHYR_DEFINE_OUTPUT_DEVICE_GPIO(led0, ssm_marshal(0));
SSM_ZEPHYR_DEFINE_OUTPUT_DEVICE_GPIO(led1, ssm_marshal(0));
SSM_ZEPHYR_DEFINE_OUTPUT_DEVICE_GPIO(led2, ssm_marshal(0));
SSM_ZEPHYR_DEFINE_OUTPUT_DEVICE_GPIO(led3, ssm_marshal(0));

int ssm_zephyr_initialize_board_devices(ssm_act_t *parent) {
  SSM_ZEPHYR_INIT_INPUT_DEVICE_GPIO(sw0);
  SSM_ZEPHYR_INIT_INPUT_DEVICE_GPIO(sw1);
  SSM_ZEPHYR_INIT_INPUT_DEVICE_GPIO(sw2);
  SSM_ZEPHYR_INIT_INPUT_DEVICE_GPIO(sw3);

  SSM_ZEPHYR_INIT_OUTPUT_DEVICE_GPIO(led0);
  SSM_ZEPHYR_INIT_OUTPUT_DEVICE_GPIO(led1);
  SSM_ZEPHYR_INIT_OUTPUT_DEVICE_GPIO(led2);
  SSM_ZEPHYR_INIT_OUTPUT_DEVICE_GPIO(led3);
  return 0;
}
#endif // #if 0
