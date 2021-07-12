#include <zephyr.h>
#include <ssm.h>

void main()
{
  printk("Sleeping for a second for you to start a terminal\r\n");
  k_sleep(K_SECONDS(1));
  printk("Starting...\r\n");

  ssm_tick();

  printf("Ticked\r\n");
}
