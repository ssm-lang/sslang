#include <zephyr.h>
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

void main()
{
  printk("Sleeping for a second for you to start a terminal\r\n");
  k_sleep(K_SECONDS(1));
  printk("Starting...\r\n");

  ssm_tick();

  printf("Ticked\r\n");
}
