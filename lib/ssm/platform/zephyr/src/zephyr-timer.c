#include <platform/zephyr-timer.h>

#include <logging/log.h>

LOG_MODULE_REGISTER(ssm_timer);

#if !DT_NODE_HAS_STATUS(DT_ALIAS(ssm_timer), okay)
#error "ssm-timer device is not supported on this board"
#endif

const struct device *ssm_timer_dev;

#define SSM_TIMER_ALARM_CHANNEL 0

#define SSM_TIMER32_TOTAL_BITS 32

#define SSM_TIMER32_TOP                                                        \
  ((0x1u << (SSM_TIMER32_TOTAL_BITS - 1)) |                                    \
   ((0x1u << (SSM_TIMER32_TOTAL_BITS - 1)) - 1))

#define SSM_TIMER32_GUARD (SSM_TIMER32_TOP / 2)

volatile uint32_t __ssm_timer_hi;

static void overflow_handler(const struct device *dev, void *user_data) {
  uint32_t key = irq_lock();
  ++__ssm_timer_hi;
  irq_unlock(key);
}

int ssm_timer_start(void) {
  int err;
  struct counter_top_cfg top_cfg = {
      .callback = overflow_handler,
      .ticks = SSM_TIMER32_TOP,
      .user_data = NULL,
      .flags = COUNTER_TOP_CFG_DONT_RESET,
  };

  if ((err = ssm_timer_board_start(ssm_timer_dev)))
    return err;

  if (!(ssm_timer_dev = device_get_binding(DT_LABEL(DT_ALIAS(ssm_timer)))))
    return -ENODEV;

  if (!(SSM_TIMER32_TOP <= counter_get_max_top_value(ssm_timer_dev)))
    return -ENOTSUP;

  LOG_INF("timer will run at %d Hz\r\n", counter_get_frequency(ssm_timer_dev));
  LOG_INF("timer will wraparound at %08x ticks\r\n", SSM_TIMER32_TOP);

  if ((err = counter_set_top_value(ssm_timer_dev, &top_cfg)))
    return err;

  if ((err = counter_set_guard_period(ssm_timer_dev, SSM_TIMER32_GUARD,
                                      COUNTER_GUARD_PERIOD_LATE_TO_SET)))
    return err;

  return counter_start(ssm_timer_dev);
}

static ssm_timer_callback_t timer_cb;

void counter_alarm_callback(const struct device *dev, uint8_t chan_id,
                            uint32_t ticks, void *user_data) {
  // SSM_DEBUG_ASSERT(chan_id == SSM_TIMER_ALARM_CHANNEL,
  //                  "counter_alarm_callback: unexpected chan_id: %u\r\n",
  //                  chan_id);
  // SSM_DEBUG_ASSERT(timer_cb, "counter_alarm_callback: timer_cb not set\r\n");
  ssm_timer_callback_t cb = timer_cb;
  timer_cb = NULL;
  cb(ticks, user_data);
}

int ssm_timer_set_alarm(ssm_time_t wake_time, ssm_timer_callback_t cb,
                        void *user_data) {

  struct counter_alarm_cfg cfg;

  if (timer_cb)
    return -EALREADY;

  timer_cb = cb;

  cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
  cfg.ticks = wake_time;
  cfg.callback = counter_alarm_callback;
  cfg.user_data = user_data;
  return counter_set_channel_alarm(ssm_timer_dev, SSM_TIMER_ALARM_CHANNEL,
                                   &cfg);
}

int ssm_timer_cancel(void) {
  int ret =
      counter_cancel_channel_alarm(ssm_timer_dev, SSM_TIMER_ALARM_CHANNEL);
  timer_cb = NULL;
  return ret;
}

ssm_time_t ssm_timer_read(void) {
  ssm_raw_time_t t;
  ssm_timer_read_raw(&t);
  return ssm_timer_calc(t);
}

uint32_t ssm_timer_read32(void) {
  uint32_t ticks;
  counter_get_value(ssm_timer_dev, &ticks);
  return ticks;
}
