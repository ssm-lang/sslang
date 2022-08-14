#include <platform/zephyr-timer.h>

#include <drivers/clock_control/nrf_clock_control.h>
#include <logging/log.h>

LOG_MODULE_DECLARE(ssm_timer);

static inline void show_clocks(const struct device *ssm_timer_dev) {

  static const char *const lfsrc_s[] = {
#if defined(CLOCK_LFCLKSRC_SRC_LFULP)
    [NRF_CLOCK_LFCLK_LFULP] = "LFULP",
#endif
    [NRF_CLOCK_LFCLK_RC] = "LFRC",
    [NRF_CLOCK_LFCLK_Xtal] = "LFXO",
    [NRF_CLOCK_LFCLK_Synth] = "LFSYNT",
  };
  static const char *const hfsrc_s[] = {
      [NRF_CLOCK_HFCLK_LOW_ACCURACY] = "HFINT",
      [NRF_CLOCK_HFCLK_HIGH_ACCURACY] = "HFXO",
  };
  static const char *const clkstat_s[] = {
      [CLOCK_CONTROL_STATUS_STARTING] = "STARTING",
      [CLOCK_CONTROL_STATUS_OFF] = "OFF",
      [CLOCK_CONTROL_STATUS_ON] = "ON",
      [CLOCK_CONTROL_STATUS_UNAVAILABLE] = "UNAVAILABLE",
      [CLOCK_CONTROL_STATUS_UNKNOWN] = "UNKNOWN",
  };
  union {
    unsigned int raw;
    nrf_clock_lfclk_t lf;
    nrf_clock_hfclk_t hf;
  } src;
  enum clock_control_status clkstat;
  bool running;

  clkstat =
      clock_control_get_status(ssm_timer_dev, CLOCK_CONTROL_NRF_SUBSYS_LF);
  running = nrf_clock_is_running(NRF_CLOCK, NRF_CLOCK_DOMAIN_LFCLK, &src.lf);
  LOG_INF("LFCLK[%s]: %s %s ;", clkstat_s[clkstat], running ? "Running" : "Off",
          lfsrc_s[src.lf]);
  clkstat =
      clock_control_get_status(ssm_timer_dev, CLOCK_CONTROL_NRF_SUBSYS_HF);
  running = nrf_clock_is_running(NRF_CLOCK, NRF_CLOCK_DOMAIN_HFCLK, &src.hf);
  LOG_INF("HFCLK[%s]: %s %s\n", clkstat_s[clkstat], running ? "Running" : "Off",
          hfsrc_s[src.hf]);
}

int ssm_timer_board_start(const struct device *ssm_timer_dev) {

  const struct device *clock;
  int err;

  /* Configure nrf board to use external oscillator for timer */
  if (!(clock = device_get_binding(DT_LABEL(DT_INST(0, nordic_nrf_clock)))))
    return -ENODEV;

  if ((err = clock_control_on(clock, CLOCK_CONTROL_NRF_SUBSYS_HF)))
    return err;

  show_clocks(ssm_timer_dev);

  return 0;
}
