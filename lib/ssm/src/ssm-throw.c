#include <ssm.h>

/** Default, platform-generic implementation of ssm_throw; spins forever. */
void ssm_throw(int reason, const char *file, int line, const char *func) {
  for(;;);
}
