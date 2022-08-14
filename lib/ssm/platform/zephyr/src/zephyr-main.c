#include <ssm-internal.h>
#include <kernel.h>

__attribute__((weak)) void main(void) {
  ssm_platform_entry();
}
