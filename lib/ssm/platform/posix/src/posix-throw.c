#include <execinfo.h>
#include <ssm-internal.h>
#include <stdio.h>
#include <unistd.h>

void ssm_throw(enum ssm_error reason, const char *file, int line,
               const char *func) {
  fprintf(stderr, "SSM error at %s:%s:%d: reason: %d\n", file, func, line,
          reason);
  void *array[64];
  int size;
  fprintf(stderr, "Backtrace:\n\n");
  size = backtrace(array, 64);
  backtrace_symbols_fd(array, size, STDERR_FILENO);
  exit(1);
}
