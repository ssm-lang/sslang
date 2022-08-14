See @ref error.

@defgroup error Error handling

@brief The SSM_THROW() macro is used to throw unrecoverable runtime errors.

SSM_THROW() calls the underlying ssm_throw() function with information about where in the source code the error was encountered. Because each platform may have different error-handling and logging capabilities, this library requires that the ssm_throw() symbol to be defined externally, typically by platform-specific bindings.

Any definition of ssm_throw() should not be expected to return; otherwise, the behavior of subsequently interacting with the runtime library becomes undefined.
For example, a platform-independent definition of ssm_throw() may just spin indefinitely, though this does not provide very helpful feedback for debugging:

~~~{.c}
void ssm_throw(ssm_error_t reason, const char *file, int line,
               const char *func) {
  for (;;);
}
~~~

A more sensible definition might call exit(), perhaps after logging some relevant information using the provided arguments:

~~~{.c}
void ssm_throw(ssm_error_t reason, const char *file, int line,
               const char *func) {
  printf("SSM error at %s:%d (%s): reason: %d\n", file, line, func, reason);
  exit(reason);
}
~~~
