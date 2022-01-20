See @ref time.

@defgroup time Time

@brief SSM timestamps are 64-bit values which should never wraparound, and are represented by #ssm_time_t.

The runtime library maintains a monotonically increasing "current time" which can be read using ssm_now().

For portability reasons, this particular runtime implementation uses nanoseconds as its time base; it is the responsibility of platform code to convert this resolution to the tick rate of the hardware timer. See <https://github.com/ssm-lang/ssm-runtime/pull/1>.
