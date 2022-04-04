See @ref act.

@defgroup act Activation records

@brief The local state of each user-defined coroutine is saved within its <em>activation record</em>.

Each coroutine executes at most once per instant, and between instants, their activation records are managed by the runtime scheduler.

In addition to the arguments and local variables of a coroutine, which are specific to each coroutine, activation records also contain some common fields, such as the priority and depth of a coroutine. These common fields are found in the #ssm_act_t "base class", which should be embedded in the beginning of each routine-specific activation record.

Activation records are heap-allocated and initialized in routine-specific <em>enter</em> function and registered with the scheduler using ssm_activate(). When ssm_tick() is called, a coroutine is executed for that instant using its <em>step</em> function. When a coroutine returns, its step function should call ssm_leave() to deallocate the activation record.
