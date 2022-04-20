See @ref closure.

@defgroup closure Closures

The SSM runtime supports closures, a runtime representation for functions.
These encapsulate a function pointer (specifically, an enter function pointer)
and a vector of partially-applied arguments (referred to as @a argv).

When closures are applied to arguments, they may or may not <em>reduce</em>.
When a reduction takes place, a child process is created: the enter function
pointer is applied to @a argv, and the resulting activation record is scheduled
via ssm_activate().
If this happens, the caller must yield (i.e., set the program counter and
return) so that the child process may run to completion before the caller
resumes.

It might be possible to statically determine whether a caller needs to yield,
so in the general case, the caller should yield <em>conditionally</em>, using
ssm_has_children() to determine whether any reduction took place.
Optimizations may eliminate this check.

Closures are allocated with enough space for all eventual arguments to be
stored in @a argv, which is in turn passed to the callee.
Though this wastes extra space, it vastly increases opportunities to reuse the
same closure (if it is uniquely owned by the caller).

The #ssm_mm header's @a val_count field indicates how many arguments are
currently applied (also called @a arg_count), i.e., how many fields the garbage
collector should scan when the closure is dropped.
The header's @a tag field indicates how many arguments the closure will
accommodate (also called @a arg_cap), and determines its memory layout and size.
When @a arg_count reaches @a arg_cap, the closure should reduce.

A closure with an @a arg_cap of `N` has the following layout:

~~~{.c}
struct ssm_closureN {
  struct ssm_mm mm;
  ssm_value_t argv[N];
};
~~~

The "template" for each closure's memory layout is defined by #ssm_closure1;
closures with more argument capacity look like #ssm_closure1 with longer @a
argv.

The function ssm_closure_apply() applies a closure to an argument, with or
without reduction.
The SSM runtime library defines many helper interfaces to allow compilers to
generate optimized code for interacting with closures.
For example, ssm_closure_push() and ssm_closure_pop() directly add and remove
arguments to @a argv without any runtime checks or function call overhead.
ssm_closure_apply() is implemented in terms of these helpers, and should be
inlined by an optimizing code generator where appropriate.

Meanwhile, ssm_closure_apply_final() optimizes the last time a closure value is
used. It assumes that the caller is releasing the last remaining reference to
a closure, so that the closure may be safely modified in-place.

When process <code>a</code> applies function <code>f</code> to argument
<code>x</code>, and saves the result to <code>r</code>, the most general case
(omitting reference-counting) looks like:

~~~{.c}
ssm_closure_apply(f, x, a, a->priority, a->depth, &r);
if (ssm_has_children(a)) {
  a->pc = N;
  return;
case N:;
}
~~~

where <code>N</code> is the running program counter.

For convenience, <em>_auto</em> variants of ssm_closure_apply() and
ssm_closure_apply_final() call ssm_dup() on the argument.
