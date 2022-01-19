See @ref mem.

@defgroup mem Memory management

@brief Values are always word-sized, and may point to heap-allocated,
reference-counted <em>objects</em>; heap memory is allocated from a set of
size-indexed memory pools.

For simplicity and flexibility, all values in SSM runtime are word-sized,
represented using #ssm_value_t.
When the size of a value exceeds what can be represented by a word, the value
is a pointer to a heap-allocated <em>object</em>.
In particular, they point to a memory management metadata header, #ssm_mm.

The SSM runtime uses reference counting to manage the lifetime of heap objects.
The reference count is also maintained in the #ssm_mm header.
When a reference is duplicated, use ssm_dup() to increment its reference count.
When a reference is dropped, use ssm_drop() decrement its reference and free if
necessary.
These macros first check whether the given #ssm_value_t is heap-allocated
before attempting to dereference the heap pointer; to skip this check (for
example, if a value is known to be a heap object at compile time), use
ssm_dup_unsafe() and ssm_drop_unsafe().
When the reference count of an object reaches zero, it is cleaned up and freed
using ssm_drop_final(), which drop any references held by the soon-to-be-freed
object.

The #ssm_mm header's @a kind field indicates what #ssm_kind the object is.
This determines its memory layout in the heap, in particular where the object
might hold references (pointers) to other heap objects.
This information is used by ssm_drop_final() to figure out which values (if
any) to ssm_drop().

Heap objects are allocated for each #ssm_kind using functions with prefix @a
ssm_new_:

- ssm_new_adt()
- ssm_new_time()
- ssm_new_sv()

The SSM runtime comes with its own platform-agnostic allocator, parameterized
by handlers set using ssm_mem_init().
It allocates small pieces of memory in <em>blocks</em>, within designated
<em>memory pools</em>.
Each memory pool consists of zero or more fixed-size <em>memory pages</em>, and
are requested from the platform/OS on-demand via the alloc_page() handler.
Meanwhile, allocations for larger ranges of memory are deferred to the
alloc_mem() and free_mem() handlers.

The allocator's memory pools may be configured using the #SSM_MEM_POOL_MIN,
#SSM_MEM_POOL_FACTOR_BASE2, #SSM_MEM_POOL_COUNT, and #SSM_MEM_PAGE_SIZE
preprocessor constants.
The default values produce 4 memory pools, of sizes 16B, 64B, 256B, and 1024B,
and a memory page size of 4096B.
