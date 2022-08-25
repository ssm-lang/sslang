/** @file ssm.h
 *  @brief Interface to the SSM runtime.
 *
 *  This file contains the public-facing interface of the SSM runtime.
 *
 *  @author Stephen Edwards (sedwards-lab)
 *  @author John Hui (j-hui)
 */
#ifndef _SSM_H
#define _SSM_H

#include <stdbool.h> /* For bool, true, false */
#include <stddef.h>  /* For offsetof */
#include <stdint.h>  /* For uint16_t, UINT64_MAX etc. */
#include <stdlib.h>  /* For size_t */

#ifdef CONFIG_MEM_TRACE
#include <stdio.h>
#endif

/**
 * @addtogroup error
 * @{
 */

/** @brief Error codes indicating reason for failure.
 *
 *  Platforms may extend the list of errors using #SSM_PLATFORM_ERROR like this:
 *
 *  ~~~{.c}
 *  enum {
 *    SSM_CUSTOM_ERROR_CODE1 = SSM_PLATFORM_ERROR,
 *    SSM_CUSTOM_ERROR_CODE2,
 *    // etc.
 *  };
 *  ~~~
 */
typedef enum ssm_error {
  SSM_INTERNAL_ERROR = 1,
  /**< Reserved for unforeseen, non-user-facing errors. */
  SSM_EXHAUSTED_ACT_QUEUE,
  /**< Tried to insert into full activation record queue. */
  SSM_EXHAUSTED_EVENT_QUEUE,
  /**< Tried to insert into full event queue. */
  SSM_EXHAUSTED_MEMORY,
  /**< Could not allocate more memory. */
  SSM_EXHAUSTED_PRIORITY,
  /**< Tried to exceed available recursion depth. */
  SSM_NOT_READY,
  /**< Not yet ready to perform the requested action. */
  SSM_INVALID_TIME,
  /**< Specified invalid time, e.g., scheduled assignment at an earlier time. */
  SSM_INVALID_MEMORY,
  /**< Invalid memory layout, e.g., using a pointer where int was expected. */
  SSM_PLATFORM_ERROR
  /**< Start of platform-specific error code range. */
} ssm_error_t;

/** @brief Terminate due to a non-recoverable error, with a specified reason.
 *
 *  Invoked when a process must terminate, e.g., when memory or queue space is
 *  exhausted. Not expected to terminate.
 *
 *  Wraps the underlying ssm_throw() exception handler, passing along the
 *  file name, line number, and function name in the source file where the error
 *  was thrown.
 *
 *  @param reason an #ssm_error_t specifying the reason for the error.
 */
#define SSM_THROW(reason) ssm_throw(reason, __FILE__, __LINE__, __func__)

/** @brief Underlying exception handler; must be overridden by each platform.
 *
 *  This is left undefined by the SSM runtime for portability. On platforms
 *  where exit() is available, that may be used. Where possible, an exception
 *  handler may also log additional information using the given parameters for
 *  better debuggability.
 *
 *  @param reason an #ssm_error_t specifying the reason for the error.
 *  @param file   the file name of the source file where the error was thrown.
 *  @param line   the line number of the source file where the error was thrown.
 *  @param func   the function name where the error was thrown.
 */
void ssm_throw(ssm_error_t reason, const char *file, int line,
               const char *func);

/** @} */

struct ssm_mm;
struct ssm_sv;
struct ssm_trigger;
struct ssm_act;

/**
 * @addtogroup act
 * @{
 */

/** @brief Thread priority.
 *
 *  Lower numbers execute first in an instant.
 */
typedef uint32_t ssm_priority_t;

/** @brief The priority for the entry point of an SSM program. */
#define SSM_ROOT_PRIORITY 0

/** @brief Index of least significant bit in a group of priorities
 *
 *  This only needs to represent the number of bits in the #ssm_priority_t type.
 */
typedef uint8_t ssm_depth_t;

/** @brief The depth at the entry point of an SSM program. */
#define SSM_ROOT_DEPTH (sizeof(ssm_priority_t) * 8)

/** @brief The function that does an instant's work for a routine. */
typedef void ssm_stepf_t(struct ssm_act *);

/** @brief Activation record for an SSM routine.
 *
 *  Routine activation record "base class." A struct for a particular
 *  routine must start with this type but then may be followed by
 *  routine-specific fields.
 */
typedef struct ssm_act {
  ssm_stepf_t *step;       /**< C function for running this continuation. */
  struct ssm_act *caller;  /**< Activation record of caller. */
  uint16_t pc;             /**< Stored "program counter" for the function. */
  uint16_t children;       /**< Number of running child threads. */
  ssm_priority_t priority; /**< Execution priority; lower goes first. */
  ssm_depth_t depth;       /**< Index of the LSB in our priority. */
  bool scheduled;          /**< True when in the schedule queue. */
} ssm_act_t;

/** @brief Indicates a routine should run when a scheduled variable is written.
 *
 *  Node in linked list of activation records, maintained by each scheduled
 *  variable to determine which continuations should be scheduled when the
 *  variable is updated.
 */
typedef struct ssm_trigger {
  struct ssm_trigger *next;      /**< Next sensitive trigger, if any. */
  struct ssm_trigger **prev_ptr; /**< Pointer to self in previous element. */
  struct ssm_act *act;           /**< Routine triggered by this variable. */
} ssm_trigger_t;

/** @brief Whether an activation record has any children. */
#define ssm_has_children(act) ((act)->children != 0)

/** @brief Allocate and initialize a routine activation record.
 *
 *  Uses the underlying memory allocator to allocate an activation record of
 *  a given @a size, and initializes it with the rest of the fields. Also
 *  increments the number of children that @a parent has.
 *
 *  This function assumes that the embedded #ssm_act_t is at the beginning of
 *  the allocated activation record. That is, it has the following layout:
 *
 *  ~~~{.c}
 *  struct {
 *    ssm_act_t act;
 *    // Other fields
 *  };
 *  ~~~
 *
 *  @param size     the size of the routine activation record to allocate.
 *  @param step     the step function of the routine.
 *  @param parent   the parent (caller) of the activation record.
 *  @param priority the priority of the activation record.
 *  @param depth    the depth of the activation record.
 *  @returns        the newly initialized activation record.
 *
 *  @throws SSM_EXHAUSTED_MEMORY out of memory.
 */
ssm_act_t *ssm_enter_int(size_t size, ssm_stepf_t step, ssm_act_t *parent,
                         ssm_priority_t priority, ssm_depth_t depth);

#ifdef CONFIG_MEM_TRACE
#define ssm_enter(si, st, pa, pr, de)                                          \
  (fprintf(stderr, "%s:%d:ssm_enter(%lu,_,_,_,_,_)\n", __FILE__, __LINE__,     \
           (si)),                                                              \
   ssm_enter_int((si), (st), (pa), (pr), (de)))
#else
#define ssm_enter(si, st, pa, pr, de)                                          \
  ssm_enter_int((si), (st), (pa), (pr), (de))
#endif

/** @brief Destroy the activation record of a routine before leaving.
 *
 *  Calls the parent if @a act is the last child.
 *
 *  @param act  the activation record of the routine to leave.
 *  @param size the size of activation record.
 */
void ssm_leave(ssm_act_t *act, size_t size);

/** @brief Schedule a routine to run in the current instant.
 *
 *  This function is idempotent: it may be called multiple times on the same
 *  activation record within an instant; only the first call has any effect.
 *
 *  @param act activation record of the routine to schedule.
 *
 *  @throws SSM_EXHAUSTED_ACT_QUEUE the activation record is full.
 */
void ssm_activate(ssm_act_t *act);

/** @brief An activation record for the parent of the top-most routine.
 *
 *  This activation record should be used as the parent of the entry point.
 *  For example, if the entry point is named `main`, with activation record type
 *  `main_act_t` and step function `step_main`:
 *
 *  ~~~{.c}
 *  ssm_enter(sizeof(main_act_t), step_main, &ssm_top_parent,
 *            SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH)
 *  ~~~
 */
extern ssm_act_t ssm_top_parent;

/** @} */

/**
 * @addtogroup mem
 * @{
 */

/** @brief Values are 32-bits, the largest supported machine word size. */
typedef uint32_t ssm_word_t;

/** @brief SSM values are either "packed" values or heap-allocated. */
typedef union {
  struct ssm_mm *heap_ptr; /**< Pointer to a heap-allocated object. */
  ssm_word_t packed_val;   /**< Packed value. */
} ssm_value_t;

/** @brief The memory management metadata "header" for heap-allocated objects.
 *
 *  This header should always be embedded in heap-allocated objects as the first
 *  field (at memory offset 0); values of type #ssm_value_t will point to this
 *  header and use its @a kind and other fields to figure out the size and
 *  memory layout of the rest of the object.
 *
 *  The interpretation and usage of the latter 16 bits of this header, i.e., the
 *  @a info field, depends on the value of @a kind. For objects encoding
 *  variants (e.g., #ssm_adt1), the @a count field counts the number of fields
 *  in the object payload, while the @a tag records which variant is inhabited
 *  by the object. Meanwhile, vector-style objects (e.g., #ssm_closure1) also
 *  use the @a count field to record the number of values present, but use the
 *  @a cap field to determine the full capacity of the payload. Finally, objects
 *  like arrays and blobs do not need to record more than their size, and
 *  benefit from making use of all 16 bits to support up to 65536 sizes.
 */
struct ssm_mm {
  uint8_t ref_count; /**< The number of references to this object. */
  uint8_t kind;      /**< The #ssm_kind of object this is. */
  union {
    struct {
      uint8_t count; /**< Number of #ssm_value_t values in payload. */
      uint8_t tag;   /**< Which variant is inhabited by this object. */
    } variant;
    struct {
      uint8_t count; /**< Number of #ssm_value_t values in payload. */
      uint8_t cap;   /**< Which variant is inhabited by this object. */
    } vector;
    uint16_t size; /**< 16-bit size */
  } info; /**< Three "flavors" of information embedded in the header. */
};

/** @brief The different kinds of heap objects, enumerated.
 *
 *  Types enumerated here that are not ADTs are chosen because they cannot be
 *  easily or efficiently expressed as a product of words. For instance, 64-bit
 *  timestamps cannot be directly stored in the payload of a regular heap
 *  object, where even-numbered timestamps may be misinterpreted as pointers.
 */
enum ssm_kind {
  SSM_TIME_K = 0, /**< 64-bit timestamps, #ssm_time_t */
  SSM_ADT_K,      /**< ADT object, e.g., #ssm_adt1 */
  SSM_SV_K,       /**< Scheduled variables, #ssm_sv_t */
  SSM_CLOSURE_K,  /**< Closure object, e.g., #ssm_closure1 */
  SSM_ARRAY_K,    /**< Array of values, e.g., #ssm_array1 */
  SSM_BLOB_K,     /**< Blob of arbitrary data, e.g., #ssm_blob1 */
};

/** @brief Construct an #ssm_value_t from a 31-bit integral value.
 *
 *  @param v  the 31-bit integral value.
 *  @return   a packed #ssm_value_t.
 */
#define ssm_marshal(v)                                                         \
  (ssm_value_t) { .packed_val = ((v) << 1 | 1) }

/** @brief Extract an integral value from a packed #ssm_value_t.
 *
 *  @param v  the packed #ssm_value_t.
 *  @return   the raw 31-bit integral value.
 */
#define ssm_unmarshal(v) ((v).packed_val >> 1)

/** @brief Whether a value is on the heap (i.e., is an object).
 *
 *  @param v  pointer to the #ssm_value_t.
 *  @returns  non-zero if on heap, zero otherwise.
 */
#define ssm_on_heap(v) (((v).packed_val & 0x1) == 0)

/** @brief Whether a value is shared, i.e., unsafe to modify.
 *
 *  The opposite of "shared" is "unique," as in C++'s "unique_ptr."
 *
 *  @param v  pointer to the #ssm_value_t.
 *  @returns  non-zero if shared, zero otherwise.
 */
#define ssm_is_shared(v) !(ssm_on_heap(v) && ((v).heap_ptr->ref_count == 1))

/** @brief Duplicate a possible heap reference, incrementing its ref count.
 *
 *  If the caller knows @a v is definitely on the heap, call ssm_drop_unsafe()
 *  to eliminate the heap check (and omit call if it is definitely not on the
 *  heap).
 *
 *  @param v  pointer to the #ssm_mm header of the heap item.
 */
#define ssm_dup(v) (ssm_on_heap(v) ? ssm_dup_unsafe(v) : (v))

/** @brief Drop a reference to a possible heap item, and free it if necessary.
 *
 *  If @a v is freed, all references held by the heap item itself will also be
 *  be dropped.
 *
 *  If the caller knows that @a v is definitely a heap item, call
 *  ssm_drop_unsafe() to eliminate the heap check (and omit call if it is
 *  definitely not on the heap).
 *
 *  @param v  #ssm_value_t to be dropped.
 */
#define ssm_drop(v)                                                            \
  do                                                                           \
    if (ssm_on_heap(v))                                                        \
      ssm_drop_unsafe(v);                                                      \
  while (0)

/** @brief Duplicate a heap reference, incrementing its ref count.
 *
 *  Called by ssm_dup().
 *
 *  @note assumes that @a v is a heap pointer, i.e., `ssm_on_heap(v)`.
 *
 *  @param v  pointer to the #ssm_mm header of the heap item.
 */
#define ssm_dup_unsafe(v) ((++(v).heap_ptr->ref_count, (v)))

/** @brief Drop a reference to a heap item, and free it if necessary.
 *
 *  If @a v is freed, ssm_drop_final() will be called to drop all heap objects
 *  @a v refers to.
 *
 *  Called by ssm_drop().
 *
 *  @note assumes that @a v is a heap pointer, i.e., `ssm_on_heap(v)`.
 *
 *  @param v  #ssm_value_t to be dropped.
 *  @returns  0 on success.
 */
#define ssm_drop_unsafe(v)                                                     \
  do                                                                           \
    if (--(v).heap_ptr->ref_count == 0)                                        \
      ssm_drop_final(v);                                                       \
  while (0)

/** @brief Finalize and free a heap object.
 *
 *  Drops all heap objects @a v refers to, and perform any additional
 *  finalization as required by the #ssm_kind of @a v.
 *
 *  Called by ssm_drop_unsafe().
 *
 *  @note assumes that @a v is a heap pointer, i.e., `ssm_on_heap(v)`.
 *
 *  @param v  #ssm_value_t to be finalized and freed.
 */
void ssm_drop_final(ssm_value_t v);

/** @brief Call ssm_dup() on an array of values. */
void ssm_dups(size_t cnt, ssm_value_t *arr);

/** @brief Call ssm_drop() on an array of values. */
void ssm_drops(size_t cnt, ssm_value_t *arr);

/** @} */

/**
 * @addtogroup time
 * @{
 */

/** @brief Absolute time; never to overflow. */
typedef uint64_t ssm_time_t;

/** @brief Heap-allocated time values.
 *
 *  These should never be declared on their own, and should only be allocated on
 *  the heap using ssm_new_time().
 *
 *  @invariant for all `struct ssm_time t`, `t.mm.kind == SSM_TIME_K`.
 */
struct ssm_time {
  struct ssm_mm mm; /**< Embedded memory management header. */
  ssm_time_t time;  /**< Time value payload. */
};

/** @brief Time indicating something will never happen.
 *
 *  The value of this must be derived from the underlying type of #ssm_time_t.
 */
#define SSM_NEVER UINT64_MAX

/** Ticks per nanosecond */
#define SSM_NANOSECOND 1L
/** Ticks per microsecond */
#define SSM_MICROSECOND (SSM_NANOSECOND * 1000L)
/** Ticks per millisecond */
#define SSM_MILLISECOND (SSM_MICROSECOND * 1000L)
/** Ticks per second */
#define SSM_SECOND (SSM_MILLISECOND * 1000L)
/** Ticks per minute */
#define SSM_MINUTE (SSM_SECOND * 60L)
/** Ticks per hour */
#define SSM_HOUR (SSM_MINUTE * 60L)

/** @brief The current model time.
 *
 *  @returns the current model time.
 */
ssm_time_t ssm_now(void);

/** @brief Allocate a #ssm_time on the heap.
 *
 *  @param time what the heap-allocated @a time field is initialized to.
 *  @returns    #ssm_value_t pointing to the heap-allocated #ssm_time.
 *
 *  ssm_value_t ssm_new_time(ssm_time_t time)
 */
extern ssm_value_t ssm_new_time_int(ssm_time_t time);
#ifdef CONFIG_MEM_TRACE
#define ssm_new_time(t)                                                        \
  (fprintf(stderr, "%s:%d:ssm_new_time()\n", __FILE__, __LINE__),              \
   ssm_new_time_int(t))
#else
#define ssm_new_time(t) ssm_new_time_int(t)
#endif

/** @brief Read the heap-allocated time pointed to by an #ssm_value_t.
 *
 *  @note The behavior of using this macro on an #ssm_value_t that does not
 *        point to an #ssm_time is undefined.
 *  @note The behavior of using the result of this macro as an l-value is
 *        undefined.
 *
 *  @param v  the #ssm_value_t
 *  @returns  the #ssm_time_t in the heap.
 */
#define ssm_time_read(v) (container_of((v).heap_ptr, struct ssm_time, mm)->time)

/** @} */

/**
 * @addtogroup adt
 * @{
 */

/** @brief The struct template of a heap-allocated ADT object.
 *
 *  This ADT struct is meant to be used as a template for performing other
 *  ADT-related pointer arithmetic; no instance of this should ever be declared.
 *
 *  Though this struct's @a fields is only declared with 1 #ssm_value_t, actual
 *  heap-allocated ADT objects may have more fields. For instance, an object
 *  with 3 fields might look like:
 *
 *  ~~~{.c}
 *  struct ssm_adt3 {
 *    struct ssm_mm mm;
 *    ssm_value_t fields[3];
 *  };
 *  ~~~
 *
 *  Note that the memory layout of all ADTs is the same save for the length of
 *  the @a fields, so we use this struct definition as the "base case" of ADT
 *  object lengths.
 */
struct ssm_adt1 {
  struct ssm_mm mm;      /**< Variant-flavored memory management header. */
  ssm_value_t fields[1]; /**< Array of ADT object fields. */
};

/** @brief Allocate a new ADT object on the heap.
 *
 *  @note This function fully initializes the #ssm_mm header of the ADT object,
 *        but leaves its fields uninitialized. It is the responsibility of the
 *        caller to properly <em>all</em> @a val_count fields.
 *
 *  @param field_count  the number of fields in the ADT object.
 *  @param tag          the tag of the ADT object, stored in the #ssm_mm header.
 *  @returns            #ssm_value_t poining to the ADT object on the heap.
 */
extern ssm_value_t ssm_new_adt_int(uint8_t field_count, uint8_t tag);

#ifdef CONFIG_MEM_TRACE
#define ssm_new_adt(fc, tag)                                                   \
  (fprintf(stderr, "%s:%d:ssm_new_adt(%d, %d)\n", __FILE__, __LINE__, (fc),    \
           (tag)),                                                             \
   ssm_new_adt_int((fc), (tag)))
#else
#define ssm_new_adt(fc, tag) ssm_new_adt_int((fc), (tag))
#endif

/** @brief Access the field of an ADT object.
 *
 *  The result of this macro can also be used as an l-value, i.e., it may be
 *  assigned to, e.g.,:
 *
 *  ~~~{.c}
 *  ssm_adt_field(v, 0) = ssm_marshal(1);
 *  ~~~
 *
 *  @note The behavior of using this macro on an #ssm_value_t that does not
 *        point to an ADT object of sufficient size (@a val_count greater than
 *        @a i) is undefined.
 *
 *  @param v  the #ssm_value_t pointing to a heap-allocated ADT object.
 *  @param i  the 0-base index of the field in @a v to be accessed.
 *  @returns  the @a i'th field of @a v.
 */
#define ssm_adt_field(v, i)                                                    \
  (&*container_of((v).heap_ptr, struct ssm_adt1, mm)->fields)[i]

/** @brief Retrieve the tag of an ADT object.
 *
 *  @note The behavior of using this macro on an #ssm_value_t that points to
 *        anything other than an ADT object is undefined.
 *
 *  @param v  the #ssm_value_t whose tag is being retrieved.
 *  @returns  the tag of @a v.
 */
#define ssm_tag(v)                                                             \
  (ssm_on_heap(v) ? (v).heap_ptr->info.variant.tag : ssm_unmarshal(v))

/** @brief Obtain number of fields in the ADT pointed by @a v. */
#define ssm_adt_field_count(v) ((v).heap_ptr->info.variant.count)

/** @brief Compute the size of a heap-allocated ADT.
 *
 *  @param val_count  the @a val_count field of the ADT object's #ssm_mm header.
 *  @returns          the size of the ADT object in the heap.
 */
#define ssm_adt_size(val_count)                                                \
  (sizeof(struct ssm_adt1) + sizeof(ssm_value_t) * ((val_count)-1))

/** @brief Compute the size of an ADT already allocated on the heap. */
#define ssm_adt_heap_size(v) ssm_adt_size(ssm_adt_field_count(v))

/** @} */

/**
 * @addtogroup sv
 * @{
 */

/** @brief A scheduled variable that supports scheduled updates with triggers.
 *
 *  Scheduled variables are heap-allocated built-in types that represent
 *  variables with reference-like semantics in SSM. These should be always
 *  allocated on the heap using ssm_new_sv().
 *
 *  Routines may directly assign to them in the current instant (ssm_assign()),
 *  or schedule a delayed assignment to them (ssm_later()). The @a last_updated
 *  time is recorded in each case, sensitive routines, i.e., @a triggers, are
 *  woken up.
 *
 *  At most one delayed assignment may be scheduled at a time, but a single
 *  update may wake any number of sensitive routines.
 *
 *  @invariant @a later_time != #SSM_NEVER iff this variable in the event queue.
 *  @invariant for all `ssm_sv_t v`, `v.mm.kind == SSM_SV_K`.
 */
typedef struct ssm_sv {
  struct ssm_mm mm;
  ssm_time_t later_time;   /**< When the variable should be next updated. */
  ssm_time_t last_updated; /**< When the variable was last updated. */
  ssm_trigger_t *triggers; /**< List of sensitive continuations. */
  ssm_value_t value;       /**< Current value. */
  ssm_value_t later_value; /**< Buffered value for delayed assignment. */
} ssm_sv_t;

/** @brief Allocate an #ssm_sv on the heap.
 *
 *  @note Initialization does not count as an update event; @a last_updated is
 *        initialized to #SSM_NEVER.
 *  @note The @a later_value field is left uninitialized.
 *
 *  @param val  the initial value of the scheduled variable.
 *  @returns    #ssm_value_t pointing to the #ssm_sv on the heap.
 */
extern ssm_value_t ssm_new_sv_int(ssm_value_t val);
#ifdef CONFIG_MEM_TRACE
#define ssm_new_sv(v)                                                          \
  (fprintf(stderr, "%s:%d:ssm_new_sv()\n", __FILE__, __LINE__),                \
   ssm_new_sv_int(v))
#else
#define ssm_new_sv(v) ssm_new_sv_int(v)
#endif

/** @brief Retrieve #ssm_sv pointer pointed to by an #ssm_value_t.
 *
 *  @note The behavior of using this macro on an #ssm_value_t that does not
 *        point to a scheduled variable is undefined.
 *
 *  @param val  the #ssm_value_t
 *  @returns    pointer to the #ssm_sv_t
 */
#define ssm_to_sv(val) container_of((val).heap_ptr, ssm_sv_t, mm)

/** @brief Read the value of a scheduled variable.
 *
 *  @note The behavior of using this macro on an #ssm_value_t that does not
 *        point to a scheduled variable is undefined.
 *  @note The behavior of using the result of this macro as an l-value is
 *        undefined. To assign to a scheduled variable, use ssm_assign() or
 *        ssm_later().
 *
 *  @param val  #ssm_value_t that points to a scheduled variable.
 *  @returns    the value that @a val points to.
 */
#define ssm_deref(val) (ssm_to_sv(val)->value)

/** @brief Instantaneous assignment to a scheduled variable.
 *
 *  Updates the value of @a var in the current instant, and wakes up all
 *  sensitive processes at a lower priority than the calling routine.
 *
 *  ssm_assign() will ssm_drop() the previous value of @a var and ssm_dup() the
 *  new @a val. If the caller already knows whether @a var and @a val reside on
 *  the heap, they may call ssm_assign_unsafe() to forego the ssm_drop() and
 *  ssm_dup() calls, but will be responsible for reference counting themselves
 *  (i.e., calling ssm_dup_unsafe() and ssm_drop_unsafe() for heap objects).
 *
 *  @note Does not overwrite a scheduled assignment.
 *  @note The behavior of this macro when @a var does not point to a scheduled
 *        variable is undefined.
 *
 *  @param var  pointer to the scheduled variable.
 *  @param prio priority of the calling routine.
 *  @param val  the value to be assigned to @a var.
 */
#define ssm_assign(var, prio, val)                                             \
  do {                                                                         \
    ssm_dup(val);                                                              \
    ssm_drop(ssm_deref(var));                                                  \
    ssm_sv_assign_unsafe(ssm_to_sv(var), prio, val);                           \
  } while (0)

/** @brief Delayed assignment to a scheduled variable.
 *
 *  Schedules a delayed assignment to @a var at a later time.
 *
 *  ssm_later() will ssm_dup() the scheduled @a val. If the caller already
 *  knows whether @a val resides on the heap, they may call ssm_later_unsafe()
 *  to forego the ssm_dup(), but will be responsible for reference counting
 *  themselves (i.e., calling ssm_dup_unsafe() for heap objects).
 *
 *  Overwrites any previously scheduled update, if any.
 *
 *  @note The behavior of this macro when @a var does not point to a scheduled
 *        variable is undefined.
 *
 *  @param var  pointer to the scheduled variable.
 *  @param when the time when the update should take place.
 *  @param val  the value to be assigned to @a var.
 *
 *  @throws SSM_INVALID_TIME          @a later is greater than ssm_now().
 *  @throws SSM_EXHAUSTED_EVENT_QUEUE event queue ran out of space.
 */
#define ssm_later(var, when, val)                                              \
  do {                                                                         \
    ssm_dup(val);                                                              \
    ssm_sv_later_unsafe(ssm_to_sv(var), when, val);                            \
  } while (0)

/** @brief ssm_assign() without automatic reference counting.
 *
 *  @note Does not overwrite a scheduled assignment.
 *
 *  @param var  pointer to the scheduled variable.
 *  @param prio priority of the calling routine.
 *  @param val  the value to be assigned to @a var.
 *
 *  @sa ssm_assign().
 */
void ssm_sv_assign_unsafe(ssm_sv_t *var, ssm_priority_t prio, ssm_value_t val);

/** @brief ssm_later() without automatic reference counting.
 *
 *  Overwrites any previously scheduled update, if ssm_laterr
 *
 *  @note ssm_drop() <em>will</em> be called on the previous @a later_value,
 *        so the caller is not responsible for checking that.
 *
 *  @param var  pointer to the scheduled variable.
 *  @param when the time when the update should take place.
 *  @param val  the value to be assigned to @a var.
 *
 *  @throws SSM_INVALID_TIME          @a later is greater than ssm_now().
 *  @throws SSM_EXHAUSTED_EVENT_QUEUE event queue ran out of space.
 *
 *  @sa ssm_later().
 */
void ssm_sv_later_unsafe(ssm_sv_t *var, ssm_time_t when, ssm_value_t val);

/** @brief Sensitize a trigger to a scheduled variable.
 *
 *  Macro provided for convenience.
 *
 *  @sa ssm_sv_sensitize().
 *
 *  @param var  #ssm_value_t pointing to a scheduled variable.
 *  @param trig  trigger to be registered on @a var.
 */
#define ssm_sensitize(var, trig) ssm_sv_sensitize(ssm_to_sv(var), trig)

/** @brief Desensitize a trigger.
 *
 *  Macro provided for convenience.
 *
 *  @sa ssm_sv_desensitize().
 *
 *  @param trig  the trigger.
 */
#define ssm_desensitize(trig) ssm_sv_desensitize(trig)

/** @brief Sensitize a variable to a trigger.
 *
 *  Adds a trigger to a scheduled variable, so that @a trig's activation
 *  record is awoken when the variable is updated.
 *
 *  This function should be called by a routine before sleeping (yielding).
 *
 *  @param var  pointer to the scheduled variable.
 *  @param trig trigger to be registered on @a var.
 */
void ssm_sv_sensitize(ssm_sv_t *var, ssm_trigger_t *trig);

/** @brief Desensitize a variable from a trigger.
 *
 *  Remove a trigger from its variable.
 *
 *  This function should be called by a routine after returning from sleeping.
 *
 *  @param trig the trigger.
 */
void ssm_sv_desensitize(ssm_trigger_t *trig);

/** @} */

/**
 * @addtogroup closure
 * @{
 */

/** @brief The type signature for all SSM enter functions.
 *
 *  This type standardizes the "calling convention" of SSM functions, so that
 *  they can be stored in generic closures and treated uniformly.
 *
 *  @param parent the activation record of the caller.
 *  @param prio   the priority of the callee.
 *  @param depth  the depth of the callee.
 *  @param argv   an array of #ssm_value_t arguments given to the callee.
 *  @param ret    the return value address that the callee should write to.
 *  @returns      an allocated activation record for the callee.
 */
typedef ssm_act_t *(*ssm_func_t)(ssm_act_t *parent, ssm_priority_t prio,
                                 ssm_depth_t depth, ssm_value_t *argv,
                                 ssm_value_t *ret);

/** @brief The struct template of a heap-allocated closure object.
 *
 *  This struct is meant to be used as a template for performing other
 *  closure-related pointer arithmetic; no instance of this should ever be
 *  declared. See elaborated discussion about this pattern for #ssm_adt1.
 *
 *  A closure should always be allocated with enough space in @a argv to
 *  accommodate <em>all</em> arguments expected by the enter function @a f,
 *  so faciliate efficient in-place usage.
 *
 *  The current number of arguments owned by the closure is tracked by the
 *  memory management header's @a val_count field; the reference counts of the
 *  @a argv fields within this range need to be decremented when the enclosing
 *  closure is freed. The total number of arguments accommodated by the closure
 *  is tracked by the @a tag field; this also determines the size of the closure
 *  object.
 */
struct ssm_closure1 {
  struct ssm_mm mm;    /**< Memory management header. */
  ssm_func_t f;        /**< Enter function pointer. */
  ssm_value_t argv[1]; /**< An array of arguments. */
};

/** @brief Obtain the number of argument values owned by a closure. */
#define ssm_closure_arg_count(v)                                               \
  (container_of((v).heap_ptr, struct ssm_closure1, mm)->mm.info.vector.count)

/** @brief Obtain the number of argument values accommodated by a closure. */
#define ssm_closure_arg_cap(v)                                                 \
  (container_of((v).heap_ptr, struct ssm_closure1, mm)->mm.info.vector.cap)

/** @brief Obtain the enter function pointer of a closure. */
#define ssm_closure_func(v)                                                    \
  (container_of((v).heap_ptr, struct ssm_closure1, mm)->f)

/** @brief Retrieve the argument array of a closure. */
#define ssm_closure_argv(v)                                                    \
  (&*container_of((v).heap_ptr, struct ssm_closure1, mm)->argv)

/** @brief Obtain the ith argument of a closure. */
#define ssm_closure_arg(v, i) ssm_closure_argv(v)[i]

/** @brief Compute the size of a closure object.
 *
 *  @param val_count  the @a val_count field of the closure's #ssm_mm header.
 *  @returns          the size of the closure object.
 */
#define ssm_closure_size(val_count)                                            \
  (sizeof(struct ssm_closure1) + (sizeof(ssm_value_t) * ((val_count)-1)))

/** @brief Compute the size of a closure already allocated on the heap. */
#define ssm_closure_heap_size(v) ssm_closure_size(ssm_closure_arg_cap(v))

/** @brief Add an argument to a closure.
 *
 *  Note that this helper increments the @a arg_count of @a closure but does not
 *  perform memory management, i.e., does not ssm_dup() @a arg. It also does not
 *  check whether @a closure has capacity for @a arg.
 */
#define ssm_closure_push(closure, arg)                                         \
  do                                                                           \
    ssm_closure_arg(closure, ssm_closure_arg_count(closure)++) = (arg);        \
  while (0)

/** @brief Remove an argument from a closure.
 *
 *  Note that this helper decrements the @a arg_count of @a closure but does not
 *  perform memory management, i.e., does not ssm_drop() @a arg. It also does
 *  not check whether @a closure has a non-zero number of arguments already
 *  applied.
 */
#define ssm_closure_pop(closure)                                               \
  do                                                                           \
    ssm_closure_arg_count(closure)--;                                          \
  while (0)

/** @brief Spawn and schedule a new child process from a fully-applied closure.
 *
 *  Note that this helper does not perform any memory management, nor does it
 *  ensure that @a closure has all arguments applied.
 */
#define ssm_closure_activate(closure, parent, prio, depth, ret)                \
  ssm_activate(ssm_closure_func(closure)(parent, prio, depth,                  \
                                         ssm_closure_argv(closure), ret))

/** @brief Allocate a closure on the heap.
 *
 *  @param f        the enter function pointer of the closure.
 *  @param arg_cap  the number of arguments the closure should accommodate.
 *  @returns        a value pointing to the heap-allocated closure.
 */
extern ssm_value_t ssm_new_closure_int(ssm_func_t f, uint8_t arg_cap);
#ifdef CONFIG_MEM_TRACE
#define ssm_new_closure(f, args)                                               \
  (fprintf(stderr, "%s:%d:ssm_new_closure(_, %d)\n", __FILE__, __LINE__,       \
           (args)),                                                            \
   ssm_new_closure_int((f), (args)))
#else
#define ssm_new_closure(f, args) ssm_new_closure_int((f), (args))
#endif

/** @brief Create a copy of a closure.
 *
 *  @note Calls ssm_dup() on all previously applied arguments.
 *
 *  @param closure  the closure to be copied.
 *  @returns        a value pointing to the heap-allocated closure.
 */
ssm_value_t ssm_closure_clone(ssm_value_t closure);

/** @brief Apply an argument to a closure.
 *
 *  The behavior depends on whether @a closure needs to be activated after being
 *  applied to @a arg, i.e., whether all of its arguments have arrived.
 *
 *  If @a closure is activated, a child process will be scheduled, so the caller
 *  must yield to the scheduler to allow the child process to run. The child
 *  process will write its return value to @a ret.
 *
 *  If @a closure is not activated, a copy of it (via ssm_closure_clone()) with
 *  @a arg pushed to its @a argv will be written to @a ret.
 *
 *  In both cases, all values in @a closure's @a argv will have ssm_dup() called
 *  on them, since application will result in those values being shared in a new
 *  context, either from a child process or from the cloned closure. Note that
 *  ssm_dup() is <em>not</em> called on @a arg; to do so automatically, use
 *  ssm_closure_apply_auto().
 *
 *  @param closure  the closure to be applied.
 *  @param arg      the argument @a closure is applied to.
 *  @param parent   the current process performing the application.
 *  @param prio     priority of the current process.
 *  @param depth    depth of the current process.
 *  @param ret      pointer to the return value.
 */
void ssm_closure_apply(ssm_value_t closure, ssm_value_t arg, ssm_act_t *parent,
                       ssm_priority_t prio, ssm_depth_t depth,
                       ssm_value_t *ret);

/** @brief Apply an argument to a closure that is used for the last time.
 *
 *  An optimized version of ssm_closure_apply(), to be used only provided:
 *
 *  1.  this is the last time @a closure is used by the caller.
 *  2.  no other processes share @a closure.
 *
 *  This implementation should be used as the fast path when those conditions
 *  hold; it improves upon the more general ssm_closure_apply() by using @a
 *  closure in-place, and avoiding unnecessary ssm_dup() and ssm_drop() called.
 *  After this is called, @a closure will not need to be dropped.
 *
 *  The behavior of this function is equivalent to:
 *
 *  ```{.c}
 *  ssm_closure_apply(f, a, p, prio, depth, ret);
 *  ssm_drop(f);
 *  ```
 *
 *  Like ssm_closure_apply(), this function does not ssm_drop() @a arg; to do so
 *  automatically, use ssm_closure_apply_final_auto().
 *
 *  @param closure  the closure to be applied.
 *  @param arg      the argument @a closure is applied to.
 *  @param parent   the current process performing the application.
 *  @param prio     priority of the current process.
 *  @param depth    depth of the current process.
 *  @param ret      pointer to the return value.
 */
void ssm_closure_apply_final(ssm_value_t closure, ssm_value_t arg,
                             ssm_act_t *parent, ssm_priority_t prio,
                             ssm_depth_t depth, ssm_value_t *ret);

/** @brief Closure application with automatic memory management.
 *
 *  ssm_dup() is called on @a a before @a f is applied to it;
 *  see ssm_closure_apply().
 *
 *  @param closure  the closure to be applied.
 *  @param arg      the argument @a closure is applied to.
 *  @param parent   the current process performing the application.
 *  @param prio     priority of the current process.
 *  @param depth    depth of the current process.
 *  @param ret      pointer to the return value.
 */
#define ssm_closure_apply_auto(closure, arg, parent, prio, depth, ret)         \
  do {                                                                         \
    ssm_dup(arg);                                                              \
    ssm_closure_apply(closure, arg, parent, prio, depth, ret);                 \
  } while (0)

/** @brief In-place closure application with automatic memory management.
 *
 *  ssm_dup() is called on @a a before @a f is applied to it;
 *  see ssm_closure_apply_final().
 *
 *  @param closure  the closure to be applied.
 *  @param arg      the argument @a closure is applied to.
 *  @param parent   the current process performing the application.
 *  @param prio     priority of the current process.
 *  @param depth    depth of the current process.
 *  @param ret      pointer to the return value.
 */
#define ssm_closure_apply_final_auto(closure, arg, parent, prio, depth, ret)   \
  do {                                                                         \
    ssm_dup(arg);                                                              \
    ssm_closure_apply_final(closure, arg, parent, prio, depth, ret);           \
  } while (0)

/** @brief Helper to free a closure (without reference counting). */
#define ssm_closure_free(closure)                                              \
  ssm_mem_free((closure).heap_ptr,                                             \
               ssm_closure_size(ssm_closure_arg_cap(closure)))

/** @} */

/**
 * @addtogroup array
 * @{
 */

/** @brief The struct template of a heap-allocated array of values.
 *
 */
struct ssm_array1 {
  struct ssm_mm mm;        /**< Size-flavored memory management header. */
  ssm_value_t elements[1]; /**< Elements of the heap-allocated array. */
};

/** @brief The length of an array pointed by @a v. */
#define ssm_array_len(v) ((v).heap_ptr->info.size)

/** @brief Obtain pointer to the array elements payload pointed to by @ a v. */
#define ssm_array_elements(v)                                                  \
  (&*(container_of((v).heap_ptr, struct ssm_array1, mm)->elements))

/** @brief Obtain pointer to the ith element of the array pointed by @a v. */
#define ssm_array_element(v, i) (ssm_array_elements(v)[i])

/** @brief Compute the size of an array with its header.
 *
 *  @param count  the number of array elements, i.e., the @a size field.
 *  @returns      size that a array of @a count elemtns occupies in the heap.
 */
#define ssm_array_size(count)                                                  \
  (sizeof(struct ssm_array1) + sizeof(ssm_value_t) * ((count)-1))

/** @brief Compute the size an array in the heap from an #ssm_value_t.
 *
 *  @param v  #ssm_value_t pointing to some blob in the heap.
 *  @returns  size of the array that @a v points to.
 */
#define ssm_array_heap_size(v) ssm_array_size(ssm_array_len(v))

/** @brief Allocate an array on the heap.
 *
 *  Note that this function returns an array with all elements uninitialized,
 *  which must be initialized before ssm_drop() can be called on the array.
 *
 *  @param count  number of #ssm_value_t elements to be stored in the array.
 *  @returns      #ssm_value_t pointing to heap-allocated array.
 */

extern ssm_value_t ssm_new_array_int(uint16_t count);
#ifdef CONFIG_MEM_TRACE
#define ssm_new_array(c)                                                       \
  (fprintf(stderr,"%s:%d:ssm_new_array(%d)\n", \
	   __FILE__, __LINE__, (c)),		     \
   ssm_new_array_int(c)
#else
#define ssm_new_array(c) ssm_new_array_int(c)
#endif

/** @} */

/**
 * @addtogroup blob
 * @{
 */

/** @brief The @a size resolution for heap-allocated blobs. */
#define SSM_BLOB_SIZE_SCALE 4

/** @brief The struct template of a heap-allocated blob.
 *
 *  Blobs are just arbitrary chunks of memory in the heap where any kind of
 * data can be stored, with any layout. Since the memory manager will not scan
 * blobs for pointers, any resources maintained within blobs must be managed
 * via other means.
 *
 *  Though this struct's @a payload is only declared with 4 bytes, actual
 *  heap-allocated blobs may have large payloads. For instance, a 48-byte blob
 *  might look like:
 *
 *  ~~~{.c}
 *  struct ssm_blob48 {
 *    struct ssm_mm mm;
 *    char payload[48 / SSM_BLOB_SIZE_SCALE];
 *  };
 *  ~~~
 *
 *  The memory layout of all blobs is the same save for the size of the
 *  @a payload, so we use this struct definition as the "base case" of blobs.
 *
 *  To allow even larger blobs, the actual size of the payload is divided by
 *  #SSM_BLOB_SIZE_SCALE while stored in the @a size field.
 */
struct ssm_blob1 {
  struct ssm_mm mm; /**< Size-flavored memory management header. */
  char payload[SSM_BLOB_SIZE_SCALE]; /**< Payload of heap-allocated blob. */
};

/** @brief Compute the size of a blob with its header.
 *
 *  The @a size parameter should already be scaled, i.e., already multiplied by
 *  #SSM_BLOB_SIZE_SCALE, before being passed into this macro.
 *
 *  @param size   scaled size of the blob's payload.
 *  @returns      size that a blob of @a size payload occupies in the heap.
 */
#define ssm_blob_size(size)                                                    \
  (sizeof(struct ssm_blob1) - SSM_BLOB_SIZE_SCALE + (size))

/** @brief Compute the size a blob in the heap.
 *
 *  @param v  #ssm_value_t pointing to some blob in the heap.
 *  @returns  size of the blob that @a v points to.
 */
#define ssm_blob_heap_size(v)                                                  \
  ssm_blob_size(((v).heap_ptr->info.size) * SSM_BLOB_SIZE_SCALE)

/** @brief Obtain pointer to the payload of a blob from an #ssm_value_t. */
#define ssm_blob_payload(v)                                                    \
  (&*(container_of((v).heap_ptr, struct ssm_blob1, mm)->payload))

/** @brief Allocate a blob on the heap.
 *
 *  @param size   size of the payload to the allocated.
 *  @returns      #ssm_value_t pointing to heap-allocated blob.
 */
extern ssm_value_t ssm_new_blob_int(uint16_t size);
#ifdef CONFIG_MEM_TRACE
#define ssm_new_blob(s)                                                        \
  (fprintf(stderr, "%s:%d:ssm_new_blob(%lu)\n", __FILE__, __LINE__, (size)),   \
   ssm_new_blob_int(size))
#else
#define ssm_new_blob(s) ssm_new_blob_int(s)
#endif

/** @} */

/**
 * @addtogroup mem
 * @{
 */

/** @brief Allocate a contiguous range of memory.
 *
 *  Memory will be allocated from an appropriately sized memory pool, if one
 *  is available. Guaranteed to be aligned against the smallest power of 2
 *  greater than @a size.
 *
 *  @param size   the requested memory range size, in bytes.
 *  @returns      pointer to the first byte of the allocate memory block.
 */
void *ssm_mem_alloc(size_t size);

/** @brief Preallocate memory pages to ensure capacity in memory pools.
 *
 *  Does nothing if no memory pool will fit a block of @a size.
 *
 *  @param size       size whose memory pool should be preallocaed pages.
 *  @param num_pages  number of pages to allocate.
 */
void ssm_mem_prealloc(size_t size, size_t num_pages);

/** @brief Deallocate memory allocated by ssm_mem_alloc().
 *
 *  The behavior of freeing memory not allocated by ssm_mem_alloc() is
 *  undefined.
 *
 *  @param m      pointer to the memory range allocated by ssm_mem_alloc().
 *  @param size   the size of the memory range allocated by ssm_mem_alloc().
 */
void ssm_mem_free(void *m, size_t size);

/** @} */

/** @ingroup util
 *  @def member_type
 *  @brief Obtain the type of a struct member.
 *
 *  Intended for use in #container_of, for type safety.
 *
 *  When GNU C is not available, falls back to ISO C99 and returns `const void`
 *  (see https://stackoverflow.com/a/10269925/10497710).
 *
 *  For instance, given the following struct definition:
 *
 *  ~~~{.c}
 *  struct foo { int bar; float baz; };
 *  ~~~
 *
 *  `member_type(struct foo, baz)` expands to `float`.
 *
 *  @param type   the struct type.
 *  @param member the name of the member in @a type.
 *  @returns      the type of @a member in @a type.
 */
#ifdef __GNUC__
#define member_type(type, member) __typeof__(((type *)0)->member)
#else
#define member_type(type, member) const void
#endif

/** @ingroup util
 *  @brief Obtain the pointer to an outer, enclosing struct.
 *
 *  For example, with the following struct definition:
 *
 *  ~~~{.c}
 *  struct foo { int bar; float baz; };
 *  ~~~
 *
 *  Given some `float *p`, `container_of(p, struct foo, baz)` will return
 *  a pointer to the enclosing `struct foo`. The caller is responsible for
 *  ensuring that such an enclosing pointer actually exists.
 *
 *  Use this macro instead of pointer casting or pointer arithmetic; this macro
 *  is more principled in what it does and, with GNU C support, will trigger
 *  compiler warnings when @a ptr and the @a member type do not agree.
 *
 *  Adapted from the Linux kernel source
 *  (see https://stackoverflow.com/a/10269925/10497710).
 *
 *  @param ptr    pointer to member.
 *  @param type   type of enclosing struct.
 *  @param member name of member in enclosing struct.
 *  @returns      pointer to enclosing struct.
 */
#define container_of(ptr, type, member)                                        \
  ((type *)((char *)(member_type(type, member) *){ptr} -                       \
            offsetof(type, member)))

#endif
