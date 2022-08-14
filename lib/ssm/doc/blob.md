See @ref blob.

@defgroup blob Blobs

The SSM allocator allows programs to allocate contiguous chunks of
reference-counted memory whose contents are of arbitrary layout.
This object type provides users the flexibility to extend runtime with objects
that the runtime does not natively support, so long as they commit to managing
stored resources themselves, because the garbage collector will not scan the
payload for other managed heap pointers.

Blobs use size-flavored #ssm_mm headers, where the @a size field determines the
size of blob payload. To support even larger blob sizes, the actual size is
divided by #SSM_BLOB_SIZE_SCALE while stored in @a size.

The heap memory layout for blobs are described by #ssm_blob1.
