See @ref array.

@defgroup array Arrays

The SSM runtime supports contiguous arrays of #ssm_value_t. These are managed
similarly to ADT objects (@ref adt), except the #ssm_mm header in arrays use
the 16-bit @a size field, meaning (1) there is no tag, and (2) arrays
may accommodate up to 65536 fields (instead of 256).
