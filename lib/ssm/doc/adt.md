See @ref adt.

@defgroup adt Algebraic data types

The SSM runtime supports user-defined algebraic data types (ADTs), sometimes
known as "tagged unions."
An ADT is inhabited by one of several <em>variants</em>;
those variants may also contain #ssm_value_t <em>fields</em>.
For more information, see languages Haskell or OCaml, which also have ADTs.

Each variant is identified by a per-ADT <em>tag</em>.
When an ADT variant does not have any fields, the <em>tag</em> is stored as
a @a packed_val in an #ssm_value_t.
When an ADT variant does contain fields, it is allocated on the heap, where the
tag is stored in the #ssm_mm header in the @a tag field.

The #ssm_mm header's @a val_count field indicates how many fields the variant
has, and thus its memory layout in the heap.
An ADT object with `N` fields has the following layout:

~~~{.c}
struct ssm_adtN {
  struct ssm_mm mm;
  ssm_value_t fields[N];
};
~~~

The "template" for each ADT object's memory layout is defined by #ssm_adt1; ADT
objects with more fields look like #ssm_adt1 with longer @a fields.
