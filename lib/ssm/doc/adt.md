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
When an ADT variant does contain fields, it is allocated on the heap with
a variant-flavored #ssm_mm header, where the tag is stored in the @a tag field.

The @a count field records the number of fields that are in the object, which
are stored in an #ssm_value_t array following the header.
The "template" for each ADT object's memory layout is defined by #ssm_adt1; ADT
objects with more fields look like #ssm_adt1 with longer @a fields.
