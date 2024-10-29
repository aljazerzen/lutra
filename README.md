# Lutra data encoding

## Head and body

Each type has head and an optional body.
Head has static size.
Body has dynamic size, which is somehow encoded in the head.
For example:
- int, float and bool only have a head,
- text has offset and lenght in head and bytes in the body.

## Primitives

int   - 64 bit integer, little-endian signed
float - 64 bit float, little-endian IEEE 754
bool  - 1 bit
text  - text encoded as UTF8, head has offset & len, body has bytes

## Tuple

Head contains heads of each of the fields. Body is empty.

## Array

Head contains offset of the body (32 bit le)
and length of the array (32 bit le).

## Enum

The selected variant is enocoded with a tag
which uses S bits,
where `S = ceil(log2(number_of_variants))`.

Then, the size of inner head I is determined:
`H = variants.map(|v| v.size_of_head).max()`

If `H + S <= 64 bits`, head of the enum contains
the tag followed by the head of the inner value
of the selected variant. 

If `H + S > 64bits`, head of the enum contains
the tag followed by the offset of the inner value
of the selected variant.