# Lutra data encoding

## Head and body

Each type has head and an optional body.
Head has static size.
Body has dynamic size.
Buffer that contains a type, has the type's head at the beginning of the buffer.
If there is a body, head contains a pointer to the body.
Head and body might not be adjacent in the buffer - they might be separated by data belonging to other types.

For example:

- `int`, `float` and `bool` only have a head,
- `text` has pointer and length in head and UTF-8 encoded bytes in the body.

## Primitives

bool - 1 bit
int - 64 bit integer, little-endian signed
uint - 64 bit integer, little-endian unsigned
float - 64 bit float, little-endian IEEE 754
text - text encoded as UTF8, head has offset & len, body has bytes

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
