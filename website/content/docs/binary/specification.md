---
title: Specification
---

Each type has a defined encoding with head and an optional body.

Head has static size.
Body has dynamic size.

Buffer that contains the encoded type, has the its head at the beginning of the buffer.
If there is a body, head contains a pointer to the body.
Head and body might not be adjacent in the buffer - they might be separated by data belonging to other types.

For example:

- `int`, `float` and `bool` only have a head,
- `text` has pointer and length in head and UTF-8 encoded bytes in the body.


## Primitives

- `bool` - 8 bit, only first bit is used,
- `int{n}` - 64 bit integer, little-endian signed,
- `uint{n}` - 64 bit integer, little-endian unsigned,
- `float{n}` - 64 bit float, little-endian IEEE 754,
- `text` - head contains offset & len, body contains the text encoded as UTF8,

## Tuple

Head contains heads of each of the fields. Body is empty.

## Array

Head contains offset of the body (32 bit le)
and length of the array (32 bit le).

Body contains consecutive heads of each of the items
of the array.

## Enum

The selected variant is encoded with a tag
which uses `S` bits,
where `S = ceil(log2(number_of_variants))`.

Then, the size of inner head I is determined:
`H = variants.map(|v| v.size_of_head).max()`

If `H <= 32 bits`, head of the enum contains
the tag followed by the head of the inner value
of the selected variant.
The head of the inner value is padded, such that its
total length is `H`.
The size of the head is `S + H`.

If `H > 32 bits`, head of the enum contains
the tag followed by the pointer to the body.
Body contains the inner value of the selected variant.
The size of the head is `S + 32 bits`.
