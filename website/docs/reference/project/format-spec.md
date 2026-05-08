---
title: Format spec
---

The data format operates on byte arrays.
Each byte array is associated with a type which is not stored in the byte array itself.

For example, array `[0x01, 0x94]` could represent a two-byte integer of type `Prim16`.

## Head and body

The type dictates the encoding of value. Each type has:

- a head, which has a static size, known in advance, and
- an optional body, which has a dynamic size.

Given an array that contains the encoded type, the head is encoded at the beginning of the array.
If there is a body, head contains a pointer to the body.
Head and body might not be adjacent in the buffer - they might be separated by data belonging to other types.

For example:

- `Prim16` only has a head and no body,
- `Array of Prim16` has a head, which contains a pointer to the body and array length, and a body which contains encoded array items.

## Primitives

- `Prim8` - 8 bits as 1 byte in the head,
- `Prim16` - 16 bits as 2 bytes in the head,
- `Prim32` - 32 bits as 4 bytes in the head,
- `Prim64` - 64 bits as 8 bytes in the head,
- `text` - head contains offset & len, body contains the text encoded as UTF8.

## Tuple

Head contains heads of each of the fields. Body is empty.

## Array

Head contains offset of the body (4 bytes, little-endian)
and length of the array (4 bytes, little-endian).

Body contains consecutive heads of each of the items of the array.

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
