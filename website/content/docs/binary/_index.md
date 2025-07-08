---
title: Binary format
weight: 3
---

Lutra binary format is a method of serializing structured data. It can be used to transfer data between programming languages, processes or devices.

Schema of the data structures is defined with the Lutra language. This schema is used to generate code in the desired language.

The format supports partial decoding, i.e. reading only a part of the encoded data structure.

Schema evolution is not supported. In other words, to deserialize a structure, it must use the exact schema it has been serialized with. Any other schema might work, but the format does not provide any guarantees.

Currently supported languages:

- Rust
- Python

## Example

```lt
type Movie: {
  id: int16,
  title: text,
}

let value: Movie = {
  id = 5,
  title = "Gladiator",
}
```

Given the type definition above, the `value` constant would be encoded as:

```
| id    | title                                                  |
| 05 00 | 08 00 00 00 | 09 00 00 00 | 47 6c 61 64 69 61 74 6f 72 |
| int16 | offset      | length      | contents                   |
```
