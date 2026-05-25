---
title: Binary format
---

Lutra binary format is a method of serializing structured data.
It is used at the runner boundary to encode program inputs and outputs.

Schema of the data structures is defined with the Lutra language.
This schema is used to generate code in the desired language.

<figure markdown="1">
  ![Data flow diagram](./data-format-flow.drawio.svg)
  <figcaption markdown="span">
  Data flow from runners (databases) to target languages.<br/>
  Binary format is the common interface between them.
  </figcaption>
</figure>

The format supports partial decoding, that is, reading only part of the encoded data structure.

Schema evolution is not supported. To deserialize a structure safely, you must use the same schema that was used to serialize it. Other schemas might appear to work, but the format provides no guarantees.

Currently supported languages:

- Rust
- Python

## Example

```lt
type Movie: {
  id: Prim16,
  title: Text,
}

let value: Movie = {
  id = 5,
  title = "Gladiator",
}
```

Given the type definition above, the `value` constant would be encoded as:

```
| id     | title                                                  |
| 05 00  | 08 00 00 00 | 09 00 00 00 | 47 6c 61 64 69 61 74 6f 72 |
| Prim16 | offset      | length      | contents                   |
```
