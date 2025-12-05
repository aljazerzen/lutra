---
title: Key language features
---

Basic Types:

- Numeric: `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `float32`, `float64`
- Text: `text`
- Boolean: `bool`
- Arrays: `[ItemType]`
- Tuples: `{field1: Type1, field2: Type2, ...}`
- Enums: `enum {Variant1, Variant2: Type, ...}`

The type system is static and each expression must have a type that is known at compile time.
Values can be grouped into tuples and arrays, and functions can transform and combine these data structures.

Functions:

- Defined using `func (param: Type) -> ReturnValue`
- Can be assigned to variables with `let`

Standard Library:

- Array operations: `map`, `filter`, `flat_map`, `slice`, `sort`
- Aggregations: `min`, `max`, `sum`, `count`, `mean`, `fold`
- Boolean ops: `all`, `any`, `contains`
- Window functions: `lag`, `lead`, `row_number`

Piping:

- Pipe operator `|`, transforms `value | function` into `function(value)`.
- Can chain many functions:
  ```lt
  (5 | func (x) -> [x, x] | filter(func (x) -> x > 3))`
  ```

Pattern Matching:

- Using `match` expressions
- Can destructure enum variants

Modules:

- Code can be organized into modules
- Modules can contain types and functions
- Module members accessed via `::`