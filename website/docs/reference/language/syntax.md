---
title: Syntax
---

This page summarizes the source-level syntax that all Lutra programs use.
Use it when you need to confirm how names, files, paths, and punctuation work.

## Files and top-level structure

A Lutra source file contains a sequence of top-level definitions.
In practice, these are usually `const`, `func`, `type`, `import`, and `module`
definitions.

```lt
import std::cmp

type Status: enum {open, closed}

const greeting = "hello"

func main() -> cmp(1: int32, 2)
```

## Comments

Line comments start with `#` and continue to the end of the line.

```lt
# This is a comment.
const main = 1: int32
```

## Identifiers

Identifiers name values, functions, modules, types, fields, and enum variants.

```lt
const album_count = 10: int32
func add_one(x: int32) -> x + 1
```

Lutra also supports qualified paths with `::`.

```lt
std::option::or_default
std::date::to_timestamp
project::pg::from_albums
```

## Delimiters and separators

Lutra uses these delimiters:

- `(` and `)` for grouping, function parameters, and function calls.
- `{` and `}` for tuples, tuple types, and module bodies.
- `[` and `]` for arrays and array types.

It uses these separators and punctuation marks:

- `,` between items.
- `:` for type annotations and type definitions.
- `->` for function bodies.
- `=>` for match branches.
- `.` for field access.
- `|` for pipelines and alternative patterns.
- `..` for tuple spread.
- `;` between `let` bindings inside a block expression.

## Grouping

Parentheses group expressions and are commonly used for:

- multiline pipelines,
- `if` branches,
- `match` subjects,
- `let` blocks.

```lt
func main() -> (
  [1, 2, 3]: [int32]
  | std::map(x -> x + 1)
)
```

## Source formatting

Whitespace is used for readability, but punctuation determines structure.
For example, these two function definitions are equivalent:

```lt
func add_one(x: int32) -> x + 1
```

```lt
func add_one(x: int32) -> (
  x + 1
)
```

## See also

- [Literals](literals.md)
- [Types](types.md)
- [Expressions](expressions.md)
- [Definitions](definitions.md)
- [Modules](modules.md)
