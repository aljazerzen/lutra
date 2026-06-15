---
title: Types
---

This page describes the type forms used in the Lutra language.

## Standard types

The standard library defines these framed standard types:

- `Bool`
- `Int8`, `Int16`, `Int32`, `Int64`
- `Uint8`, `Uint16`, `Uint32`, `Uint64`
- `Float32`, `Float64`
- `Text`

Examples:

```lt
const enabled: Bool = true
const count: Int32 = 4
const ratio: Float64 = 1.5
const name: Text = "lutra"
```

## Array types

Array types use `[T]`.

```lt
[Int64]
[Text]
[{a: Int64, b: Text}]
```

An array contains zero or more values of the same element type.

## Tuple types

Tuple types use `{...}`.
A tuple field can be positional or named.

```lt
{Int8, Text, [Bool]}
{a: Int64, b: Text}
{sum: Int64, count: Int64}
```

You can access tuple fields by position or by name. See
[Expressions](expressions.md).

## Enum types

Enum types use `enum {...}`.
Each variant has a name and can optionally carry a payload.

```lt
enum {done, pending: Int64, cancelled: Text}
```

Nested enums are also valid:

```lt
enum {
  cat: Text,
  dog: enum {collie: Text, generic},
}
```

## Framed types

A type definition can also introduce a framed type with a single wrapped field.
The standard library uses this form for framed standard types such as
`Bool`, `Int32`, `Text`, `Date`, `Time`, `Duration`, `Timestamp`, and
`Decimal`.

Conceptually, a framed type definition looks like this:

```lt
type Date(days_epoch: Int32)
```

Values of framed types can be constructed by calling the type name:

```lt
std::Date(0)
std::Time(0)
```

The wrapped field can be accessed with normal field lookup syntax:

```lt
@2025-11-14.days_epoch
@2025-11-14.0
```

## Type annotations

You can attach a type to an expression with `: Type`.

```lt
1: Int32
[]: [Int64]
.cat("Whiskers"): Animal
```

Type annotations are commonly used when:

- a numeric literal needs a specific type,
- an empty array needs an element type,
- an enum value must be interpreted as a particular enum type.

## Type variables and constraints

Functions can quantify over type variables with a `where` clause.

```lt
func identity(x: T): T
where T
-> x
```

A constraint can restrict a type variable to a tuple shape:

```lt
func false_x_false(x: T) where T: {x2: Text, ..} -> {false, ..x, false}.x2
```

The tuple constraint `..` means the tuple may contain additional fields.

## Notes on option values

Lutra does not have a separate option type syntax.
In source code, option-like values are regular enums, typically written as:

```lt
enum {none, some: T}
```

The standard library builds on that convention.

## See also

- [Literals](literals.md)
- [Expressions](expressions.md)
- [Patterns](patterns.md)
- [Definitions](definitions.md)
