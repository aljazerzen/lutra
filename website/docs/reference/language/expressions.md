---
title: Expressions
---

This page describes the expression forms used in Lutra.

## Overview

Expressions compute values.
Examples include literals, tuple construction, function calls, pipelines,
conditionals, and matches.

## Literals and names

A literal or a name is an expression.

```lt
false
42: Int32
"hello"
a
std::cmp
```

See [Literals](literals.md) for the scalar literal forms.

## Tuple expressions

Tuple expressions use `{...}`.
Fields can be positional or named.

```lt
{true, 10}
{a = true, 10, b = 5.4}
```

You can mix named and positional fields in the same tuple.

### Tuple field access

Use `.` with a field name or zero-based position.

```lt
{a = 10, false}.a
{a = 10, false}.1
```

Field lookup also works after tuple spread and on framed values such as
`std::Date`.

Tuple field access (named and positional) is also used to access [inner
values of framed types](./definitions#framed-types).

### Tuple spread

Use `..expr` inside a tuple expression to insert all fields from another tuple.

```lt
{a = 4: Int32, ..{x1 = 3: Int32, x2 = "hello"}, b = false}
{false, ..x, false}
```

Tuple spread preserves field order.
Spread works with named and positional fields.

## Array expressions

Array expressions use `[...]`.
All items must have the same type.

```lt
[true, false, true]
[10, 33, -3, 2, 40]: [Int64]
[]: [Int64]
```

## Enum construction

Enum variants are constructed with a leading `.`.
Variants without payloads do not use parentheses.
Variants with payloads do.

```lt
.done
.pending(513)
.cancelled("I don't like it")
.dog(.collie("Belie"))
```

A type annotation is sometimes needed so the compiler knows which enum type you
mean:

```lt
.cat("Whiskers"): Animal
```

## Function calls

A function call uses `name(arg1, arg2, ...)`.

```lt
my_func(4, false)
std::add(6, 2)
```

Arguments can be positional or labeled. See [Definitions](definitions.md) for
labeled parameter syntax.

```lt
calculate(1, add = 2, multiply = 3)
```

## Anonymous functions

Lutra supports two anonymous function forms.

The short form uses `name -> expr`:

```lt
x -> x + 1
n -> match n {
  "world" => "Hello world!",
  n => f"Hello, {n}!"
}
```

The long form uses `func (...) -> expr` and can include parameter and return
annotations:

```lt
func (x: Int64) -> x + 1
func (animal: Animal): Text -> match animal {
  .cat(name) => name,
  _ => "<unnamed>",
}
```

## Pipelines

The pipe operator `|` passes the value on the left as the first argument to the
expression on the right.
Pipelines are left-associative.

These forms are equivalent:

```lt
my_func(5)
5 | my_func()
5 | my_func
```

These forms are also equivalent:

```lt
another_func(my_func(5), false)
5 | my_func | another_func(false)
```

The right side of a pipeline is usually one of these:

- a function name,
- a function call with the remaining arguments,
- an anonymous function.

## Conditional expressions

An `if` expression chooses between two expressions.

```lt
if x then "yes" else "no"
```

Branches can use parentheses for multiline formatting:

```lt
if x then (
  "yes"
) else (
  "no"
)
```

## Match expressions

A `match` expression compares a subject value against patterns.
The first matching branch is selected.

```lt
match name {
  "world" => "Hello world!",
  n => f"Hello, {n}!"
}
```

You can also match enum variants and nested enum values:

```lt
match animal {
  .cat(name) => f"Hello {name}",
  .dog(.generic) => "Who's a good boy?",
  .dog(.collie(name)) => f"Come here {name}",
}
```

See [Patterns](patterns.md) for the available pattern forms.

## Let blocks

A parenthesized block can contain one or more `let` bindings, separated by
semicolons, followed by a final expression.
The value of the block is the value of the final expression.

```lt
(
  let a = false;
  {a, a, !a}
)
```

You can use multiple bindings:

```lt
(
  let a = x * 2;
  let b = a * 2;
  let c = -b;
  c
)
```

## Type ascription

Use `expr: Type` to fix the type of an expression.

```lt
4: Int32
[]: [Bool]
.none: enum {none, some: Text}
```

## See also

- [Literals](literals.md)
- [Types](types.md)
- [Patterns](patterns.md)
- [Operators](operators.md)
