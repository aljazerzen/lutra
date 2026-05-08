---
title: Operators
---

## Arithmetic operators

These arithmetic operators are used as infix operators:

| Operator | Meaning |
| --- | --- |
| `*` | Multiplication. |
| `/` | Division. |
| `%` | Remainder. |
| `+` | Addition. |
| `-` | Subtraction. |

Examples:

```lt
30 + 2
10 / 6
10.0 / 6.0
-10 % 6
```

Unary negation uses prefix `-`.

```lt
-2
- (-3.1: float64)
```

## Comparison operators

These comparison operators return `bool`:

| Operator | Meaning |
| --- | --- |
| `==` | Equal to. |
| `!=` | Not equal to. |
| `<` | Less than. |
| `<=` | Less than or equal to. |
| `>` | Greater than. |
| `>=` | Greater than or equal to. |

Examples:

```lt
30: int64 == 30
2 < 3: int64
"aa" >= "ab"
```

## Boolean operators

Boolean operators use these forms:

| Operator | Meaning |
| --- | --- |
| `!` | Logical not. |
| `&&` | Logical and. |
| `||` | Logical or. |

Examples:

```lt
!false
false && true
true || false
```

## The pipe operator

The pipe operator `|` passes the left-hand value as the first argument to the
right-hand side.

```lt
4: int32 | std::add(1)
[1, 2, 3]: [int32] | std::map(x -> x * 2)
```

Unlike the operators listed earlier, `|` is not arithmetic or boolean. It is a
pipeline operator used to express left-to-right data flow.

## Operator precedence

From tighter to looser precedence:

1. Prefix unary operators: `-`, `!`
2. Multiplicative operators: `*`, `/`, `%`
3. Additive operators: `+`, `-`
4. Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
5. Boolean `&&`
6. Boolean `||`
7. Pipe `|`

Use parentheses when you want to make grouping explicit.

## Relationship to the standard library

Most symbolic operators correspond to standard library functions.
For example:

- `+` corresponds to `std::add`
- `==` corresponds to `std::eq`
- `!` corresponds to `std::not`

The operator syntax is the source-level shorthand.

## See also

- [Expressions](expressions.md)
- [Types](types.md)
