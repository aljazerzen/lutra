---
title: Literals
---

This page lists the literal forms used in Lutra expressions.

## Boolean literals

Boolean literals are `true` and `false`.

```lt
true
false
```

## Text literals

Text literals use double quotes.

```lt
"hello"
"I don't like it"
```

### Interpolated text literals

Interpolated text literals start with `f` and can embed expressions in `{}`.

```lt
f"Hello {name}"
f"Come here {dog_name}"
```

## Integer literals

Integer literals are written in decimal by default.
You can add a type with `: Type` when the surrounding context does not already
fix the type.

```lt
1
432
-23
1: int32
18446744073709551615: uint64
```

Hexadecimal integer literals use the `0x` prefix.

```lt
0x7f: int8
0xffff: uint16
0x7fffffffffffffff: int64
```

Underscores are allowed as visual separators.

```lt
3_112_212_123_123_123
```

## Floating-point literals

Floating-point literals use a decimal point.

```lt
42.801
-32.2
1.10: float32
-1121121221.0: float64
```

## Decimal literals

When annotated with`std::Decimal`, numeric literals are interpreted as
`std::Decimal` values.

```lt
func main(): [std::Decimal] -> [
  4.2,
  4.20,
  4.02,
  3_112_212_123_123_123.11,
]
```

`std::Decimal` preserves the fixed-scale decimal representation used by the
value, so `4.2` is printed as `4.20`.

## Date literals

Date literals start with `@` and use `year-month-day`.

```lt
@2025-11-14
@-10000-11-14
@+100000-11-14
```

These literals have type `std::Date`.

## Time literals

Time literals also start with `@` and use `hour:minute:second`, with an
optional fractional part.

```lt
@16:07:44.123456
@16:07:44.12
@213:06:00
@-5:12:31.12
```

These literals have type `std::Time`.

## Timestamp literals

Timestamp literals combine a date and a time with `T`.

```lt
@2025-12-29T16:07:44.123456
@2025-12-29T16:07:44.12
```

These literals have type `std::Timestamp`.

## Composite values

Tuple, array, and enum construction use expression syntax rather than literal
syntax. See [Expressions](expressions.md).

## See also

- [Types](types.md)
- [Expressions](expressions.md)
- [Operators](operators.md)
