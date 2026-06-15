# `std::ops` - Operators

## `func` mul

```lutra
func mul(T, T): T
where T: AnyNumber
```

Multiplies two numbers.

## `func` div

```lutra
func div(T, T): T
where T: AnyNumber
```

Divides two numbers.

## `func` mod

```lutra
func mod(T, T): T
where T: AnyNumber
```

Division remainder.

## `func` add

```lutra
func add(T, T): T
where T: AnyNumber | Duration | Decimal
```

Adds two numbers.

## `func` sub

```lutra
func sub(T, T): T
where T: AnyNumber | Duration | Decimal
```

Subtracts two numbers.

## `func` neg

```lutra
func neg(T): T
where T: Int8 | Int16 | Int32 | Int64 | Float32 | Float64 | Duration
```

Negates a number.

## `func` cmp

```lutra
func cmp(T, T): Ordering
where T
```

Compare two values to determine if the first is less, equal, or greater than
the second.

## `type` Ordering

```lutra
type Ordering: enum {less, equal, greater}
```

Result of a three-way comparison. Returned by [`cmp`](#func-cmp).

## `func` eq

```lutra
func eq(T, T): Bool
where T
```

Tests if values are equal. Used by `==` operator.

## `func` ne

```lutra
func ne(T, T): Bool
where T
```

Tests if values are not equal. Used by `!=` operator.

## `func` gt

```lutra
func gt(T, T): Bool
where T
```

Tests if left value is greater than the right. Used by `>` operator.

## `func` lt

```lutra
func lt(T, T): Bool
where T
```

Tests if left value is less than the right. Used by `<` operator.

## `func` gte

```lutra
func gte(T, T): Bool
where T
```

Tests if left value is greater or equal to the right. Used by `>=` operator.

## `func` lte

```lutra
func lte(T, T): Bool
where T
```

Tests if left value is less or equal to the right. Used by `<=` operator.

## `func` and

```lutra
func and(Bool, Bool): Bool
```

Tests if both values are true. Used by `&&` operator.

## `func` or

```lutra
func or(Bool, Bool): Bool
```

Tests if either of the values is true. Used by `||` operator.

## `func` not

```lutra
func not(Bool): Bool
```

Inverts a boolean. Used by `!` operator.

