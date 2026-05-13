# `std` - Standard library

## Modules

- [`option`](option/index.md) - Nullable values
- [`math`](math/index.md) - Mathematical functions
- [`text`](text/index.md) - Text functions
- [`fs`](fs/index.md) - File system operations
- [`sql`](sql/index.md) - SQL database interface
- [`date`](date/index.md)
- [`timestamp`](timestamp/index.md)

## `func` default

```lutra
func default(): T
where T
```

Constructs the usual "default" value

For `bool` it is false.
For numbers, this is zero.
For `text` it is an empty text.
For tuples, it a tuple with every field set to default value.
For arrays, it is an empty array.
For enums, it the first variant.

## `func` to_int8

```lutra
func to_int8(T): int8
where T: number
```

Converts a number to `int8`

## `func` to_int16

```lutra
func to_int16(T): int16
where T: number
```

Converts a number to `int16`

## `func` to_int32

```lutra
func to_int32(T): int32
where T: number
```

Converts a number to `int32`

## `func` to_int64

```lutra
func to_int64(T): int64
where T: number
```

Converts a number to `int64`

## `func` to_uint8

```lutra
func to_uint8(T): uint8
where T: number
```

Converts a number to `uint8`

## `func` to_uint16

```lutra
func to_uint16(T): uint16
where T: number
```

Converts a number to `uint16`

## `func` to_uint32

```lutra
func to_uint32(T): uint32
where T: number
```

Converts a number to `uint32`

## `func` to_uint64

```lutra
func to_uint64(T): uint64
where T: number
```

Converts a number to `uint64`

## `func` to_float32

```lutra
func to_float32(T): float32
where T: number
```

Converts a number to `float32`

## `func` to_float64

```lutra
func to_float64(T): float64
where T: number
```

Converts a number to `float64`

## `func` to_text

```lutra
func to_text(T): text
where T: primitive
```

Converts a value to `text`

## `func` mul

```lutra
func mul(T, T): T
where T: number
```

Multiplies two numbers.

## `func` div

```lutra
func div(T, T): T
where T: number
```

Divides two numbers.

## `func` mod

```lutra
func mod(T, T): T
where T: number
```

Division remainder.

## `func` add

```lutra
func add(T, T): T
where T: number
```

Adds two numbers.

## `func` sub

```lutra
func sub(T, T): T
where T: number
```

Subtracts two numbers.

## `func` neg

```lutra
func neg(T): T
where T: int8 | int16 | int32 | int64 | float32 | float64
```

Negates a number.

## `func` cmp

```lutra
func cmp(T, T): Ordering
where T: primitive
```

Compare two values to determine if the first is less, equal, or greater than
the second.

## `type` Ordering

```lutra
type Ordering: enum {less, equal, greater}
```

Result of a three-way comparison. Returned by [`cmp`](#cmp).

## `func` eq

```lutra
func eq(T, T): bool
where T: primitive
```

Tests if values are equal. Used by `==` operator.

## `func` ne

```lutra
func ne(T, T): bool
where T: primitive
```

Tests if values are not equal. Used by `!=` operator.

## `func` gt

```lutra
func gt(T, T): bool
where T: primitive
```

Tests if left value is greater than the right. Used by `>` operator.

## `func` lt

```lutra
func lt(T, T): bool
where T: primitive
```

Tests if left value is less than the right. Used by `<` operator.

## `func` gte

```lutra
func gte(T, T): bool
where T: primitive
```

Tests if left value is greater or equal to the right. Used by `>=` operator.

## `func` lte

```lutra
func lte(T, T): bool
where T: primitive
```

Tests if left value is less or equal to the right. Used by `<=` operator.

## `func` and

```lutra
func and(bool, bool): bool
```

Tests if both values are true. Used by `&&` operator.

## `func` or

```lutra
func or(bool, bool): bool
```

Tests if either of the values is true. Used by `||` operator.

## `func` not

```lutra
func not(bool): bool
```

Inverts a boolean. Used by `!` operator.

## `func` index

```lutra
func index([T], int64): enum {none, some: T}
where T
```

Returns the item at a zero-based `position`, or `.none` if out of bounds.

## `func` count

```lutra
func count([T]): int64
where T
```

Returns the number of items in an array.

## `func` is_empty

```lutra
func is_empty([T]): bool
where T
```

Returns `true` if the array contains no items.

## `func` map

```lutra
func map([I], func (I): O): [O]
where I, O
```

Transforms each item using a `mapper` function.
Returns a new array of the same length.

## `func` flat_map

```lutra
func flat_map([I], func (I): [O]): [O]
where I, O
```

Applies `mapper` to each item and concatenates the resulting arrays.

## `func` filter

```lutra
func filter([T], func (T): bool): [T]
where T
```

Returns only the items for which `condition` returns `true`.

## `func` find

```lutra
func find([T], func (T): bool): enum {none, some: T}
where T
```

Returns the first item for which `condition` returns `true`,
or `.none` if no item matches.

## `func` slice

```lutra
func slice([T], int64, int64): [T]
where T
```

Returns items from index `start` (inclusive) to `end` (exclusive).

## `func` sort

```lutra
func sort([I], func (I): K): [I]
where I, K: primitive
```

Sorts the array in ascending order by the value returned by `key`.

## `func` to_columnar

```lutra
func to_columnar([T]): {for f: F in T do f: [F]}
where T: {..}
```

Transposes an array of rows into a tuple of columns.
Each field of the row type becomes a column — an array of that field's values.
The inverse of [`from_columnar`](#from_columnar).

## `func` from_columnar

```lutra
func from_columnar({for f: F in T do f: [F]}): [T]
where T: {..}
```

Transposes a tuple of columns into an array of rows.
The inverse of [`to_columnar`](#to_columnar).

## `func` map_columnar

```lutra
func map_columnar([I], func ({for f: F in I do f: [F]}): {for f: F in O do f: [F]}): [O]
where I: {..}, O: {..}
```

Applies `mapper` to the array in columnar form.
Equivalent to [`to_columnar`](#to_columnar), then `mapper`, then [`from_columnar`](#from_columnar).

## `func` aggregate

```lutra
func aggregate([T], func ({for f: F in T do f: [F]}): O): O
where T: {..}, O
```

Reduces an array to a single value by applying `mapper` in columnar form.
Equivalent to [`to_columnar`](#to_columnar) followed by `mapper`.

## `func` zip

```lutra
func zip([L], [R]): [{L, R}]
where L, R
```

Pairs items from `left` and `right` by position.
The result length equals the shorter of the two inputs.

## `func` group

```lutra
func group([I], func (I): K): [{key: K, values: [I]}]
where I, K
```

Groups array items by a key.
Returns groups, which contain the key and an array of values in this group.

## `func` group_map

```lutra
func group_map([I], func (I): K, func (K, [I]): O): [O]
where I, K, O
```

Groups array items by a key.
Returns groups, which contain the key and an array of values in this group.

## `func` distinct

```lutra
func distinct([T]): [T]
where T
```

Deduplicates array items.

## `func` append

```lutra
func append([T], [T]): [T]
where T
```

Concatenates two arrays, `first` items followed by `second` items.

## `func` fold

```lutra
func fold([I], A, func (A, I): A): A
where I, A
```

Folds every input into the an accumulator value by applying an operation,
returning the final result.

The `operation` takes two arguments: accumulator and an input.
It returns the value that the accumulator should have in next iteration.

The `initial` value is the accumulator for the first call.

After folding all inputs, the last accumulator is returned.

This function is sometimes also called "reduce" or "inject".
This function is similar to [std::scan], but it returns only the final
accumulator.

## `func` scan

```lutra
func scan([I], A, func (A, I): A): [A]
where I, A
```

Applies an operation to each input, using the output from the previous
iteration.

The `operation` takes two arguments: accumulator and an input.
It returns the value that the accumulator should have in next iteration.

The `initial` value is the accumulator for the first call.

Returns values of all produced accumulators.

This function is similar to [std::fold], but it returns all accumulators,
instead of only the final one.

## `func` apply_until_empty

```lutra
func apply_until_empty([T], func ([T]): [T]): [T]
where T
```

Applies an operation to an array, until it returns an empty array.
First operation is supplied with the initial array.
Each following operation is supplied with result of previous operation.

Returns all produced arrays concatenated together, including initial array.

In SQL, this is known as "RECURSIVE CTE" or "recursive join".

## `func` sequence

```lutra
func sequence(N, N): [N]
where N: int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64
```

Returns an array of sequential integers
from start (inclusive) to end (exclusive).

For example: `sequence(0, 3) == [0, 1, 2]`

## `func` min

```lutra
func min([T]): enum {none, some: T}
where T: primitive
```

Find the minimum value in the array.
Returns `none` when array is empty.

## `func` max

```lutra
func max([T]): enum {none, some: T}
where T: primitive
```

Find the maximum value in the array.
Returns `none` when array is empty.

## `func` sum

```lutra
func sum([T]): T
where T: number
```

Compute sum of all number in an array.
Returns zero when array is empty.

## `func` mean

```lutra
func mean([T]): float64
where T: number
```

Compute arithmetic mean of an array of numbers.

## `func` all

```lutra
func all([bool]): bool
```

Returns `true` if all items in the array are `true`.

## `func` any

```lutra
func any([bool]): bool
```

Returns `true` if any value in the array is `true`.

## `func` contains

```lutra
func contains([T], T): bool
where T: primitive
```

Returns `true` if the haystack array contains an item equal to `needle`.

## `func` lag

```lutra
func lag([T], int64): [T]
where T
```

Shifts array items backwards by an offset.
Items in front are set to their default value.

For example, `lag(["a", "b", "c"], 1)` is `["", "a", "b"]`.

## `func` lead

```lutra
func lead([T], int64): [T]
where T
```

Shifts array items forwards by an offset.
Items in back are set to their default value.

For example, `lead(["a", "b", "c"], 1)` is `["b", "c", ""]`.

## `func` rolling_mean

```lutra
func rolling_mean([T], uint32, uint32): [float64]
where T: number
```

Computes rolling arithmetic mean over an array of numbers.
Also known as moving average.

Returns an array of same length as the input,
where each item is the mean of the corresponding input item and
a number of preceding and following items.

For example, `rolling_mean(..., 1, 2)` computes mean of each item, along with
1 preceding and 2 following items, which is 4 items total.

## `func` rank

```lutra
func rank([T]): [int32]
where T: primitive
```

Computes rank of each array item.

Rank is the number of items that are less than current item, plus one.

The values range from `1` to `n` (number of items).
Also known as "min rank".

For example, `rank(["a", "b", "b", "c"])` is `[1, 2, 2, 4]`.

## `func` rank_dense

```lutra
func rank_dense([T]): [int32]
where T: primitive
```

Computes dense rank of each array item.

Dense rank is the number of unique items that are less than current item,
plus one.

The values range from `1` to `n` (number of unique items).

For example, `rank_dense(["a", "b", "b", "c"])` is `[1, 2, 2, 3]`.

## `func` rank_percentile

```lutra
func rank_percentile([T]): [float64]
where T: primitive
```

Computes percentile rank of each array item.

Percentile rank is the item rank rescaled to range from 0.0 to 1.0.

The values range from 0.0 to 1.0.

For example, `rank_percentile(["a", "b", "b", "c"])` is
`[0.0, 0.33333, 0.25, 1.0]`.

## `func` cume_dist

```lutra
func cume_dist([T]): [float64]
where T: primitive
```

Computes ECDF (Empirical Cumulative Density Function) of array items.

Value of each item i is the number of items that are less than or equal to i,
divided by the total number of items.

The values range from `1.0/n` to `1.0`.

For example, `cume_dist(["a", "b", "b", "c"])` is
`[0.25, 0.75, 0.75, 1.0]`.

## `type` Timestamp

```lutra
type Timestamp(microseconds: int64)
```

An instant in time. Timestamp without a timezone.

Backed by a signed 64-bit integer, indicating microseconds since the Unix
epoch (1970-01-01T00:00:00.000 UTC), excluding leap seconds.

## `type` Date

```lutra
type Date(days_epoch: int32)
```

Elapsed days since Unix Epoch (1970-01-01).

Backed by a signed 32-bit integer, representing number of days.

## `type` Time

```lutra
type Time(microseconds: int64)
```

Length of time, unrelated to calendar events.
Can be interpreted as duration or offset from the midnight.

Backed by a signed 64-bit integer, representing number of microseconds.

## `func` add_time

```lutra
func add_time(Time, Time): Time
```

Adds two `Time` durations together.

## `func` sub_timestamp

```lutra
func sub_timestamp(Timestamp, Timestamp): Time
```

Computes the elapsed `Time` between two `Timestamp`s.
Result is `a - b`: positive when `a` is later than `b`.

## `func` timezone_offset

```lutra
func timezone_offset(text, Date): Time
```

Returns offset of a time zone from UTC on a given date.

## `type` Decimal

```lutra
type Decimal(int64)
```

Exact decimal value with a fixed `SCALE`, which is the number of digits past
the decimal point.
Currently, `SCALE` is 2. This will be configurable in the future.
Can hold values from `-10**(19-SCALE)` to `10**(19-SCALE)`.

Backed by a signed 64-bit integer.

