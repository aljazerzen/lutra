# `std::array` - Array functions

Basic array functions, aggregation, and window functions

## `func` index

```lutra
func index(array: [T], position: Int64): enum {none, some: T}
where T
```

Returns the item at a zero-based `position`, or `.none` if out of bounds.

## `func` count

```lutra
func count(array: [T]): Int64
where T
```

Returns the number of items in an array.

## `func` is_empty

```lutra
func is_empty([T]): Bool
where T
```

Returns `true` if the array contains no items.

## `func` map

```lutra
func map(array: [I], mapper: func (I): O): [O]
where I, O
```

Transforms each item using a `mapper` function.
Returns a new array of the same length.

## `func` flat_map

```lutra
func flat_map(array: [I], mapper: func (I): [O]): [O]
where I, O
```

Applies `mapper` to each item and concatenates the resulting arrays.

## `func` filter

```lutra
func filter(array: [T], condition: func (T): Bool): [T]
where T
```

Returns only the items for which `condition` returns `true`.

## `func` find

```lutra
func find(array: [T], func (T): Bool): enum {none, some: T}
where T
```

Returns the first item for which `condition` returns `true`,
or `.none` if no item matches.

## `func` slice

```lutra
func slice(array: [T], start: Int64, end: Int64): [T]
where T
```

Returns items from index `start` (inclusive) to `end` (exclusive).

## `func` sort

```lutra
func sort(array: [I], key: func (I): K): [I]
where I, K: primitive
```

Sorts the array in ascending order by the value returned by `key`.

## `func` to_columnar

```lutra
func to_columnar(rows: [T]): {for f: F in T do f: [F]}
where T: {..}
```

Transposes an array of rows into a tuple of columns.
Each field of the row type becomes a column — an array of that field's values.
The inverse of [`from_columnar`](#func-from_columnar).

## `func` from_columnar

```lutra
func from_columnar(columnar: {for f: F in T do f: [F]}): [T]
where T: {..}
```

Transposes a tuple of columns into an array of rows.
The inverse of [`to_columnar`](#func-to_columnar).

## `func` map_columnar

```lutra
func map_columnar(array: [I], mapper: func ({for f: F in I do f: [F]}): {for f: F in O do f: [F]}): [O]
where I: {..}, O: {..}
```

Applies `mapper` to the array in columnar form.
Equivalent to [`to_columnar`](#func-to_columnar), then `mapper`, then [`from_columnar`](#func-from_columnar).

## `func` aggregate

```lutra
func aggregate(array: [T], mapper: func ({for f: F in T do f: [F]}): O): O
where T: {..}, O
```

Reduces an array to a single value by applying `mapper` in columnar form.
Equivalent to [`to_columnar`](#func-to_columnar) followed by `mapper`.

## `func` zip

```lutra
func zip(left: [L], right: [R]): [{L, R}]
where L, R
```

Pairs items from `left` and `right` by position.
The result length equals the shorter of the two inputs.

## `func` group

```lutra
func group(array: [I], get_key: func (I): K): [{key: K, values: [I]}]
where I, K
```

Groups array items by a key.
Returns groups, which contain the key and an array of values in this group.

## `func` group_map

```lutra
func group_map(array: [I], get_key: func (I): K, mapper: func (K, [I]): O): [O]
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
func append(first: [T], second: [T]): [T]
where T
```

Concatenates two arrays, `first` items followed by `second` items.

## `func` fold

```lutra
func fold(array: [I], initial: A, operation: func (A, I): A): A
where I, A
```

Folds every input into the an accumulator value by applying an operation,
returning the final result.

The `operation` takes two arguments: accumulator and an input.
It returns the value that the accumulator should have in next iteration.

The `initial` value is the accumulator for the first call.

After folding all inputs, the last accumulator is returned.

This function is sometimes also called "reduce" or "inject".
This function is similar to [`scan`](#func-scan), but it returns only the final
accumulator.

## `func` scan

```lutra
func scan(array: [I], initial: A, operation: func (A, I): A): [A]
where I, A
```

Applies an operation to each input, using the output from the previous
iteration.

The `operation` takes two arguments: accumulator and an input.
It returns the value that the accumulator should have in next iteration.

The `initial` value is the accumulator for the first call.

Returns values of all produced accumulators.

This function is similar to [`fold`](#func-fold), but it returns all accumulators,
instead of only the final one.

## `func` loop_until_empty

```lutra
func loop_until_empty(initial: [T], operation: func ([T]): [T]): [T]
where T
```

Applies an operation to an array, until it returns an empty array.
First operation is supplied with the initial array.
Each following operation is supplied with result of previous operation.

Returns all produced arrays concatenated together, including initial array.

In SQL, this is known as "RECURSIVE CTE" or "recursive join".

## `func` sequence

```lutra
func sequence(start: N, end: N): [N]
where N: Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64
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
func mean([T]): Float64
where T: number
```

Compute arithmetic mean of an array of numbers.

## `func` all

```lutra
func all([Bool]): Bool
```

Returns `true` if all items in the array are `true`.

## `func` any

```lutra
func any([Bool]): Bool
```

Returns `true` if any value in the array is `true`.

## `func` contains

```lutra
func contains(haystack: [T], needle: T): Bool
where T: primitive
```

Returns `true` if the haystack array contains an item equal to `needle`.

## `func` lag

```lutra
func lag(array: [T], offset: Int64): [T]
where T
```

Shifts array items backwards by an offset.
Items in front are set to their default value.

For example, `lag(["a", "b", "c"], 1)` is `["", "a", "b"]`.

## `func` lead

```lutra
func lead(array: [T], offset: Int64): [T]
where T
```

Shifts array items forwards by an offset.
Items in back are set to their default value.

For example, `lead(["a", "b", "c"], 1)` is `["b", "c", ""]`.

## `func` rolling_mean

```lutra
func rolling_mean(array: [T], preceding: Uint32, following: Uint32): [Float64]
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
func rank(array: [T]): [Int32]
where T: primitive
```

Computes rank of each array item.

Rank is the number of items that are less than current item, plus one.

The values range from `1` to `n` (number of items).
Also known as "min rank".

For example, `rank(["a", "b", "b", "c"])` is `[1, 2, 2, 4]`.

## `func` rank_dense

```lutra
func rank_dense(array: [T]): [Int32]
where T: primitive
```

Computes dense rank of each array item.

Dense rank is the number of unique items that are less than current item,
plus one.

The values range from `1` to `n` (number of unique items).

For example, `rank_dense(["a", "b", "b", "c"])` is `[1, 2, 2, 3]`.

## `func` rank_percentile

```lutra
func rank_percentile(array: [T]): [Float64]
where T: primitive
```

Computes percentile rank of each array item.

Percentile rank is the item rank rescaled to range from 0.0 to 1.0.

The values range from 0.0 to 1.0.

For example, `rank_percentile(["a", "b", "b", "c"])` is
`[0.0, 0.33333, 0.25, 1.0]`.

## `func` cume_dist

```lutra
func cume_dist(array: [T]): [Float64]
where T: primitive
```

Computes ECDF (Empirical Cumulative Density Function) of array items.

Value of each item i is the number of items that are less than or equal to i,
divided by the total number of items.

The values range from `1.0/n` to `1.0`.

For example, `cume_dist(["a", "b", "b", "c"])` is
`[0.25, 0.75, 0.75, 1.0]`.

