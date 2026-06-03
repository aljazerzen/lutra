---
title: Aggregations
---

Aggregation is the point where a table stops being only a list of rows and
starts producing summaries.

In Lutra, aggregation happens in three stages:

- group rows and partitions with key and values,
- switch values to a columnar view with `aggregate` or `map_columnar`,
- reduce one column, for example with `sum` or `count`.

## Count rows

`count` returns the number of items in an array.

```lt
const movies = [
  {id = 1: Int32, title = "Arrival"},
  {id = 2: Int32, title = "Dune"},
]

func main() -> movies | count()
```

`count([])` is `0`.

## Sum a column

A common pattern is:

1. map rows to one field,
2. aggregate that field.

```lt
const sales = [
  {category = "books", amount = 10: Int32},
  {category = "games", amount = 25: Int32},
  {category = "books", amount = 15: Int32},
]

func main() -> (
  sales
  | map(x -> x.amount)
  | sum()
)
```

`sum` returns zero on an empty array.

## `min`, `max`, and `mean`

`min` and `max` return an option-like result because an empty array has no
minimum or maximum.

```lt
func main() -> {
  min([5, 3, 8: Int32]),
  max([5, 3, 8: Int32]),
}
```

```lt
func main() -> min([]: [Int32])
```

That returns `none`.

`mean` always returns `Float64`.

```lt
func main() -> mean([5, 3, 8: Int32])
```

On an empty array, `mean` returns `NaN`.

## Boolean aggregates

Use `all`, `any`, and `contains` for boolean-style questions.

```lt
func main() -> {
  all([true, true, true]),
  any([false, false, true]),
  contains(["a", "b", "c"], "b"),
}
```

Useful edge cases:

- `all([])` is `true`
- `any([])` is `false`
- `contains([], x)` is `false`

## Group rows and summarize them with `group_map`

`group_map` is the primary grouped-aggregation tool.
It is often the closest Lutra equivalent to SQL `GROUP BY`.

```lt
const sales = [
  {category = "books", amount = 10: Int32},
  {category = "games", amount = 25: Int32},
  {category = "books", amount = 15: Int32},
]

func main() -> (
  sales
  | group_map(
    x -> x.category,
    func (category, rows) -> {
      category = category,
      total_amount = rows | map(r -> r.amount) | sum(),
    }
  )
)
```

The flow is:

1. choose a grouping key,
2. gather all rows for that key,
3. compute one summary row for that group.

## Compute several metrics per group

You can return as many summary fields as you need.

```lt
func main() -> (
  sales
  | group_map(
    x -> x.category,
    func (category, rows) -> {
      category = category,
      total_amount = rows | map(r -> r.amount) | sum(),
      count = rows | count(),
    }
  )
)
```

This is often more convenient than using `group` first and then writing another
`map` step.

## Aggregate a whole relation into one row

You can summarize the whole relation by grouping everything under the same key.

```lt
func main() -> (
  sales
  | group_map(
    _ -> true,
    func (_, rows) -> {
      total_amount = rows | map(r -> r.amount) | sum(),
      count = rows | count(),
    }
  )
)
```

This returns an array with one summary row.

Sometimes you want the summary row itself rather than a one-item array.
A common pattern is:

```lt
func main() -> (
  sales
  | group_map(
    _ -> true,
    func (_, rows) -> {
      total_amount = rows | map(r -> r.amount) | sum(),
      count = rows | count(),
    }
  )
  | index(0)
  | option::or_default()
)
```

This is also useful when the input relation may be empty.

## Use `group` directly when you need the grouped rows

`group` returns rows of the shape `{key, values}`.

```lt
func main() -> (
  sales
  | group(x -> x.category)
)
```

Use `group` directly when you want access to the grouped rows themselves.
Use `group_map` when you already know you want one summary row per group.

## Use `fold` and `scan` for custom reductions

`fold` lets you build your own accumulator.

```lt
func main() -> fold(
  [1, 2, 3, 4, 5]: [Int64],
  {sum = 0: Int64, count = 0: Int64},
  func (s, n: Int64) -> {sum = s.sum + n, count = s.count + 1}
)
```

`scan` is similar, but it returns every intermediate accumulator.

```lt
func main() -> scan(
  [1, 2, 3, 4, 5]: [Int64],
  0,
  func (s: Int64, n: Int64): Int64 -> n + s
)
```

Use `fold` when you want only the final result. Use `scan` when you want the
running results.

## Think in columns with `aggregate`

Most of the time, Lutra code works row by row. Sometimes it is more natural to
work column by column instead.

`aggregate` converts an array of rows into a columnar shape and passes it to a
closure.

```lt
const rel: [{sales: Int64, refunds: Int64}] = [
  {sales = 5, refunds = 3},
  {sales = 65, refunds = 1},
  {sales = 3, refunds = 2},
]

func main() -> aggregate(rel, func (x: {sales: [Int64], refunds: [Int64]}) -> {
  min_sales = min(x.sales),
  min_refunds = min(x.refunds),
})
```

The closure receives columns, not rows. That means it can naturally express
operations that are easier to write over full columns.

## Understand `to_columnar`, `from_columnar`, and `map_columnar`

These helper functions expose the same idea more explicitly.

`to_columnar` turns rows into columns:

```lt
func main() -> to_columnar([
  {sales = 5: Int16, refunds = 3: Int16},
  {sales = 65, refunds = 1},
  {sales = 3, refunds = 2},
])
```

`from_columnar` turns columns back into rows:

```lt
func main() -> from_columnar({
  sales = [5: Int16, 65, 3],
  refunds = [3: Int16, 1, 2],
})
```

`map_columnar` lets you transform a relation through a columnar closure and then
convert back to rows.

```lt
func main() -> map_columnar(
  [
    {sales = 5: Int16, refunds = 3: Int16},
    {sales = 65, refunds = 1},
    {sales = 3, refunds = 2},
  ],
  func (x: {sales: [Int16], refunds: [Int16]}) -> {
    sales = lag(x.sales, 1),
    refunds = lead(x.refunds, 1),
  }
)
```

A useful rule of thumb is:

- use `map` and `group_map` for row-oriented work,
- use `aggregate` or `map_columnar` when the problem is naturally column-oriented.

## See also

- [Tabular data basics](tabular-data.md) for row-oriented transformations.
- [Pipelines](pipelines.md) for the general left-to-right programming style.
- [Date and time](date-time.md) for local calendar conversions.
- [Reporting](reporting.md) for end-to-end summary patterns.
- [Reference: Expressions](../reference/language/expressions.md) for exact call and pipe rules.
- [Reference: Types](../reference/language/types.md) for tuples, arrays, and grouped result shapes.
