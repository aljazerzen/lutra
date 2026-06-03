---
title: Tabular data
---

Lutra is especially comfortable when you work with arrays of tuples. That shape
matches tabular data well:

- each tuple is a row,
- each field is a column,
- pipelines describe transformations from one table shape to another.

If you have used SQL, data frames, or spreadsheets, this should feel familiar.
The main difference is that Lutra makes the row shape explicit in the type
system, and then lets you transform that shape step by step.

## Model rows as tuples

Here is a small in-memory dataset. `movies` is a table of movie rows, and
`directors` is a table of director rows.

```lt
type Movie: {
  id: Int32,
  title: Text,
  year: Int32,
  director_id: Int32,
}

type Director: {
  id: Int32,
  name: Text,
}

const movies: [Movie] = [
  {id = 1, title = "Arrival", year = 2016, director_id = 1},
  {id = 2, title = "Dune", year = 2021, director_id = 1},
  {id = 3, title = "Barbie", year = 2023, director_id = 2},
]

const directors: [Director] = [
  {id = 1, name = "Denis Villeneuve"},
  {id = 2, name = "Greta Gerwig"},
]
```

Notice what the types tell you immediately:

- a movie row always has `id`, `title`, `year`, and `director_id`,
- a director row always has `id` and `name`,
- `movies` is an array of `Movie`, so every item has the same row shape,
- `directors` is an array of `Director`.

That explicit structure is what makes the later transformations predictable.

## Slice rows by position

Use `slice` when you want a subrange by index.

```lt
func main() -> (
  movies
  | slice(0, 2)
)
```

This does not change the row shape. It only changes how many rows you keep.
You still have `[Movie]` on the output, just with fewer items.

## Filter rows by predicate

Use `filter` when you want to keep only matching rows.

```lt
func main() -> (
  movies
  | filter(m -> m.year >= 2020)
)
```

Here, each row is tested with the predicate `m.year >= 2020`.
Rows that satisfy the predicate stay in the result. Rows that do not satisfy it
are removed.

Again, the shape does not change. The result is still `[Movie]`.

## Map rows into a new shape

Use `map` to rename fields, derive fields, or drop fields.

```lt
func main() -> (
  movies | map(m -> {
    movie_id = m.id,
    title = m.title,
    is_recent = m.year >= 2020,
  })
)
```

This is the first transformation that changes the row shape.
Each `Movie` row becomes a new tuple with fields `movie_id`, `title`,
and `is_recent`.

This is one of the most common operations in Lutra. You take a row with one
shape and produce a row with a different shape.

## Sort rows

Use `sort` when order matters.

```lt
func main() -> (
  movies | sort(m -> m.year)
)
```

`sort` does not change the row shape either. It only changes row order.
Think of it as reordering the table, not changing its columns.

## Use `flat_map` when one row can produce many rows

`flat_map` is useful when one input item can expand into zero or more output
items.

```lt
func words(title: Text) -> [title, f"{title}!" ]

func main() -> (
  ["Arrival", "Dune"]
  | flat_map(t -> words(t))
)
```

You can read this as:

- take one input item,
- return an array of output items for it,
- flatten all those arrays into one result array.

This is useful when the relationship is one-to-many.

## Join-like lookups are often plain mapping

Lutra does not need special syntax for every join-like operation. A common
pattern is to map each row and fetch related data inside the mapper.

```lt
func get_director_by_id(id: Int32) -> (
  directors | find(d -> d.id == id)
)

func main() -> (
  movies | map(m -> {
    m.title,
    director = get_director_by_id(m.director_id) | option::or_default(),
  })
)
```

What is happening here:

1. start with each movie row,
2. use `director_id` to look up a related director row,
3. build a new row that contains both movie data and related director data.

This keeps the transformation explicit: start from one row shape, enrich it,
and return a new row shape.

## Read pipelines as table transformations

A realistic pipeline often looks like this:

```lt
func main() -> (
  movies
  | filter(m -> m.year >= 2016)
  | map(m -> {
    m.id,
    title = m.title,
    decade = m.year / 10,
  })
  | sort(m -> m.title)
  | slice(0, 10)
)
```

Try to read it one step at a time:

1. start with all movie rows,
2. keep only movies from 2016 onward,
3. reshape each row to keep only the fields you want,
4. sort the rows by title,
5. take the first 10 rows.

That is the core mental model for Lutra on tabular data: each step consumes one
well-typed table shape and produces the next one.

## See also

- [Pipelines](pipelines.md)
- [Aggregations](aggregations.md)
- [Reporting](reporting.md)
- [Projects](projects.md)
- [Reference: Expressions](../reference/language/expressions.md)
