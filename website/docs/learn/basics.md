---
title: Basics
---

This guide shows you how to run small Lutra programs locally and introduces the
core language ideas.

Prerequisite:

- install the [Lutra CLI](../usage/cli.md)

## Create your first program

Create a file named `example.lt`:

```lt
func main() -> "Hello, world!"
```

Run it with the interpreter:

```console
$ lutra run --project example.lt --runner interpreter
const output = "Hello, world!"
```

Lutra does not have a `print` function. Instead, you run a function and the CLI
prints the returned value.

## Use constants and functions

A `const` defines a value. A `func` defines a computation.

```lt
const greeting = "Hello"

func greet(name: Text) -> f"{greeting}, {name}!"

func main() -> greet("Ada")
```

```console
$ lutra run --project example.lt --runner interpreter
const output = "Hello, Ada!"
```

Use `const` for fixed values. Use `func` when you need computation, parameters,
or reusable logic.

## Work with types

Lutra is statically typed. Every expression has a type known at compile time.

```lt
const answer: Int32 = 42
const title = "Arrival"

func is_large(x: Int32) -> x > 10

func main() -> {
  answer,
  title,
  is_large(answer),
}
```

Common types include:

- standard types such as `Bool`, `Int32`, `Int64`, `Float32`, `Float64`, and `Text`
- tuples such as `{id: Int32, title: Text}`
- arrays such as `[Text]`
- enums such as `enum {draft, published}`

## Type annotations

Lutra can infer many types, but explicit annotations are useful when they make
intent clearer or when the compiler must choose a numeric type.

```lt
const movie_id: Int32 = 5
const rating = 8.7: Float64

func main() -> {movie_id, rating}
```

## Build tuples and arrays

Tuples group values of different types:

```lt
const movie = {
  id = 1: Int32,
  title = "Arrival",
  released = true,
}

func main() -> {movie.title, movie.released}
```

Arrays hold many values of the same type:

```lt
const titles = [
  "Arrival",
  "Dune",
  "Sicario",
]

func main() -> titles
```

You will use arrays of tuples often, because they model tabular data naturally.

```lt
const movies: [{id: Int32, title: Text}] = [
  {1, "Arrival"},
  {2, "Dune"},
]

func main() -> movies
```

## Use enums for alternatives

Enums represent one value chosen from named variants.

```lt
type Status: enum {
  draft,
  published,
  scheduled: Text,
}

const posts: [Status] = [
  .draft,
  .published,
  .scheduled("2026-01-01"),
]

func main() -> posts
```

Optional values are usually modeled as `enum {none, some: T}`.

```lt
type MovieWithSubtitle: {
  id: Int32,
  subtitle: enum {none, some: Text},
}

const movie1: MovieWithSubtitle = {1, .none}
const movie2: MovieWithSubtitle = {2, .some("Part Two")}

func main() -> [movie1, movie2]
```

## Keep learning

You now know enough to read and run small Lutra programs.

Next steps:

- [Pipelines](pipelines.md)
- [Tabular data basics](tabular-data.md)
- [Aggregations](aggregations.md)
- [Projects](projects.md)

For exact syntax, use the language reference:

- [Syntax](../reference/language/syntax.md)
- [Types](../reference/language/types.md)
- [Expressions](../reference/language/expressions.md)
- [Patterns](../reference/language/patterns.md)
