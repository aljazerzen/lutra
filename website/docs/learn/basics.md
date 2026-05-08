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
$ lutra run --project example.lt --interpreter
const output = "Hello, world!"
```

Lutra does not have a `print` function. Instead, you run a function and the CLI
prints the returned value.

## Use constants and functions

A `const` defines a value. A `func` defines a computation.

```lt
const greeting = "Hello"

func greet(name: text) -> f"{greeting}, {name}!"

func main() -> greet("Ada")
```

```console
$ lutra run --project example.lt --interpreter
const output = "Hello, Ada!"
```

Use `const` for fixed values. Use `func` when you need computation, parameters,
or reusable logic.

## Work with types

Lutra is statically typed. Every expression has a type known at compile time.

```lt
const answer: int32 = 42
const title = "Arrival"

func is_large(x: int32) -> x > 10

func main() -> {
  answer,
  title,
  is_large(answer),
}
```

Common types include:

- primitive types such as `bool`, `int32`, `int64`, `float32`, `float64`, and `text`
- tuples such as `{id: int32, title: text}`
- arrays such as `[text]`
- enums such as `enum {draft, published}`

## Type annotations

Lutra can infer many types, but explicit annotations are useful when they make
intent clearer or when the compiler must choose a numeric type.

```lt
const movie_id: int32 = 5
const rating = 8.7: float64

func main() -> {movie_id, rating}
```

## Build tuples and arrays

Tuples group values of different types:

```lt
const movie = {
  id = 1: int32,
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
const movies: [{id: int32, title: text}] = [
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
  scheduled: text,
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
  id: int32,
  subtitle: enum {none, some: text},
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
