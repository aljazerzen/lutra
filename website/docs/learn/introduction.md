---
title: Introduction
---

Lutra is a statically typed language for querying data and describing data
structures.

It is designed around a few ideas:

- values have clear, explicit types,
- programs are built from expressions,
- pipelines make data flow easy to read,
- the same Lutra program can target different execution backends.

A small Lutra program looks like this:

```lt
func main() -> (
  [1, 2, 3]: [int32]
  | map(x -> x * 2)
)
```

When you run it, Lutra evaluates the `main` function and prints the result:

```console
$ lutra run --project example.lt --runner interpreter
const output = [
  2,
  4,
  6,
]
```

A typical Lutra program combines:

- **types** such as tuples, arrays, and enums,
- **functions** that transform values,
- **pipelines** that keep transformations readable,
- **modules** that organize code into projects.

## How to read the docs

Use the docs by intent:

- Go to [Basics](basics.md) if you want your first runnable examples.
- Stay in `learn/` if you want to become fluent in writing Lutra.
- Go to [Usage](../usage/cli.md) if you want task-oriented guides for the CLI, Python, or Rust.
- Go to the language [Reference](../reference/language/syntax.md) if you want exact syntax and semantics.
- Go to the [Reference overview](../reference/index.md) if you want to understand runners, the binary format, and the rest of the project reference.

## Learning path

The recommended order is:

1. [Basics](basics.md)
2. [Pipelines](pipelines.md)
3. [Tabular data basics](tabular-data.md)
4. [Aggregations](aggregations.md)
5. [Projects](projects.md)
