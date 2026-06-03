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
const my_ints: [Int32] = [1, 2, 3]

func main() -> my_ints | map(x -> x * 2)
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

- Start with [Learn / Basics](basics.md) if you want your first runnable examples.
- Go to [Usage](../usage/cli.md) if you want task-oriented guides for the CLI, Python, or Rust.
- Go to the [Reference](../reference/index.md) if you want exact information about the language, runners, or internals.

## Learning path

The recommended order is:

1. [Basics](basics.md)
2. [Pipelines](pipelines.md)
3. [Tabular data basics](tabular-data.md)
4. [Aggregations](aggregations.md)
5. [Date and time](date-time.md)
6. [Reporting](reporting.md)
7. [Projects](projects.md)
