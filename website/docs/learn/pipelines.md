---
title: Pipelines
---

Pipelines are the most common way to structure everyday Lutra code.
They let you read a transformation from left to right: start with a value, apply
one step, then apply the next.

## Define small functions

A function takes input values and returns an output value.

```lt
func greet(name: Text) -> f"Hello, {name}!"

func add(a: Int32, b: Int32) -> a + b

func main() -> {greet("Ada"), add(3, 4)}
```

Functions can infer their return type when it is obvious. You can also annotate
it explicitly.

```lt
func is_even(x: Int32): Bool -> x % 2 == 0
```

In practice, Lutra code often uses:

- small named helper functions,
- inline functions inside pipelines,
- tuples to keep related results together.

## Use constants for fixed values

Constants are useful for fixed values that do not require computation.

```lt
const tax_rate: Float64 = 0.2

func with_tax(price: Float64) -> price * (1.0 + tax_rate)
```

Use `func` when you need computation. Use `const` when you need a reusable
value.

## Read pipelines from left to right

A pipe sends the value on the left into the expression on the right.

```lt
func add_one(x: Int32) -> x + 1
func double(x: Int32) -> x * 2

func main() -> 10 | add_one | double
```

This is equivalent to:

```lt
func main() -> double(add_one(10))
```

Pipelines become more useful as soon as you have more than one or two steps.
They make the flow of data visible.

## Use inline functions inside pipelines

You can write a function directly where you need it.

```lt
func main() -> (
  [1, 2, 3]: [Int32]
  | map(x -> x * 2)
)
```

Inline functions are common with `map`, `filter`, `sort`, `group`, and related
operations.

## Use `let` for intermediate results

`let` introduces a local name inside an expression.

```lt
func main() -> (
  let subtotal = 50: Int32;
  let tax = 10: Int32;
  subtotal + tax
)
```

This is helpful when:

- a calculation would otherwise be repeated,
- an expression is becoming hard to read,
- you want to name a meaningful intermediate value.

## Return tuples from transformation steps

Functions often return tuples so that related values stay together.

```lt
func summarize(name: Text, score: Int32) -> {
  name = name,
  passed = score >= 50,
  score = score,
}

func main() -> summarize("Ada", 88)
```

This becomes especially important in data work, where each pipeline step often
reshapes one tuple into another.

## Branch inside a pipeline step

Sometimes a transformation depends on a condition.
Use `if` when you already have a boolean condition and only need two branches.

```lt
func label(x: Int32) -> if x > 0 then "positive" else "zero or negative"

func main() -> [label(3), label(-1)]
```

Use `match` when you want to branch on enum variants.

```lt
type Status: enum {
  draft,
  published,
  scheduled: Text,
}

func to_text(status: Status) -> match status {
  .draft => "draft",
  .published => "published",
  .scheduled(date) => f"scheduled for {date}",
}

func main() -> to_text(.scheduled("2026-01-01"))
```

Patterns can also bind values:

```lt
type Animal: enum {
  cat: Text,
  dog: Text,
}

func greet_animal(animal: Animal) -> match animal {
  .cat(name) => f"Hello, {name}!",
  .dog(name) => f"Who's a good dog, {name}?",
}
```

And they can combine cases:

```lt
type Kind: enum {cat, dog, hamster}

func needs_walk(kind: Kind) -> match kind {
  .cat | .hamster => false,
  .dog => true,
}
```

Use `_` when you do not care about the matched value.

## Prefer readable pipelines over nested calls

This is harder to read:

```lt
func main() -> sum(map([1, 2, 3]: [Int32], x -> x * 2))
```

This is usually better:

```lt
func main() -> (
  [1, 2, 3]: [Int32]
  | map(x -> x * 2)
  | sum()
)
```

The benefit becomes even clearer with tuple-shaped rows:

```lt
const movies = [
  {id = 1: Int32, title = "Arrival", year = 2016: Int32},
  {id = 2: Int32, title = "Dune", year = 2021: Int32},
]

func main() -> (
  movies
  | map(m -> {
    m.id,
    title = m.title,
    is_recent = m.year >= 2020,
  })
)
```

That left-to-right style is the foundation of tabular-data programming in
Lutra.

## See also

- [Basics](basics.md)
- [Tabular data basics](tabular-data.md)
- [Aggregations](aggregations.md)
- [Reference: Expressions](../reference/language/expressions.md)
- [Reference: Patterns](../reference/language/patterns.md)
- [Reference: Operators](../reference/language/operators.md)
