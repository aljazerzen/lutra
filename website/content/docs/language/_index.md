---
title: Language
weight: 2
---

Lutra is a minimal programming language featuring static typing and an algebraic type system. Designed primarily for interoperability, it serves as a bridge between different programming languages and platforms.

## Showcase

```lt
# Define a module for task operations
module task_ops {
  # Define some types
  type Status: enum {
    Pending,
    InProgress: {started_at: text, owner: text},
    Done: text,
    Failed: text
  }

  type Task: {
    id: int32,
    title: text,
    status: Status,
    priority: int8
  }

  # Get all tasks
  let tasks: [Task] = [
    {id = 1, title = "Write docs", status = Status::Pending, priority = 1},
    {
      id = 2,
      title = "Fix bugs",
      status = Status::InProgress({started_at = "2024-01-01", owner = "Alice"}),
      priority = 3
    },
    {id = 3, title = "Deploy", status = Status::Done("2024-01-02"), priority = 2},
    {id = 4, title = "Test", status = Status::Failed("Invalid config"), priority = 2}
  ]

  # Function to get task status as text
  let get_status_text = func (status: Status) -> match status {
    .Pending => "Pending",
    .InProgress(info) => f"In Progress by {info.owner}",
    .Done(date) => f"Completed on {date}",
    .Failed(reason) => f"Failed: {reason}"
  }

  # Function to get tasks by priority
  let get_by_priority = func (min_priority: int8): [Task] -> (
    tasks
    | std::filter(func (t) -> t.priority >= min_priority)
    | std::sort(func (t) -> t.priority)
  )
}

# Main program that uses these features
let my_program = func () -> {
  # Get high priority tasks
  high_priority = (
    task_ops::get_by_priority(2)
    | std::map(func (t) -> {
      id = t.id,
      title = t.title,
      status = task_ops::get_status_text(t.status),
      metrics = {
        priority = t.priority,
        is_active = match t.status {
          .Pending => true,
          .InProgress => true,
          _ => false
        }
      }
    })
  ),

  # Some stats
  stats = {
    total_tasks = std::count(task_ops::tasks),
    avg_priority = (
      task_ops::tasks
      | std::map(func (t) -> t.priority)
      | std::average()
    ),
    has_pending = (
      task_ops::tasks
      | std::map(func (t) -> match t.status {
        .Pending => true,
        _ => false
      })
      | std::any()
    )
  }
}
```

## Key language features

Basic Types:
- Numeric: `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `float32`, `float64`
- Text: `text`
- Boolean: `bool`
- Arrays: `[ItemType]`
- Tuples: `{field1: Type1, field2: Type2, ...}`
- Enums: `enum {Variant1, Variant2: Type, ...}`

The type system is static and each expression must have a known type at compile time.
Values can be grouped into tuples and arrays, and functions can transform and combine these data structures.

Functions:
- Defined using `func (param: Type) -> ReturnValue`
- Can be assigned to variables with `let`

Standard Library:
- Array operations: map, filter, flat_map, slice, sort
- Aggregations: min, max, sum, count, average
- Boolean ops: all, any, contains
- Window functions: lag, lead, row_number

Pipelines:
- Start with a value, followed by `|` pipe operator,
- Can chain many functions, passing the value into their first argument
 -`(5 | func (x) -> [x, x] | filter(func (x) -> x > 3))`

Pattern Matching:
- Using `match` expressions
- Can destructure enum variants

Modules:
- Code can be organized into modules
- Modules can contain types and functions
- Module members accessed via `::`

## Read more

Read more in following sections:

- [Getting started](./getting-started)
- [Reference](./reference)
