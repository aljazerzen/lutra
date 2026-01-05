---
title: Showcase
---

Lutra is a minimal programming language featuring static typing and an algebraic type system. Designed primarily for interoperability, it serves as a bridge between different programming languages and platforms.

```lt
# Define a module for task operations
module task_ops {
  import std::Date

  # Define some types
  type Status: enum {
    pending,
    in_progress: {started_at: Date, owner: text},
    done: Date,
    failed: text,
  }

  type Task: {id: int32, title: text, status: Status, priority: int8}

  # Get all tasks
  const tasks: [Task] = [
    {id = 1, title = "Write docs", status = .pending, priority = 1},
    {
      id = 2,
      title = "Fix bugs",
      status = .in_progress({started_at = @2024-01-01, owner = "Alice"}),
      priority = 3,
    },
    {id = 3, title = "Deploy", status = .done(@2024-01-02), priority = 2},
    {id = 4, title = "Test", status = .failed("Invalid config"), priority = 2},
  ]

  # Function to get task status as text
  func get_status_text(status: Status) -> match status {
    .pending => "pending",
    .in_progress(info) => f"In Progress by {info.owner}",
    .done(_) => f"Completed",
    .failed(reason) => f"failed: {reason}",
  }

  # Function to get tasks by priority
  func get_by_priority(min_priority: int8): [Task] -> (
    tasks
    | std::filter(func (t) -> t.priority >= min_priority)
    | std::sort(func (t) -> t.priority)
  )
}

# Main program that uses these features
func my_program() -> {
  # Get high priority tasks
  high_priority = (
    task_ops::get_by_priority(2)
    | std::map(func (t) -> {
      id = t.id,
      title = t.title,
      status = project::task_ops::get_status_text(t.status),
      metrics = {
        priority = t.priority,
        is_active = match t.status {
          .pending => true,
          .in_progress(_) => true,
          _ => false,
        },
      },
    })
  ),

  # Some stats
  stats = {
    total_tasks = std::count(task_ops::tasks),
    avg_priority = (
      task_ops::tasks | std::map(func (t) -> t.priority) | std::mean()
    ),
    has_pending = (
      task_ops::tasks
      | std::map(func (t) -> match t.status {
        .pending => true,
        _ => false,
      })

      | std::any()
    ),
  },
}
```
