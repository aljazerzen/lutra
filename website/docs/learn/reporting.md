---
title: Reporting
---

Reporting code usually takes a table of rows and turns it into summaries.

In Lutra, that usually means:

1. filter the rows you care about
2. group them if needed
3. compute summary values such as totals or counts
4. sort or trim the result for presentation

This page shows common reporting patterns as complete functions.

## Start with a small dataset

These examples use a timesheet-style table.

```lt
type Entry: {
  project: Text,
  user: Text,
  status: enum {open, done},
  started_at: Timestamp,
  ended_at: enum {none, some: Timestamp},
}

const entries: [Entry] = [
  {
    project = "alpha",
    user = "Ada",
    status = .done,
    started_at = @2025-01-02T09:00:00,
    ended_at = .some(@2025-01-02T10:30:00),
  },
  {
    project = "beta",
    user = "Ada",
    status = .done,
    started_at = @2025-01-03T13:00:00,
    ended_at = .some(@2025-01-03T14:00:00),
  },
  {
    project = "alpha",
    user = "Bea",
    status = .open,
    started_at = @2025-01-04T08:00:00,
    ended_at = .none,
  },
]
```

## Compute derived metrics

Your input rows often do not contain the metric in the exact shape you need.
In this example, the rows store `started_at` and `ended_at`, but the report is
interested in durations.

You could repeat that calculation in every report. It is usually better to
factor it into a helper function:

```lt
func entry_duration(entry: Entry): Time -> (
  let ended_at = entry.ended_at | option::or_else(entry.started_at);
  timestamp::sub(ended_at, entry.started_at)
)

func main() -> entry_duration(entries | index(0) | option::or_default())
```

## Summarize all rows into one result

Now, we can summarize all entries into a single row.

```lt
func totals(entries: [Entry]): {count: Int64, duration: Time} -> {
  count = entries | count(),
  duration = entries | map(entry_duration) | sum(),
}

func main() -> totals(entries)
```

This works well when you want one row of totals for a whole result set.

## Group rows by a key

Use `group_map` when you want one summary row per key.
It takes two functions:

- a key extractor
- a mapper that turns each partition into one output row

```lt
func totals_by_project(entries: [Entry]): [{project: Text, duration: Time}] -> (
  entries
  | group_map(
      e -> e.project,
      func (project, partition: [Entry]) -> {
        project = project,
        duration = partition | map(entry_duration) | sum(),
      }
    )
  | sort(x -> x.project)
)
```

This is the basic reporting shape in Lutra:
partition the rows by a key, then map each partition to its summary.

## Compute several metrics per group

A grouped summary can return more than one metric.

```lt
type ProjectMetric: {project: Text, count: Int64, duration: Time}

func project_metrics(entries: [Entry]): [ProjectMetric] -> (
  entries
  | group_map(
      e -> e.project,
      func (project, partition: [Entry]) -> (
        let summary = totals(partition);
        {
          project = project,
          count = summary.count,
          duration = summary.duration,
        }
      )
    )
  | sort(x -> x.project)
)
```

The named `ProjectMetric` type keeps the function signature short and gives the
result shape a reusable name.

This example also reuses `totals` from the earlier section.
That keeps the per-project logic focused on grouping rather than rewriting the
same summary logic again.

## Filter before you aggregate

Reporting queries often apply business rules before aggregation.
For example, you might want to report only completed entries.

```lt
func completed_metrics_by_project(entries: [Entry]): [ProjectMetric] -> (
  entries
  | filter(e -> match e.status {
      .done => true,
      .open => false,
    })
  | project_metrics()
)
```

Filtering first keeps the grouped logic small and makes the rule easier to read.

## Sort aggregated results and take the top N

A report often needs the largest groups rather than every group.
After aggregation, sort by the metric you care about and slice the result.

```lt
func top_projects(entries: [Entry], n: Int64): [ProjectMetric] -> (
  entries
  | project_metrics()
  | sort(x -> -x.duration)
  | slice(0, n)
)
```

## See also

- [Date and time](date-time.md)
- [Aggregations](aggregations.md)
- [Tabular data basics](tabular-data.md)
