---
title: Date and time
---

Lutra has separate types for calendar dates, clock times, and instants in time.
That separation helps you write reporting code without mixing up local calendar
logic and UTC instants.

Use these types for different jobs:

- `Date` for a calendar day such as `@2025-11-14`
- `Time` for a duration or clock-like time value such as `@16:07:44`
- `Timestamp` for an instant in time such as `@2025-12-29T16:07:44`

## Understand the three core types

`Date` stores a calendar day.
Internally, it is backed by `days_epoch`.

```lt
func main() -> {
  date = @2025-11-14,
  days_epoch = @2025-11-14.days_epoch,
  same_value = @2025-11-14.0,
}
```

`Time` stores a signed number of microseconds.
You will often use it as a duration.

```lt
func main() -> {
  short = @00:15:00,
  micros = @00:15:00.microseconds,
}
```

`Timestamp` stores a UTC instant with microsecond precision.
It does not carry an embedded timezone.

```lt
func main() -> {
  ts = @2025-12-29T16:07:44,
  micros = @2025-12-29T16:07:44.microseconds,
}
```

## Use literals when you need fixed values

Lutra has built-in literal syntax for all three types:

```lt
func main() -> {
  date = @2025-11-14,
  time = @16:07:44,
  timestamp = @2025-12-29T16:07:44,
}
```

## Convert a local date to a timestamp

Use `date::to_timestamp(date, tz)` when you want the UTC instant for midnight
at the start of a local day.

```lt
func start_of_day() -> date::to_timestamp(@2026-06-03, "Europe/Ljubljana")
```

This is a common reporting boundary.
For example, you might turn a user-selected local date into the timestamp range
that you pass to a filter.

## Extract year, month, and day from a date

Use `date::to_year_month_day` when you need numeric calendar parts.

```lt
func main() -> date::to_year_month_day(@2025-12-29)
```

That returns:

```lt
{year = 2025, month = 12, day = 29}
```

This is useful when you need a stable calendar key or want to derive the first
of a month.

## Convert a timestamp to a local date

Use `timestamp::to_date(ts, tz)` when you need local calendar grouping.

```lt
func main() -> timestamp::to_date(@2025-12-31T23:30:00, "Europe/Ljubljana")
```

This program returns `@2026-01-01`, because `Europe/Ljubljana` timezone is an hour
ahead of UTC, which means that the local date already falls in the next year.

This conversion is the key rule for calendar reporting:
`Timestamp` is an instant, but reports are often grouped by local day, week, or
month.

## Compute durations

Use `timestamp::sub` for elapsed time between two instants.

```lt
func main() -> timestamp::sub(
  @2025-12-29T16:30:00,
  @2025-12-29T16:07:44,
)
```

Use `date::sub` when you want the duration between two dates.

```lt
func main() -> date::sub(@2025-12-29, @2025-12-01)
```

Both results are `Time` values.

## Check timezone offsets when you need them

Use `timezone_offset(time_zone, date)` when you need the UTC offset that
applies on a local date.

```lt
func main() -> timezone_offset("Europe/Ljubljana", @2025-07-01)
```

This is most useful when you are debugging timezone-sensitive logic.
Most reporting code can stay at the level of `timestamp::to_date` and
`date::to_timestamp`.

## Group rows by local calendar month

When you group timestamps by month, convert them to a local `Date` first.
Then derive a stable month key.

```lt
type Event: {
  ts: Timestamp,
  amount: Int64,
}

const events: [Event] = [
  {ts = @2025-01-01T00:30:00, amount = 10},
  {ts = @2025-01-15T12:00:00, amount = 15},
  {ts = @2025-02-01T09:00:00, amount = 7},
]

func totals_by_month(events: [Event], time_zone: Text): [{month: Int32, total: Int64}] -> (
  events
  | group_map(
      e -> (
        let d = e.ts | timestamp::to_date(time_zone);
        let parts = d | date::to_year_month_day;
        d.days_epoch - ((parts.day | to_int32) - 1)
      ),
      func (month, rows) -> {
        month = month,
        total = rows | map(r -> r.amount) | sum(),
      }
    )
  | sort(x -> x.month)
)
```

The key idea is not the arithmetic itself.
The key idea is the order of operations:

1. convert the instant to a local `Date`
2. derive the calendar bucket from that date
3. aggregate within that bucket

## See also

- [Runners](runners.md)
- [Reporting](reporting.md)
- [Aggregations](aggregations.md)
- [Reference: Literals](../reference/language/literals.md)
- [Reference: Types](../reference/language/types.md)
- [Reference: `std::date`](../reference/language/std/date/index.md)
- [Reference: `std::timestamp`](../reference/language/std/timestamp/index.md)
