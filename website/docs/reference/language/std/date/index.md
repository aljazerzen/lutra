# `std::date`

## `func` sub

```lutra
func sub(Date, Date): Duration
```

Computes duration between two dates.

Dates are assumed to be in the same timezone.
Duration is measured from one midnight to another.

## `func` to_timestamp

```lutra
func to_timestamp(date: Date, time_zone: Text): Timestamp
```

Computes the timestamp of a local date at a timezone.

## `func` to_year_month_day

```lutra
func to_year_month_day(Date): {year: Int32, month: Uint8, day: Uint8}
```

Converts a date into year, month, and day numbers.
Month and day start with 1.

