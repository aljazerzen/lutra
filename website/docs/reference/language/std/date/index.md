# `std::date`

## `func` sub

```lutra
func sub(super::Date, super::Date): super::Time
```

Computes time duration between two dates.

Dates are assumed to be in the same timezone.
Duration is measured from one midnight to another.

## `func` to_timestamp

```lutra
func to_timestamp(date: super::Date, time_zone: text): super::Timestamp
```

Computes the timestamp of a local date at a timezone.

## `func` to_year_month_day

```lutra
func to_year_month_day(super::Date): {year: int32, month: uint8, day: uint8}
```

Converts a date into year, month, and day numbers.
Month and day start with 1.

