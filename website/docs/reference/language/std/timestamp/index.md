# `std::timestamp`

## `func` to_date

```lutra
func to_date(timestamp: Timestamp, time_zone: Text): Date
```

Computes the local date of a timestamp at a timezone.

## `func` sub

```lutra
func sub(Timestamp, Timestamp): Duration
```

Computes the elapsed `Duration` between two `Timestamp`s.
Result is `a - b`: positive when `a` is later than `b`.

