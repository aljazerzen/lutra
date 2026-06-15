# `std::time` - Time-of-day helper functions.

## `func` to_duration

```lutra
func to_duration(Time): Duration
```

Converts a `Time` (time-of-day) into a `Duration` from midnight.

## `func` diff

```lutra
func diff(Time, Time): Duration
```

Computes the signed duration from `b` to `a` (`a - b`).
The result may be negative.

## `func` add

```lutra
func add(Time, Duration): Time
```

Adds a `Duration` to a `Time`, wrapping within `[0, 24h)`.

## `func` sub

```lutra
func sub(Time, Duration): Time
```

Subtracts a `Duration` from a `Time`, wrapping within `[0, 24h)`.

