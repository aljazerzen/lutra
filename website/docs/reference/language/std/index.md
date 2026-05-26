# `std` - Standard library

## Modules

- [`convert`](convert/index.md) - Type conversions
- [`ops`](ops/index.md) - Operators
- [`option`](option/index.md) - Nullable values
- [`array`](array/index.md) - Array functions
- [`math`](math/index.md) - Mathematical ops
- [`text`](text/index.md) - Text functions
- [`fs`](fs/index.md) - File system operations
- [`sql`](sql/index.md) - SQL interface
- [`date`](date/index.md)
- [`timestamp`](timestamp/index.md)

## `anno` doc

```lutra
anno doc(const content: text)
```

Annotates a definition with documentation.

## `anno` hidden

```lutra
anno hidden()
```

Annotates a definition to be hidden from documentation.

## `anno` metadata

```lutra
anno metadata(const name: text)
```

Annotates a project with metadata.

## `anno` runner

```lutra
anno runner(const url: text)
```

Annotates a project with default runner URL.

## `anno` rust_derive

```lutra
anno rust_derive(const macros: [text])
```

Annotates a type with traits to `#[Derive]` when translating to Rust.

## `type` Int8

```lutra
type Int8(int8)
```

A signed 8-bit integer.

## `type` Int16

```lutra
type Int16(int16)
```

A signed 16-bit integer.

## `type` Int32

```lutra
type Int32(int32)
```

A signed 32-bit integer.

## `type` Int64

```lutra
type Int64(int64)
```

A signed 64-bit integer.

## `type` Uint8

```lutra
type Uint8(uint8)
```

An unsigned 8-bit integer.

## `type` Uint16

```lutra
type Uint16(uint16)
```

An unsigned 16-bit integer.

## `type` Uint32

```lutra
type Uint32(uint32)
```

An unsigned 32-bit integer.

## `type` Uint64

```lutra
type Uint64(uint64)
```

An unsigned 64-bit integer.

## `type` Float32

```lutra
type Float32(float32)
```

A 32-bit floating-point number.

## `type` Float64

```lutra
type Float64(float64)
```

A 64-bit floating-point number.

## `type` Timestamp

```lutra
type Timestamp(microseconds: int64)
```

An instant in time. Timestamp without a timezone.

Backed by a signed 64-bit integer, indicating microseconds since the Unix
epoch (1970-01-01T00:00:00.000 UTC), excluding leap seconds.

## `type` Date

```lutra
type Date(days_epoch: int32)
```

Elapsed days since Unix Epoch (1970-01-01).

Backed by a signed 32-bit integer, representing number of days.

## `type` Time

```lutra
type Time(microseconds: int64)
```

Length of time, unrelated to calendar events.
Can be interpreted as duration or offset from the midnight.

Backed by a signed 64-bit integer, representing number of microseconds.

## `func` add_time

```lutra
func add_time(Time, Time): Time
```

Adds two `Time` durations together.

## `func` sub_timestamp

```lutra
func sub_timestamp(Timestamp, Timestamp): Time
```

Computes the elapsed `Time` between two `Timestamp`s.
Result is `a - b`: positive when `a` is later than `b`.

## `func` timezone_offset

```lutra
func timezone_offset(time_zone: text, date: Date): Time
```

Returns offset of a time zone from UTC on a given date.

## `type` Decimal

```lutra
type Decimal(int64)
```

Exact decimal value with a fixed `SCALE`, which is the number of digits past
the decimal point.
Currently, `SCALE` is 2. This will be configurable in the future.
Can hold values from `-10**(19-SCALE)` to `10**(19-SCALE)`.

Backed by a signed 64-bit integer.

