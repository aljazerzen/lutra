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
- [`time`](time/index.md) - Time-of-day helper functions.

## `anno` doc

```lutra
anno doc(const content: Text)
```

Annotates a definition with documentation.

## `anno` hidden

```lutra
anno hidden()
```

Annotates a definition to be hidden from documentation.

## `anno` metadata

```lutra
anno metadata(const name: Text)
```

Annotates a project with metadata.

## `anno` runner

```lutra
anno runner(const url: Text)
```

Annotates a project with default runner URL.

## `anno` rust_derive

```lutra
anno rust_derive(const macros: [Text])
```

Annotates a type with traits to `#[Derive]` when translating to Rust.

## `type` Bool

```lutra
type Bool(prim8)
```

A boolean value.

## `type` Int8

```lutra
type Int8(prim8)
```

A signed 8-bit integer.

## `type` Int16

```lutra
type Int16(prim16)
```

A signed 16-bit integer.

## `type` Int32

```lutra
type Int32(prim32)
```

A signed 32-bit integer.

## `type` Int64

```lutra
type Int64(prim64)
```

A signed 64-bit integer.

## `type` Uint8

```lutra
type Uint8(prim8)
```

An unsigned 8-bit integer.

## `type` Uint16

```lutra
type Uint16(prim16)
```

An unsigned 16-bit integer.

## `type` Uint32

```lutra
type Uint32(prim32)
```

An unsigned 32-bit integer.

## `type` Uint64

```lutra
type Uint64(prim64)
```

An unsigned 64-bit integer.

## `type` Float32

```lutra
type Float32(prim32)
```

A 32-bit floating-point number.

## `type` Float64

```lutra
type Float64(prim64)
```

A 64-bit floating-point number.

## `type` Text

```lutra
type Text([prim8])
```

Unicode text of arbitrary length. Encoded as UTF-8.

## `type` Timestamp

```lutra
type Timestamp(microseconds: Int64)
```

An instant in time. Timestamp without a timezone.

Backed by a signed 64-bit integer, indicating microseconds since the Unix
epoch (1970-01-01T00:00:00.000 UTC), excluding leap seconds.

## `type` Date

```lutra
type Date(days_epoch: Int32)
```

Elapsed days since Unix Epoch (1970-01-01).

Backed by a signed 32-bit integer, representing number of days.

## `type` Duration

```lutra
type Duration(microseconds: Int64)
```

A signed duration of time, unrelated to calendar events.

Backed by a signed 64-bit integer, representing number of microseconds.

## `type` Time

```lutra
type Time(micros_midnight: Uint64)
```

Time of day, in the range [0, 24h).

Backed by an unsigned 64-bit integer, representing microseconds since midnight.

## `func` timezone_offset

```lutra
func timezone_offset(time_zone: Text, date: Date): Duration
```

Returns offset of a time zone from UTC on a given date.

## `type` Decimal

```lutra
type Decimal(prim64)
```

Exact decimal value with a fixed `SCALE`, which is the number of digits past
the decimal point.
Currently, `SCALE` is 2. This will be configurable in the future.
Can hold values from `-10**(19-SCALE)` to `10**(19-SCALE)`.

