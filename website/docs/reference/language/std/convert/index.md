# `std::convert` - Type conversions

## `func` default

```lutra
func default(): T
where T
```

Constructs the usual "default" value

For `bool` it is false.
For numbers, this is zero.
For `text` it is an empty text.
For tuples, it a tuple with every field set to default value.
For arrays, it is an empty array.
For enums, it the first variant.

## `func` to_int8

```lutra
func to_int8(T): int8
where T: number
```

Converts a number to `int8`

## `func` to_int16

```lutra
func to_int16(T): int16
where T: number
```

Converts a number to `int16`

## `func` to_int32

```lutra
func to_int32(T): int32
where T: number
```

Converts a number to `int32`

## `func` to_int64

```lutra
func to_int64(T): int64
where T: number
```

Converts a number to `int64`

## `func` to_uint8

```lutra
func to_uint8(T): uint8
where T: number
```

Converts a number to `uint8`

## `func` to_uint16

```lutra
func to_uint16(T): uint16
where T: number
```

Converts a number to `uint16`

## `func` to_uint32

```lutra
func to_uint32(T): uint32
where T: number
```

Converts a number to `uint32`

## `func` to_uint64

```lutra
func to_uint64(T): uint64
where T: number
```

Converts a number to `uint64`

## `func` to_float32

```lutra
func to_float32(T): float32
where T: number
```

Converts a number to `float32`

## `func` to_float64

```lutra
func to_float64(T): float64
where T: number
```

Converts a number to `float64`

## `func` to_text

```lutra
func to_text(T): text
where T: primitive
```

Converts a value to `text`

