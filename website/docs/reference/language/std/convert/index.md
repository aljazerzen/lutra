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
func to_int8(T): Int8
where T: number
```

Converts a number to `int8`

## `func` to_int16

```lutra
func to_int16(T): Int16
where T: number
```

Converts a number to `int16`

## `func` to_int32

```lutra
func to_int32(T): Int32
where T: number
```

Converts a number to `int32`

## `func` to_int64

```lutra
func to_int64(T): Int64
where T: number
```

Converts a number to `int64`

## `func` to_uint8

```lutra
func to_uint8(T): Uint8
where T: number
```

Converts a number to `uint8`

## `func` to_uint16

```lutra
func to_uint16(T): Uint16
where T: number
```

Converts a number to `uint16`

## `func` to_uint32

```lutra
func to_uint32(T): Uint32
where T: number
```

Converts a number to `uint32`

## `func` to_uint64

```lutra
func to_uint64(T): Uint64
where T: number
```

Converts a number to `uint64`

## `func` to_float32

```lutra
func to_float32(T): Float32
where T: number
```

Converts a number to `float32`

## `func` to_float64

```lutra
func to_float64(T): Float64
where T: number
```

Converts a number to `float64`

## `func` to_text

```lutra
func to_text(T): Text
where T: primitive
```

Converts a value to `text`

