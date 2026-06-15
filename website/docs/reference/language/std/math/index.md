# `std::math` - Mathematical ops

## `func` abs

```lutra
func abs(T): T
where T: AnyNumber
```

Absolute value

## `const` pi32

```lutra
const pi32: Float32 = 3.1415927
```

The mathematical constant π as a 32-bit float.

## `const` pi64

```lutra
const pi64: Float64 = 3.141592653589793
```

The mathematical constant π as a 64-bit float.

## `func` pow

```lutra
func pow(value: T, exponent: T): T
where T: Int64 | Float64
```

Raises `value` to the power of `exponent`.

