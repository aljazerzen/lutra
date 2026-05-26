# `std::math` - Mathematical ops

## `func` abs

```lutra
func abs(T): T
where T: number
```

Absolute value

## `const` pi32

```lutra
const pi32: float32 = 3.1415927
```

The mathematical constant π as a 32-bit float.

## `const` pi64

```lutra
const pi64: float64 = 3.141592653589793
```

The mathematical constant π as a 64-bit float.

## `func` pow

```lutra
func pow(value: T, exponent: T): T
where T: int64 | float64 | project::Int64 | project::Float64
```

Raises `value` to the power of `exponent`.

