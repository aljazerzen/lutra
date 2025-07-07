---
title: Reference
weight: 2
---

Lutra is a statically typed programming language with a minimal
algebraic type system and some aspects of functional paradigm.


## Type system

**Primitives** include following types:

- `bool` holds one bit of information. Possible values are `false` and `true`.
- `int8`, `int16`, `int32`, `int64` are signed integers. `int{n}` holds `n` bits of information.
  Possible values are first `2**(n-1)` non-negative integers and first `2**(n-1)` negative integers
- `uint8`, `uint16`, `uint32`, `uint64` are unsigned integers. `uint{n}` holds `n` bits of information.
  Possible values are first `2**n` non-negative integers.
- `float32`, `float64` are IEEE 754 single and double precision floating point numbers, respectively.
- `text` is UTF8 encoded string of characters of length at most `2**32 - 1`.

**Tuples** are a product of possibly many different types. Each field of the tuple can be named.

```
type my_tuple = {float32, my_bool = bool, my_int = int32}
```

**Arrays** are repetitions of one single type.

```
type array_of_bytes = [uint8]
```

**Enums** are a sum type of possibly many different types. Each variant must be named.

```
type status = enum {
  done = {},
  in_progress = uint16,
  cancelled = {reason = text},
}
```

## Expressions

Primitive types can be expressed with literals:

```
# bool
true
false

# int64
1
432
-23

# float64
42.801
-32.2

# text
"hello world"
```

Tuples can be constructed using curly braces and
can contain arbitrary expressions of different types:

```
{true, 10}
```

Each of tuple fields may be prefixed by a name:

```
{a = true, 10, b = 5.4}
```

Arrays can be constructed using square brackets and
can contain arbitrary expressions. All items must be of
the same type. Array items cannot be named.

```
[true, false, true]

[10, 33, -3, 2, 40]
```

Tuple field lookup can be expressed using `.` followed by
the name or 0-based position of the field.

```
{a = 10, false}.a # results in 10
{a = 10, false}.1 # results in false
```

Function calls are expressed by the name of the function
followed by parenthesis, which contain function arguments.

```
my_func(4, false)

std::add(6, 2)
```

Pipelines are alternative notation for function call which
are more convenient for chaining. The notation consists of
parenthesis that contain expressions separated by pipe symbol `|`.
Using a pipe symbol between two expressions is syntactically equivalent
to using the first expression as the first argument to the call of the second
expression.
All expressions apart from the first are therefore limited to function calls,
names and inline functions.

Each of the following notations are equivalent:
- `my_func(5)`
- `5 | my_func()`
- `5 | my_func`

The following notations are equivalent:
- `another_func(my_func(5), false)`
- `5 | my_func | another_func(false)`

Following binary and unary operators use standard infix and prefix notation, respectively,
and are equivalent to calls to corresponding standard library functions.

Binary:
- `* std::mul`
- `/ std::div`
- `% std::mod`
- `+ std::add`
- `- std::sub`
- `== std::eq`
- `!= std::ne`
- `>  std::gt`
- `<  std::lt`
- `>= std::gte`
- `<= std::lte`
- `&& std::and`
- `|| std::or`

Unary:
- `- std::neg`
- `! std::not`

Ranges are syntactic sugar for constructing tuples of form
`{start = _, end = _}` and use `..` infix notation.

The following notations are equivalent:
- `3..10`
- `{start = 3, end = 10}`

Functions can be defined as follows:

```
func (x: T, y: U): V -> ...
```

where:
- `x` and `y` are two function arguments,
- `T` and `U` are their required types, respectively,
- `V` is the function return type,
- `...` is the expression that computes the return value.

Return type of the function is optional if it can be inferred
from the function body.

## Declarations

Lutra source files are a sequences of declarations of variables,
types and modules.

Variable declarations use keyword `let`, followed by the name,
the type and the expression of the assigned value.

```
let my_variable: int64 = 5
```

Type may be omitted it can be inferred from the value expression.

Type declarations are use keyword `type` followed by the name and the type.

```
let my_type = int64
```

Module declarations use keyword `module`. Modules are a namespace for
other declarations.

```
module my_module {
  let my_variable = 3
}
```

