---
title: Definitions
---

This page describes the top-level definition forms used in Lutra source files.

## Constant definitions

A constant definition uses `const`.
You can provide a type annotation or let the compiler infer the type.

```lt
const a: Int32 = 4
const lang = "sl"
const main = @2025-11-14
```

## Function definitions

A function definition uses `func`.

```lt
func add_one(x: Int64) -> x + 1
func bool_to_text(x: Bool) -> if x then "yes" else "no"
```

### Parameters

A parameter usually has this form:

```lt
name: Type
```

Example:

```lt
func add_one(x: Int64) -> x + 1
```

### Return types

A function can declare its return type after the parameter list.

```lt
func main(): Int32 -> 4
```

If the return type is omitted, the compiler infers it from the body.

```lt
func add_one(x: Int64) -> x + 1
```

### Labeled parameters

A function parameter can have an external label and a local name.
The syntax is `label local_name: Type`.

```lt
func calculate(value: Int32, add addend: Int32, multiply multiplier: Int32) -> (
  value * multiplier + addend
)
```

At the call site, you can pass these parameters positionally or by label:

```lt
calculate(1, 2, 3)
calculate(1, add = 2, multiply = 3)
calculate(1, multiply = 3, add = 2)
```

### Default values

A parameter can declare a default value with `= value`.
The syntax is `name: Type = value`, or `label local_name: Type = value` for a
labeled parameter.

When the caller omits an argument, the function uses the default value.

```lt
func add(x: Int32, y: Int32 = 1) -> x + y

func main() -> {
  add(4),     # 5
  add(4, 3),  # 7
}
```

Default values also work with labeled parameters:

```lt
func wrap(value: Text, left prefix: Text = "[", right suffix: Text = "]") -> (
  f"{prefix}{value}{suffix}"
)

func main() -> {
  wrap("x"),                          # "[x]"
  wrap("x", right = "!"),             # "[x!"
  wrap("x", right = "!", left = "<"), # "<x!"
}
```

A default value must be a constant expression.
It cannot reference other parameters, names from an outer scope, or function
calls.

```lt
# Not allowed: defaults must be constant.
func bad(x: Int32 = helper()) -> x
func bad(x: Int32 = y, y y: Int32) -> x
```

A parameter with a default value can be omitted only when every parameter after
it is labeled.
A required labeled parameter may follow a parameter that has a default:

```lt
func score(bonus: Int32 = 1, player name: Text) -> name

func main() -> {
  score(player = "alice"),    # bonus uses the default
  score(5, player = "bob"),   # bonus passed positionally
}
```

If an unlabeled positional parameter follows a defaulted parameter, the default
can never be used, because the caller must always pass the earlier parameter
positionally to reach the later one.
The compiler reports this as an error.
To fix it, either reorder the parameters or add a label to the later parameter.

### Type parameters with `where`

Functions can have type parameters specified with `where` clause.

```lt
func identity(x: T): T
where T
-> x
```

When you call a function with type parameters, the function specializes for the
types of provided parameters.

For example, the earlier `identity` function can be called with any type and it
will return that same type:

```lt
func main() -> {
  identity(1: Int32),
  identity("hello"),
}
```

A `where` clause can leave a type parameter unconstrained:

- `where T`

It can also constrain the allowed shape or domain of that type parameter:

- `where T: {..}` for any tuple,
- `where T: {x2: Text, ..}` for a tuple with a required named field,
- `where T: {Bool, Text, ..}` for a tuple with required positional fields,
- `where T: Bool | Text` for one of several types.

For the type forms used in these constraints, see [Types](types.md).

Examples:

```lt
func false_x_false(x: T)
where T: {..}
-> {false, ..x, false}
```

## Type definitions

A type definition uses `type`.

### Type aliases

A type alias assigns a name to another type.

```lt
type Item: {
  id: Int64,
  color: enum {red, green: Bool, blue: Bool},
}
```

```lt
type Status: enum {
  open: Int16,
  closed: Bool,
}
```

### Framed types

A type definition can also declare a framed type.
Unlike ordinary tuple and enum types, a framed type is nominal: the type name
is part of the value's identity.

Use a framed type when you want a distinct named type around an inner value.

```lt
type Date(days_epoch: Int32)
```

Construct a framed value by calling the type name:

```lt
const my_date = Date(12)
```

Access the inner value through field lookup:

```lt
const my_days: Int32 = my_date.days_epoch
```

Framed types are not required to name the inner type:

```lt
type ErrorCode(Int32)
```

Inner values of named and unnamed framed types can be accessed with
positional field lookup:

```lt
const err_code: Int32 = my_error_code.0
```

The standard library defines framed standard types such as `Bool`, `Int32`,
`Text`, `Date`, and `Timestamp` using this form.

## Module definitions

A module definition uses `module` and contains nested definitions.

```lt
module chinook {
  func get_albums(): [Album] -> []
}
```

See [Modules](modules.md) for import and namespace rules.

## Entry points

Any program can be invoked as the entry point, although the CLI defaults to
invoking `main`:

```lt
func main() -> 1: Int32
```

## See also

- [Types](types.md)
- [Expressions](expressions.md)
- [Modules](modules.md)
