# `std::option` - Nullable values

## `func` is_some

```lutra
func is_some(enum {none, some: T}): bool
where T
```

Returns `true` iff the value is `.some`.

## `func` is_none

```lutra
func is_none(enum {none, some: T}): bool
where T
```

Returns `true` iff the value is `.none`.
The inverse of [`is_some`](#func-is_some).

## `func` or_else

```lutra
func or_else(enum {none, some: T}, T): T
where T
```

When value is `.none`, returns a fallback value instead.

## `func` or_default

```lutra
func or_default(enum {none, some: T}): T
where T
```

When value is `.none`, returns the default value instead.

Default value depends on the type. See [`super::default`](../index.md#import-default).
For numbers it is 0, for booleans it is false.

## `func` zip

```lutra
func zip(enum {none, some: A}, enum {none, some: B}): enum {none, some: {A, B}}
where A, B
```

Zips two options together into an option of a tuple.
Returns `.some({a, b})` only if both `a` and `b` are `.some`.

## `func` map

```lutra
func map(enum {none, some: I}, func (I): O): enum {none, some: O}
where I, O
```

Maps value in `.some`. Does nothing if the value is `.none`.

## `func` flat_map

```lutra
func flat_map(enum {none, some: I}, func (I): enum {none, some: O}): enum {none, some: O}
where I, O
```

Maps value in `.some`. Does nothing if the value is `.none`.

## `func` map_or

```lutra
func map_or(enum {none, some: I}, O, func (I): O): O
where I, O
```

When value is `.none` returns fallback, otherwise maps value in `.some`.

