# `std::text` - Text functions

## `func` concat

```lutra
func concat(left: Text, right: Text): Text
```

Concatenates two text values.

## `func` length

```lutra
func length(Text): Uint32
```

Returns the number of characters in a text value.

## `func` from_ascii

```lutra
func from_ascii(Uint8): Text
```

Converts ASCII code to its corresponding Unicode character.
This function always succeeds, because ASCII is a subset of Unicode.

## `func` join

```lutra
func join(parts: [Text], separator: Text): Text
```

Joins parts together, placing separator between each pair.

## `func` split

```lutra
func split(value: Text, separator: Text): [Text]
```

Splits text into substrings at every occurrence of a separator.

For example, `split("1_two_3", "_")` is `["1", "two", "3"]`

## `func` starts_with

```lutra
func starts_with(value: Text, prefix: Text): Bool
```

Tests if a text starts with a prefix.

## `func` contains

```lutra
func contains(value: Text, pattern: Text): Bool
```

Tests if a text contains a pattern.

## `func` ends_with

```lutra
func ends_with(value: Text, suffix: Text): Bool
```

Tests if a text ends with a suffix.

