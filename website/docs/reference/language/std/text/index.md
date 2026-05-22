# `std::text` - Text functions

## `func` concat

```lutra
func concat(left: text, right: text): text
```

Concatenates two text values.

## `func` length

```lutra
func length(text): uint32
```

Returns the number of characters in a text value.

## `func` from_ascii

```lutra
func from_ascii(uint8): text
```

Converts ASCII code to its corresponding Unicode character.
This function always succeeds, because ASCII is a subset of Unicode.

## `func` join

```lutra
func join(parts: [text], separator: text): text
```

Joins parts together, placing separator between each pair.

## `func` split

```lutra
func split(value: text, separator: text): [text]
```

Splits text into substrings at every occurrence of a separator.

For example, `split("1_two_3", "_")` is `["1", "two", "3"]`

## `func` starts_with

```lutra
func starts_with(value: text, prefix: text): bool
```

Tests if a text starts with a prefix.

## `func` contains

```lutra
func contains(value: text, pattern: text): bool
```

Tests if a text contains a pattern.

## `func` ends_with

```lutra
func ends_with(value: text, suffix: text): bool
```

Tests if a text ends with a suffix.

