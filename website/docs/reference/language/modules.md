---
title: Modules
---

This page describes namespaces, imports, and file-level modules in Lutra.

## Qualified names

Lutra uses `::` to separate module path segments.

```lt
std::cmp
std::option::or_default
project::pg::from_albums
```

## Import definitions

Use `import` to bring a name or module into scope.

```lt
import std::cmp
```

### Aliased imports

You can rename an import with `as`.

```lt
import chinook as c
```

Then use the alias as the module name:

```lt
c::Genre
c::get_albums_by_genre
```

### Grouped imports

You can import several names from the same module path.

```lt
import std::(Date, math::pow)
import std::(flat_map, filter, apply_until_empty)
```

## Module definitions

Use `module` to define a nested module inline.

```lt
module chinook {
  func get_albums(): [Album] -> []

  func get_album_by_id(album_id: int64): Album? -> (
    get_albums()
    | std::find(this -> this.id == album_id)
  )
}
```

Definitions inside a module are referenced through the module path.

```lt
chinook::get_album_by_id(3)
```

## File modules and `submodule`

Lutra builds a module tree from files and directories.

When a directory contains `module.lt`, Lutra also loads:

- files in the same directory that begin with `submodule`, and
- nested directories that contain their own `module.lt`, recursively.

This lets multiple files contribute definitions to the same module path.

```lt
submodule

func hello(a: float64, b: uint64): int64 -> 1
```

Note that compiler will always load the whole project structure, even when
provided with a path to a submodule file.

## Choosing between imports and full paths

You can either:

- use a fully qualified name such as `std::option::or_default`, or
- import the module or symbol first and use the shorter name.

For example, these styles are both valid:

```lt
func main() -> std::date::to_timestamp(@2025-12-16, "UTC")
```

```lt
import std::date::to_timestamp

func main() -> to_timestamp(@2025-12-16, "UTC")
```

## See also

- [Definitions](definitions.md)
- [Expressions](expressions.md)
- [Syntax](syntax.md)
