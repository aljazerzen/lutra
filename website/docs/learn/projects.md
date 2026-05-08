---
title: Projects
---

As your Lutra code grows, you will want to split it across files and modules.

## A file is a module

Each `.lt` file defines a module.

You can place related types, constants, and functions together in one file and
refer to them by module path.

## Import names from other modules

Use `import` to bring names into scope.

```lt
import math::(pi64, pow)

const radius = 5.0: float64

func main() -> pi64 * pow(radius, 2.0)
```

Imports keep code shorter and make dependencies explicit.

## Use module paths for clarity

You can also use fully qualified names.

```lt
func main() -> math::pow(2.0: float64, 3.0)
```

This is useful when you want to make the origin of a name obvious.

## Organize projects around related code

A project usually contains:

- type definitions,
- helper functions,
- one or more entry-point programs,
- backend-specific functions when needed.

Keep the project structure simple at first. Split files when the code becomes
hard to navigate in one place.

## Use `submodule` to split one module across several files

Project modules can be split over multiple `.lt` files.

When a directory contains `module.lt`, Lutra also loads other files in the same
directory that begin with `submodule`. Those files contribute definitions to the
same module path.

For example, imagine this directory:

```text
analytics/
├── module.lt
└── helpers.lt
```

`module.lt` might contain:

```lt
module analytics {
  func main() -> helper()
}
```

And `helpers.lt` might contain:

```lt
submodule

func helper() -> 1
```

In that setup, `helper` becomes part of the `analytics` module.

Use this pattern when one module becomes too large for a single file, but you
still want the definitions to live under the same module path.

## See also

- [Tabular data basics](tabular-basic.md)
- [Aggregations](aggregations.md)
- [Reference: Modules](../reference/language/modules.md)
- [Reference overview](../reference/index.md)
