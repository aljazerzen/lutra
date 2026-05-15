---
title: Reference
---

<div class="grid cards" markdown>

-   :material-code-braces:{.lg .baseline} [__Language__](./language/syntax)

    ---

    Syntax, literals, types, expressions, patterns, modules

-   :material-play-circle:{.lg .baseline} [__Runtime__](./runtime/runner-model)

    ---

    Runners, CLI, execution flow

-   :material-cogs:{.lg .baseline} [__Internals__](./internals/binary-format)

    ---

    Binary format, type representations

</div>

## Core concepts

- A **project** is a set of `.lt` files, organized as a tree of modules.
- A **program** is compiled code with typed input and output.
- A **runner** is an execution target (interpreter, DuckDB, PostgreSQL).
- The **binary format** carries inputs and outputs across the runner boundary.

## See also

- [Learn](../learn/introduction.md) for the language guides.
- [Usage](../usage/index.md) for task-oriented workflows.
