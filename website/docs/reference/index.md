---
title: Reference
---

Use the reference when you want exact information about Lutra.
It is organized for lookup, not as a tutorial.

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

## Runtime concepts

- A **runner** is an execution target (interpreter, DuckDB, PostgreSQL).
- A **program** is compiled code with typed input and output.
- The **binary format** carries inputs and outputs across the runner boundary.

## Internals concepts

- The **binary format** is a compact byte encoding for values crossing the runner boundary.
- **Type representations** define how each Lutra type maps to storage in different backends.
- The **format spec** defines the exact byte-level encoding rules.

## See also

- [Learn](../learn/introduction.md) for the language guides.
- [Usage](../usage/index.md) for task-oriented workflows.
