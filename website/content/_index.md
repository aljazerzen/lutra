---
title: Lutra
---

<div style="height: 2em"></div>
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Binary format</h2>
</div>


It defines a minimal algebraic type system and binary data format for each of the types.
It provides code generation tools for creating bindings in other programming languages
(currently only Rust is supported).

```
type album: {id: int64, title: text}

type invoice: {id: int64, customer: text, total: float64}
```

<div style="margin-top: 2em">
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Language</h2>
</div>

Lutra programs are a high-level, statically typed language,
which be compiled to lutra-runtime bytecode or
to SQL to be executed by relational databases
(currently only PostgreSQL is supported).

```
type album: {id: int64, title: text}

let get_albums: func (): [album]

let get_album_by_id = func (album_id: int64): album -> (
  get_albums()
  | std::filter(func (this: album) -> this.id == album_id)
  | std::index(0)
)
```

<div style="height: 2em"></div>
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Database driver</h2>
</div>

Can execute Lutra programs on relational databases, while preserving typing information of query parameters and results.

```rust
// main.rs

let client = lutra_db_driver::RunnerSync(...);

let result = client.run(&lutra::my_program(), &4).unwrap();

// result is a Rust struct
println!("id={}, title={}", result.id, result.title);
```

## Why

We believe that many pain points in modern programming stem from losing type information of
the data that is handled by programs.

Sometimes this is fault of the programming language, but in many cases, it is caused by passing
data through type-erasing points.
These can be API calls that return JSON, reading from files, or calling out to a database.
Such type-erasing points do have a notion of *expected data format*.
but the tooling around them lacks the ability to use this information.

The Lutra Project aims to provide this tooling and establish a standardized interface for interaction between software operating across different machines, processes, or programming languages.


## Core principles


### Data should always carry type information

The business logic of programs should be fully typed.
Each variable should have an associated type, either annotated explicitly
or preferably inferred from context.

This improves readability of the program and validates author's assumptions
about written code. This first guides the author when writing code, but also
automates validation of these assumptions when changes to the program are made
in the future.


### Type information should be available to the whole toolchain

Compilers, code editors, language servers and GUI code explorers should all
have access to type information of code.

They should be able to assist development, suggest improvements, organize code and
provide insights into the codebase.

This can be achieved by translating type definitions from one common language (Lutra)
to other languages where the data is used.


### Type information should exist only at compile time

Many data formats carry type information into the run time. For example, JSON stores
field names alongside data: `{"id": 3, "title": "Forrest Gump"}`.

This is inefficient and unnecessary for situations when the program already makes
assumptions about the value in a variable (e.g. `let x: Movie`).

Additionally, runtime reflection increases complexity of the codebase and moves
operations that should have been done at compile time into run time.
