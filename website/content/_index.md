---
title: Lutra
---


<p class="muted" style="margin-top: 2em; margin-bottom: -1.5em">Lutra is a ...</p>

## Language

... for preserving type information between different software components.
It is a high-level, statically typed language, designed for querying data
and expressing data structures.

```lt
type Album: {id: int16, title: text}

let get_albums: func (): [Album]

let get_album_by_id = func (album_id: int16): Album -> (
  get_albums()
  | filter(func (this: Album) -> this.id == album_id)
  | index(0)
)
```

<p class="muted" style="margin-top: 2em; margin-bottom: -1.5em">Lutra is a ...</p>

## Binary format

... with a minimal algebraic type system and tooling for creating bindings in other programming languages.
It focuses on simplicity and ease of use, but also provides partial decoding capabilities.

```
| id    | title                                                  |
| 05 00 | 08 00 00 00 | 09 00 00 00 | 47 6c 61 64 69 61 74 6f 72 |
| int16 | offset      | length      | contents                   |
```

<p class="muted" style="margin-top: 2em; margin-bottom: -1.5em">Lutra is a ...</p>

## Runner

... for executing Lutra programs on relational databases or locally, while preserving typing information of program inputs and outputs.
This allows writing code that is portable across different execution targets and
provides a modern, well-designed interface for interacting with databases.

```rust
// main.rs
let runner = lutra_db_driver::RunnerSync(...);

let result = runner.run(&lutra::my_program(), &4)?;

// result is a Rust struct
println!("id={}, title={}", result.id, result.title);
```

## Why

Many challenges in modern programming arise from losing type information within a system. Often this is the fault of the programming language, but in many cases it is caused by passing data through type-erasing points.

For example, API calls that return JSON, reading from files, or calling out to a database all lose type information.
While these interfaces usually have a notion of *expected data format*, most tools fail to express this information in a way that can be used by the toolchain and the programmer.

The Lutra Project aims to provide this tooling and establish a standardized interface for interaction between software operating across different machines, processes, or programming languages.


## Core principles


### Data should always carry type information

The business logic of programs should be fully statically typed.
Each variable should have an associated type, either annotated explicitly
or preferably inferred from context.

This improves readability of the program and validates the author's assumptions
about written code. It first guides the author when writing code, and also
automates validation of these assumptions when changes to the program are made
in the future.


### Type information should be available to the whole toolchain

Development tools like compilers, editors, language servers and code explorers should all
have access to complete type information.

They should be able to assist development, suggest improvements, organize code and
provide insights into the codebase.

This can be achieved by translating type definitions from one common language (Lutra)
to other languages where the data is used.


### Type information should exist only at compile time

Many data formats carry type information into runtime. For example, JSON stores
field names alongside data: `{"id": 3, "title": "Gladiator"}`.

This is inefficient and unnecessary for situations when the program already makes
assumptions about the value in a variable (e.g. `let x: Movie`).

Additionally, runtime reflection increases the complexity of the codebase and moves
operations that should have been done at compile time into runtime.
