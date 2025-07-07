---
permalink: /docs
layout: docs.liquid
title: "Lutra: documentation"
---

Lutra is:

- ... a [language](/language) for defining data structures and high-level, statically typed programs,

- ... a [binary format](/binary) for serializing structured data,

- ... a [database driver](/database) for working with relational databases without losing typing information.


## Overview

Basic block of compilation is a **project**.
Usually, this is a directory of `.lt` files, which contain type definitions and the code of the functions.

```elm
# project.lt

type Movie: {id: int32, title: text}

let get_movie = func (param: int32) -> (
    [
        {id = 3, title = "Forrest Gump"},
        {id = 7, title = "Conclave"},
    ]
    | std::filter(func (x: Movie) -> x.id == param)
    | std::index(0)
)

let my_program = func() -> get_movie(3)
```

The compiler is able to read these files, perform name resolution, type checking inference and report any problems with this code.
From this checked project, it can then produce programs.

```
> lutra check project.lt
All good.
```

A **program** contains the compiled code and type annotations for its input and output value.

A **runner** can execute programs. It consumes the input and produces output, both encoded in the binary format.
Runner can be an interpreter (`interpreter-br`) or a driver that connects to a database and executes the program there (`sql-postgres`).

```
> lutra run project.lt my_program
{
  id = 3,
  title = "Forrest Gump",
}
```

To execute a program from some other programming language (such as Rust or Python), it is recommended to use **codegen** to generate type definitions in the target language. These definitions can encode inputs and decode outputs of the programs.

```rust
// build.rs
fn main() {
    lutra_codegen::generate("project.lt", "generated.rs");
}
```

```rust
// target/.../out/generated.rs

...

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct Movie {
    pub id: i32,
    pub title: String,
}

pub fn my_program() -> sr::TypedProgram<(), Movie> { ... }

...
```

```rust
// main.rs
mod generated {
    // include code generated from our Lutra project
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

fn main() {
    // init the runner
    let mut runner = lutra_db_driver::RunnerSync(
        postgres::Client::connect(
            "postgres://postgres:pass@localhost:5432", postgres::NoTls,
        ).unwrap(),
    );

    // run the program
    let movie = runner.run(&generated::my_program(), &3).unwrap();

    // result is a Rust struct
    println!("id={}, title={}", movie.id, movie.title);
}
```

## Mission

We believe that many pain-points in modern programming stem from loosing type information of
the data that is handled by programs.

Sometimes this if fault of the programming language, but in many cases, it is caused by passing
data trough type-erasing points.
These can be API calls that return JSON, reading from files or calling out to a database.
Such type-erasing points do have a notion of *expected data format*,
but the tooling around them lacks the ability to use this information.

Lutra Project aims to provide this tooling and establish a standardized interface for interaction between software operating across different machines, processes, or programming languages.


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
