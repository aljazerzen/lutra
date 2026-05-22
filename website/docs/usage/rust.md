---
title: Rust guide
---

Use Lutra from Rust when you want to:

- compile Lutra projects at build time,
- generate Rust types and program bindings,
- execute typed Lutra programs from Rust,
- choose between local interpreter and PostgreSQL runners.

This page shows the common Rust workflow: keep your Lutra project in the Rust
repository, compile it in `build.rs`, include the generated Rust code, and run
programs from your application.

## Project layout

A small Rust project might look like this:

```text
my_project/
├── Cargo.toml
├── build.rs
└── src/
    ├── main.rs
    └── main.lt
```

The Lutra source lives in `src/main.lt`. The generated Rust bindings are written
to `$OUT_DIR/generated.rs` during the build.

## Write a small Lutra project

```lt title="src/main.lt"
type Movie: {id: int64, title: text, is_released: bool}

func get_movies(): [Movie] -> [
  {id = 54, title = "Hello", is_released = true},
  {id = 3, title = "world", is_released = false},
]
```

## Add Cargo dependencies

```toml title="Cargo.toml"
[build-dependencies]
lutra-codegen = { version = "0.5" }

[dependencies]
lutra-bin = { version = "0.5" }
lutra-interpreter = { version = "0.5" }
tokio = { version = "1", features = ["macros", "rt"] }
```

`lutra-codegen` compiles the Lutra project and generates Rust bindings.
`lutra-bin` is used by the generated code. `lutra-interpreter` is the simplest
runner for local execution.

## Generate bindings in `build.rs`

```rust title="build.rs"
use lutra_codegen as codegen;
use std::{env, path};

fn main() {
    let out_file = path::Path::new(&env::var("OUT_DIR").unwrap()).join("generated.rs");

    let opts = codegen::GenerateOptions::default()
        .generate_programs("", codegen::ProgramRepr::BytecodeLt)
        .generate_client();

    let input_files = codegen::generate("src/main.lt", &out_file, opts);

    for f in input_files {
        println!("cargo::rerun-if-changed={}", f.to_str().unwrap());
    }
}
```

This does two things:

1. generate Rust types that match your Lutra types,
2. generate typed program constructors for your Lutra functions.

## Include the generated code

```rust title="src/main.rs"
mod generated {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}
```

After that, your Rust code can call `generated::get_movies()`, construct a
`generated::Client`, and use the generated `generated::Movie` type.

## Run a program with the interpreter

```rust title="src/main.rs"
mod generated {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let runner = lutra_interpreter::InterpreterRunner::default();
    let client = generated::Client::new_sync(&runner);

    let movies = client.get_movies().await.unwrap().unwrap();

    println!("Result: {movies:#?}");
}
```

For a zero-argument program, the generated client method takes no input.
The result is a Rust `Vec<generated::Movie>`.

## Switch to PostgreSQL

For PostgreSQL-backed programs, change both the generated program repr and the
runtime dependency.

### Add the PostgreSQL runner

```toml title="Cargo.toml"
[dependencies]
lutra-runner-postgres = { version = "0.5" }
```

### Generate PostgreSQL programs in `build.rs`

```rust title="build.rs"
let opts = codegen::GenerateOptions::default()
    .generate_programs("", codegen::ProgramRepr::SqlPg)
    .generate_client();
```

### Run the program from Rust

```rust title="src/main.rs"
mod generated {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let runner = lutra_runner_postgres::RunnerAsync::connect_no_tls(
        "postgres://postgres:pass@localhost:5416",
    )
    .await
    .unwrap();
    let client = generated::Client::new(&runner);

    let movies = client.get_movies().await.unwrap().unwrap();

    println!("{movies:#?}");
}
```

The Rust-side calling style stays almost the same. The main difference is the
runner, the generated client wrapper, and the program repr emitted during code
generation.

## Use the examples in this repository

See the full examples here:

- `examples/rust-interpreter/`
- `examples/rust-postgres/`

These are a good starting point if you want a working end-to-end project.

## See also

- [Command line guide](cli.md) for installation and first-use workflow.
- [Projects](../learn/projects.md) if you want more context on modules and project layout.
- [CLI reference](../reference/runtime/cli.md) for exact `codegen` usage.
[Runner model](../reference/runtime/runner-model.md) if you want to understand how programs move into runners.
[Binary format](../reference/internals/binary-format.md) if you want the low-level data boundary details.
