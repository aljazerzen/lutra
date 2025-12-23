---
title: 'Use from Rust'
---

Lutra projects can be invoked from Rust, by hooking Lutra compiler into `build.rs`, where it checks the project and generates bindings that can then be called from Rust code.

## Crates

There is few Rust crates that provide different functionality:

- `lutra-bin`: binary format serialization
- `lutra-compiler`: Lutra compiler that can parse and check Lutra projects
- `lutra-codegen`: a utility crate that generates Rust and Python bindings from Lutra projects
- `lutra-interpreter`: a runner that executes programs on a local interpreter
- `lutra-runner-postgres`: a runner that executes programs on PostgreSQL

!!! note

    While Lutra (the project) is in a pre-release state, none of the packages are yet published to crates.io. Instead, I use a custom registry at [codeberg.org](https://codeberg.org/lutra/_cargo-index) to host the packages.


## Basic setup

See [full example](https://codeberg.org/lutra/lutra/src/branch/main/examples/
rust-interpreter).

Let's start with a small Lutra project, in `src/main.lt`:

```lt
# src/main.lt

type Movie: {id: int64, title: text, is_released: bool}

func get_movies(): [Movie] -> [
  {id = 54, title = "Hello", is_released = true},
  {id = 3, title = "world", is_released = false},
]
```

First, we need to add dependencies to the `Cargo.toml` file:

```toml
# Cargo.toml

[build-dependencies]
# For generating bindings
lutra-codegen = {version = "0.2", registry-index = "https://codeberg.org/lutra/_cargo-index.git"}

[dependencies]
# Used by generated bindings
lutra-bin = {version = "0.2", registry-index = "https://codeberg.org/lutra/_cargo-index.git"}
# For running programs
lutra-interpreter = {version = "0.2", registry-index = "https://codeberg.org/lutra/_cargo-index.git"}
```

Now, we call `lutra-codegen` in `build.rs` file:

```rust
// build.rs

use lutra_codegen as codegen;
use std::{env, path};

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    // generated file should reside in $OUT_DIR (./target/.../out)
    let out_file = path::Path::new(&env::var("OUT_DIR").unwrap()).join("generated.rs");

    // we want to compile all programs in the root module
    let opts = codegen::GenerateOptions::default()
        .generate_programs("", codegen::ProgramFormat::BytecodeLt);

    // run codegen
    let input_files = codegen::generate("src/main.lt", &out_file, opts);

    // print all input files, so cargo knows when to rerun codegen
    for f in input_files {
        println!("cargo::rerun-if-changed={}", f.to_str().unwrap());
    }
}
```

This will generate type definitions for `Movie` and compile `get_movies` function.
The produced code will look like this:

```rust
// target/.../out/generated.rs

...

#[derive(Debug, Clone)]
pub struct Movie {
    pub id: i32,
    pub title: String,
}

pub fn get_movies() -> rr::TypedProgram<(), Vec<Movie>> { ... }

...
```

This generated code can be included in the Rust crate.

```rust
// main.rs

// include the file generated in build.rs
mod generated {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use lutra_interpreter::Run;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    // init the runner
    let runner = lutra_interpreter::InterpreterRunner::default();

    // run the program
    let movies = runner
        .run(&generated::get_movies(), &())
        .await
        .unwrap()
        .unwrap();

    // result is a Rust struct
    println!("Result: {movies:#?}");
}
```


## PostgreSQL

See full example [here](https://codeberg.org/lutra/lutra/src/branch/main/examples/
rust-postgres).

Now, the setup is similar to the interpreter example, but we need to use `lutra-runner-postgres` instead of `lutra-interpreter`.

```toml
# Cargo.toml

[dependencies]
lutra-runner-postgres = {version = "0.2", registry-index = "https://codeberg.org/lutra/_cargo-index.git"}
```

Also, we need to tell the compiler emit `sql-pg` program format, that can run on PostgreSQL.

```rs
// build.rs

fn main() {
    // ...
    let opts = codegen::GenerateOptions::default()
        .generate_programs("", codegen::ProgramFormat::SqlPg);
    // ...
}
```

Now, we can connect to PostgreSQL and run the program from `main.rs`.

```rs
// include the file generated in build.rs
mod generated {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use lutra_runner_postgres::RunnerAsync;
use lutra_runner_postgres::Run;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    // init PostgreSQL runner
    const PG_URL: &str = "postgres://postgres:pass@localhost:5416";
    let client = RunnerAsync::connect_no_tls(PG_URL).await.unwrap();

    // fetch movies
    let res = client.run(&generated::get_movies(), &()).await;
    let movies = res.unwrap().unwrap()
    println!("{movies:#?}");
}
```