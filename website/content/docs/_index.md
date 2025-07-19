---
title: "Overview"
---

## Project

The starting point for Lutra is a **project**.
Usually, this is a directory of `.lt` files, which contain type definitions and the code of the functions.

```lt
# project.lt

type Movie: {id: int32, title: text}

let my_movies: [Movie] = [
  {id = 3, title = "Forrest Gump"},
  {id = 7, title = "Conclave"},
]

let get_movie = func (param: int32) -> (
  my_movies
  | std::filter(func (x: Movie) -> x.id == param)
  | std::index(0)
)

let my_program = func() -> get_movie(3)
```

The compiler can read these files, perform name resolution, type checking, and inference, and report any issues found in the code.
Once the project is successfully checked, it can then generate executable programs.

```
> lutra check project.lt
All good.
```

## Program

A **program** consists of compiled code along with type annotations for its input and output values.
It is produced by compiler and embedded into the calling program. For example, when using Rust,
it is most convenient to compile Lutra projects from build.rs script and include them in the compiled
Rust binary via `include_bytes!`.

## Runner

A **runner** executes programs by taking input and producing output, both in binary format.
Currently, there are two runners available:

- `interpreter-br`: a local interpreter that executes programs in the same process,
- `sql-postgres`: a PostgreSQL runner that executes SQL programs in a database.

```
> lutra run project.lt my_program
{
  id = 3,
  title = "Forrest Gump",
}
```

To call programs from other languages (Rust, Python, etc.), **codegen** can be used to generate type definitions in the target language.
The generated code also handles serialization of inputs and deserialization of outputs.

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
