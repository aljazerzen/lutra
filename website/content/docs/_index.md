---
title: "Overview"
---

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
