---
title: 'Via Rust'
---

Lutra projects can be compiled to SQL and executed on relational databases.

The Lutra database driver acts as a wrapper around existing drivers. It handles parameter encoding, program execution, and result decoding into data structures native to the target programming language.

Supported databases:

- [PostgreSQL](https://www.postgresql.org/)


## Example

We start with a Lutra project.

```lt
module db {
    type Movie: {id: int32, title: text}

    let movies: func (): [Movie]
}

func main(param: int32) -> (
    db::movies()
    | std::filter(func (x: Movie) -> x.id == param)
    | std::index(0)
)
```

This project can be compiled to Rust type definitions and SQL query using `lutra-codegen` (ideally in `build.rs` file).

Then we can use types and programs from the Lutra project in Rust code, by including generated code in our crate.

```rust
// main.rs

// include code generated from our Lutra project
mod lutra {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use lutra_bin::sr::SyncRun;

fn main() {
    // connect to database
    let mut client = lutra_db_driver::RunnerSync(
        postgres::Client::connect(
            "postgres://postgres:pass@localhost:5432",
            postgres::NoTls,
        ).unwrap(),
    );

    // run the program
    let movie = client.run(&lutra::main(), &4).unwrap();

    // result is a Rust struct
    println!("id={}, title={}", movie.id, movie.title);
}
```
