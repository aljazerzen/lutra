---
title: DuckDB
---

This page documents DuckDB-specific behavior for Lutra programs.

## Using DuckDB from the CLI

The CLI can execute Lutra programs directly on DuckDB.

Use an in-memory database:

```console
$ lutra run --project ./project.lt --runner duckdb
```

Use a file-backed database:

```console
$ lutra run --project ./project.lt --runner duckdb:data.duckdb
$ lutra interactive --project ./project.lt --runner duckdb:data.duckdb
```

You can also make DuckDB the project default:

```lt
@!runner("duckdb:data.duckdb")
```

## Using DuckDB from Rust

Add the DuckDB runner and generate `SqlDuckdb` programs in `build.rs`.

```toml title="Cargo.toml"
[build-dependencies]
lutra-codegen = { version = "0.5" }

[dependencies]
duckdb = { version = "1.4" }
lutra-bin = { version = "0.5" }
lutra-runner-duckdb = { version = "0.5" }
```

```rust title="build.rs"
use lutra_codegen as codegen;

fn main() {
    codegen::check_and_generate(
        "src/main.lt",
        codegen::GenerateOptions::default()
            .generate_programs("", codegen::ProgramRepr::SqlDuckdb)
            .generate_client(),
    );
}
```

## Using DuckDB from Python

Python code generation still works for DuckDB-backed projects, because it is
independent of the runtime backend. However, this repository currently documents
and ships Python runtime bindings for PostgreSQL, not a dedicated
`lutra-runner-duckdb` Python package.

Today, DuckDB execution is the supported path from:

- the CLI, via `--runner duckdb` or `--runner duckdb:PATH`,
- Rust, via `lutra-runner-duckdb`.

If you generate Python models for a project that also targets DuckDB, treat the
DuckDB execution step as a CLI or Rust concern.

## Type mapping

Lutra uses DuckDB native types for framed standard types in `sql::from`,
`sql::insert`, and `sql::raw`. These rules apply no matter whether the
program is launched from the CLI or through Rust.

| Lutra type | DuckDB column type |
|---|---|
| `Bool` | `BOOL` |
| `Int8`, `Int16`, `Int32`, `Int64` | `INT1`, `INT2`, `INT4`, `INT8` |
| `Uint8`, `Uint16`, `Uint32`, `Uint64` | `UINT8`, `UINT16`, `UINT32`, `UINT64` |
| `Float32`, `Float64` | `FLOAT4`, `FLOAT8` |
| `Text` | `TEXT` |
| `Date` | `DATE` |
| `Time` | `TIME` |
| `Duration` | `INT8` |
| `Timestamp` | `TIMESTAMP` |
| top-level tuple `{id: Int64, name: Text}` | `id INT8, name TEXT` |
| nested tuple `{x: {a: Int32, b: Int32}}` | `x STRUCT(a INT4, b INT4)` |
| nested array `[Text]` inside another value | `TEXT[]` |
| enum with payloads | `UNION(...)` |
| `enum {none, some: Int32}` | nullable `INT4` |

A top-level array is stored as one row per item, using the columns of the item
shape.

## Minimal example

The same table layout is used whether you interact with DuckDB from the CLI or
from Rust.

```lt title="src/main.lt"
import std::Date

type Movie: {
  id: Int64,
  title: Text,
  released_on: Date,
}

func insert_movies(movies: [Movie]) -> std::sql::insert(movies, "movies")
func get_movies(): [Movie] -> std::sql::from("movies")
```

Create the table in DuckDB:

```sql
CREATE TABLE IF NOT EXISTS movies (
  id INT8 PRIMARY KEY,
  title TEXT,
  released_on DATE
);
```

Run against that database from the CLI:

```console
$ lutra run --project ./src/main.lt --runner duckdb:movies.duckdb --program 'get_movies()'
```

Or execute the generated `sql-duckdb` program from Rust:

```rust title="src/main.rs"
mod generated {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let db_path = "movies.duckdb";

    let conn = duckdb::Connection::open(db_path)?;
    conn.execute_batch(
        r#"
        CREATE TABLE IF NOT EXISTS movies (
          id INT8 PRIMARY KEY,
          title TEXT,
          released_on DATE
        );
        "#,
    )?;
    drop(conn);

    let mut runner = lutra_runner_duckdb::Runner::open(db_path, None)?;
    let mut client = generated::Client::new_sync(&mut runner);

    let movies = vec![generated::Movie {
        id: 54,
        title: "Hello".into(),
        released_on: generated::Date { days_epoch: 20_089 },
    }];

    client.insert_movies(&movies)?.unwrap();
    let rows = client.get_movies()?.unwrap();

    println!("{rows:#?}");
    Ok(())
}
```

## See also

- [Runner model](./runner-model.md)
- [CLI reference](./cli.md)
- [Type representations](../internals/representations.md)
- [Rust guide](../../usage/rust.md)
- [Python guide](../../usage/python.md)
