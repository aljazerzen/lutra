---
title: Runners
---

The same Lutra program can run on different backends. A runner is the execution
target: it takes a compiled program, runs it, and returns the result.

Lutra currently supports three runners:

- **DuckDB**, for embedded SQL analytics,
- **PostgreSQL**, for SQL execution against a PostgreSQL database,
- the **interpreter**, for local in-process execution.

This page explains when to use each runner, what capabilities they offer, and
how they differ in practice.

## Choose a runner

Each runner fits a different situation:

- Use the **interpreter** when you want to run Lutra locally without any
  database. It supports filesystem operations such as reading and writing
  Parquet files. It is the simplest way to get started.

- Use **DuckDB** when you want SQL-backed analytics on local or embedded data.
  DuckDB supports both SQL table operations and filesystem operations. It
  handles large datasets efficiently and works well for analytical workloads.

- Use **PostgreSQL** when your data lives in a PostgreSQL database. PostgreSQL
  supports SQL table operations but not filesystem operations.

As a rule of thumb: start with the interpreter for learning and local work.
Switch to a SQL runner when your program needs to read from or write to
database tables.

## Run the same program on different runners

Many Lutra programs work on any runner without changes. For example, this
program uses only core language features:

```lt
type Movie: {id: Int32, title: Text, year: Int32}

const movies: [Movie] = [
  {1, "Arrival", 2016},
  {2, "Dune", 2021},
  {3, "Barbie", 2023},
]

func recent_movies() -> (
  movies
  | filter(m -> m.year >= 2020)
  | sort(m -> m.title)
)
```

You can run it on any runner:

```console
$ lutra run --project example.lt --runner interpreter
$ lutra run --project example.lt --runner duckdb
```

All three runners produce the same result. The difference is where the
computation happens.

## Use runner-specific features

Some standard library modules are only available on certain runners. When you
use one of these modules, your program becomes tied to the runners that
support it.

### Read and write files with the interpreter or DuckDB

The `std::fs` module provides Parquet file operations. It is available on the
interpreter and DuckDB, but not on PostgreSQL.

```lt
type Row: {id: Int32, name: Text}

func read_data(): [Row] -> fs::read_parquet("data.parquet")

func write_data(rows: [Row]) -> fs::write_parquet(rows, "output.parquet")
```

```console
$ lutra run --project example.lt --runner interpreter --program 'read_data()'
$ lutra run --project example.lt --runner duckdb --program 'read_data()'
```

### Query database tables with DuckDB or PostgreSQL

The `std::sql` module provides table operations. It is available on DuckDB and
PostgreSQL, but not on the interpreter.

```lt
type User: {id: Int64, name: Text, email: Text}

func get_users(): [User] -> sql::from("users")

func add_users(users: [User]) -> sql::insert(users, "users")
```

```console
$ lutra run --project example.lt --runner duckdb:app.duckdb --program 'get_users()'
$ lutra run --project example.lt --runner 'postgres://localhost/mydb' --program 'get_users()'
```

DuckDB also supports `std::fs`, so you can combine file and table operations
in one program:

```lt
func import_from_parquet() -> (
  fs::read_parquet("users.parquet")
  | sql::insert("users")
)
```

This program reads rows from a Parquet file and inserts them into a DuckDB
table. It works only on DuckDB because it needs both `std::fs` and `std::sql`.

## Capability matrix

This table summarizes which features are available on each runner:

| Feature | Interpreter | DuckDB | PostgreSQL |
|---|---|---|---|
| Core language (types, functions, pipelines) | ✓ | ✓ | ✓ |
| `fs` | ✓ | ✓ | — |
| `sql` | — | ✓ | ✓ |
| Nested types (tuples, arrays in columns) | ✓ | native (`STRUCT`, `LIST`) | serialized as `JSONB` |
| Enum storage | ✓ | `UNION` type | text tag column |
| Rust integration | ✓ | ✓ | ✓ |
| Python integration | ✓ | — | ✓ |

## Understand how types map to storage

When you use `sql::from` or `sql::insert`, Lutra maps your types to the
database's native column types. The two SQL runners handle this differently.

### DuckDB uses native nested types

DuckDB represents nested Lutra types with its own structured column types:

| Lutra type | DuckDB column |
|---|---|
| `{x: Int32, y: Int16}` | `x: int4, y: int2` |
| `{x: Int32, y: {a: Int16, b: Int16}}` | `x: int4, y: STRUCT(a int2, b int2)` |
| `{id: Int32, tags: [Text]}` | `id: int4, tags: text[]` |
| `enum {none, some: Int32}` | nullable `int4` |
| `enum {a: Int32, b: Text}` | `UNION(a int4, b text)` |

Nested tuples become `STRUCT`, nested arrays become `LIST`, and enums with
payloads become `UNION`. This preserves the structure without serialization.

### PostgreSQL flattens and serializes

PostgreSQL uses a different strategy. Tuples are flattened into named columns,
and nested arrays are serialized as `JSONB`:

| Lutra type | PostgreSQL columns |
|---|---|
| `{x: Int32, y: Int16}` | `x::int4, y::int2` |
| `{x: Int32, y: {a: Int16, b: Int16}}` | `x::int4, y.a::int2, y.b::int2` |
| `{id: Int32, tags: [Text]}` | `id::int4, tags::jsonb` |
| `enum {none, some: Int32}` | nullable `int4` |
| `enum {a: Int32, b: Text}` | `value::text, a::int4, b::text` |

Nested tuples are flattened into dot-separated column names. Nested arrays are
stored as `JSONB`. Enums with multiple payload variants use a text tag column.

### What this means in practice

You do not need to think about these mappings when writing Lutra code. The
compiler handles the translation automatically. But you will encounter these
column layouts when:

- creating tables manually with SQL,
- inspecting database contents with external tools,
- writing `sql::raw` queries that reference column names directly.

## Set a default runner for a project

You can set a default runner in your project file so you do not have to pass
`--runner` every time:

```lt
@!runner("duckdb:data.duckdb")

type Movie: {id: Int32, title: Text}

func get_movies(): [Movie] -> sql::from("movies")
```

```console
$ lutra run --project project.lt --program 'get_movies()'
```

The `@!runner` annotation sets the default. You can still override it with an
explicit `--runner` flag.

## Write portable programs

When you want a program to work across runners, avoid runner-specific modules
in its core logic. A common pattern is to separate data access from
transformation:

```lt
type Sale: {product: Text, amount: Int64}

// Data access — runner-specific
func load_sales(): [Sale] -> sql::from("sales")

// Transformation — works on any runner
func top_products(sales: [Sale], n: Int64) -> (
  sales
  | group_map(
      s -> s.product,
      func (product, rows) -> {
        product,
        total = rows | map(r -> r.amount) | sum(),
      }
    )
  | sort(x -> -x.total)
  | slice(0, n)
)

// Entry point — ties them together
func main() -> load_sales() | top_products(5)
```

The `top_products` function works on any runner because it uses only core
language features. You could test it with the interpreter using in-memory data,
then run the full pipeline against DuckDB or PostgreSQL.

## See also

- [Runner model](../reference/runtime/runner-model.md) for the technical
  interface between callers and runners.
- [DuckDB reference](../reference/runtime/duckdb.md) for DuckDB-specific
  details and type mappings.
- [CLI reference](../reference/runtime/cli.md) for all runner selection options.
- [Type representations](../reference/internals/representations.md) for the full
  mapping rules across all representations.
- [`std::sql` reference](../reference/language/std/sql/index.md) for SQL
  interface functions.
- [`std::fs` reference](../reference/language/std/fs/index.md) for filesystem
  functions.
