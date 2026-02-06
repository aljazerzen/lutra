# lutra-runner-duckdb

DuckDB runner implementation for Lutra - enables in-process analytical queries.

## Overview

`lutra-runner-duckdb` implements the `lutra_runner::Run` trait, providing an
embedded analytical database using [DuckDB](https://duckdb.org/). This allows
you to execute Lutra programs in-process with DuckDB's fast analytical query
engine.

**Key Features:**
- In-memory or file-based database
- Async runtime integration via `async-duckdb`
- Full support for primitives, tuples, arrays, and enums (including nested types).
- No external database server required

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
lutra-runner-duckdb = "0.3"
```

### DuckDB Version and Bundling

This crate uses the `duckdb` crate, which requires DuckDB library to be provided
in one of the following ways:

- **Dev**: Uses `nixpkgs.duckdb` from the nix environment and
  link dynamically.
- **Prod - dynamic linking**: Requires DuckDB shared library
  (`.so`/`.dylib`/`.dll`) available in the build environment.
- **Prod - static linking**: The `duckdb` crate supports a `bundled` feature
  that bundled DuckDB into this library, avoiding the need for a system library.
  To use this, add the feature to your dependencies:

  ```toml
  [dependencies]
  async-duckdb = { version = "0.3", features = ["bundled"] }
  ```

## Usage

### Rust API

```rust
use lutra_runner::Run;
use lutra_runner_duckdb::Runner;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create in-memory database
    let runner = Runner::in_memory().await?;
    
    // Or use file-based storage
    let runner = Runner::open("data/analysis.duckdb").await?;
    
    // Prepare and execute a program
    let prepared = runner.prepare(program).await?;
    let output = runner.execute(&prepared, &input).await?;
    
    Ok(())
}
```

### CLI

Lutra CLI provides this runner via `--duckdb` argument.

```bash
# In-memory database
lutra run --duckdb=:memory:

# File-based database
lutra run --duckdb=data.duckdb
```

## Work in progress

Following features are currently being worked on.

### 1. Arrays/Enums from External Tables

When constructing arrays in Lutra code, they work perfectly (see examples below).
However, reading arrays/enums from **existing DuckDB table columns** via
`std::sql::from()` is not yet fully supported:

```rust
// ✅ Works - constructing arrays in Lutra
func main() -> [1, 2, 3]: [int64]

// ✅ Works - arrays in tuples (serialized as DuckDB LIST via subquery)
func main() -> {nums: [1, 2, 3]: [int64], count: 3}

// ⚠️ Limited - reading LIST columns from existing tables
func process_data() -> std::sql::from("table_with_list_column")
// This may not deserialize LIST columns correctly
```

### 2. Schema Introspection

The `get_interface()` method currently returns an empty string. Schema
introspection from existing DuckDB databases is not yet implemented.

### 3. Input Parameter Limitations

Arrays of tuples as input parameters don't work because duckdb-rs does not yet
support STRUCT and LIST values in their bind_parameter function.


## Contributing

See the main Lutra repository for contribution guidelines.

## License

This project is part of the Lutra language project. See the main repository for license information.
