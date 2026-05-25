---
title: Command line guide
---

Use the Lutra CLI when you want to:

- explore data interactively,
- check projects for errors,
- run programs locally and remotely.

For all commands and option, see the [CLI reference](../reference/runtime/cli.md).

## Install the CLI

### Using Nix

If you use [Nix](https://nixos.org/), install the CLI with:

```console
$ nix profile install 'git+https://codeberg.org/lutra/lutra-data.git'
```

### Using Cargo

Prerequisites:

- [Rust toolchain](https://rust-lang.org/learn/get-started/)
- [DuckDB](https://duckdb.org/) library installed on your system

Then install the CLI:

```console
$ cargo install lutra-cli
```

!!! note

    The CLI depends on DuckDB. On Debian or Ubuntu, you can install it with
    `apt install libduckdb-dev`. On macOS, you can use `brew install duckdb`.

    Optional: you can also use `--features bundled` to compile DuckDB from
    source. This requires a C++ compiler.

## Run your first program

Create a file named `example.lt`:

```lt title="example.lt"
func main() -> "Hello, world!"
```

Run it on DuckDB:

```console
$ lutra run --project example.lt --runner duckdb
const output = "Hello, world!"
```

This is the fastest way to try out small language examples from the learning
guides.

If you want omit the `--runner` argument, you can set the default runner in the
`.lt` file.

```lt title="example.lt"
@!runner("duckdb")
func main() -> "Hello, world!"
```

## Check a project

Use `check` to validate a project without running it.

```console
$ lutra check --project example.lt
All good.
```

This is useful when you want type checking and diagnostics while editing code.

## Run an expression directly

You can also run a one-off expression without creating a project function.

```console
$ lutra run \
    --runner duckdb \
    --program 'fold([1, 2, 3], 0: Int32, func (s, v) -> s + v)'
6
```

This is handy for quick experiments.

## Explore a project interactively

Use `interactive` when you want a live project environment with recompilation.

```console
$ lutra interactive --project example.lt
```

You can also use other runners:

```console
$ lutra interactive --project project.lt --runner postgres://user:pass@localhost:5432/db
$ lutra interactive --project project.lt --runner duckdb:data.duckdb
```

`interactive` is a good fit for exploration, rapid iteration, and trying
transformations against a real backend.

## See also

- [CLI reference](../reference/runtime/cli.md) for exact commands and options.
- [Runner model](../reference/runtime/runner-model.md) if you want to understand how programs are executed.
- [Learn](../learn/introduction.md) if you want to learn the language.
- [Tabular data basics](../learn/tabular-data.md) and [Aggregations](../learn/aggregations.md) for the main data-oriented guides.
- [Python guide](python.md) and [Rust guide](rust.md) for host-language workflows.
