---
title: Command line
---

This reference summarizes the Lutra CLI commands, what they do, and the most
important options for each one.

For installation and a first walkthrough, see the [Command line guide](../../usage/cli.md).

## Top-level usage

```console
$ lutra --help
Usage: lutra [OPTIONS] <COMMAND>

Commands:
  interactive      Interactive REPL shell
  discover         Read a project
  check            Validate a project
  compile          Compile a program
  run              Compile a program and run it
  pull             Pull runner interface into a project
  format           Format source files
  gen-code         Generate bindings code from a project
  gen-docs         Generate Markdown reference from a project
  language-server  Start language server (LSP)
  serve            Serve a runner proxy over TCP binary protocol
  help             Print this message or the help of the given subcommand(s)

Options:
  -v, --verbose
  -h, --help
```

Use `-v` or `--verbose` for more logging.

## Runner

Runners can be selected with `--runner <URL>` argument. Supported URL schemes:

- `duckdb:PATH` — DuckDB runner
- `postgres://DSN` — PostgreSQL runner
- `interpreter` or `lt` — local interpreter
- `tcp://HOST:PORT` — remote runner over the TCP binary protocol

If the project root declares `@!runner("...")`, commands that operate on a
project use that runner by default. Any explicit runner flag overrides the
project default.

Examples:

```console
$ lutra run --project ./project.lt --runner lt
$ lutra run --project ./project.lt --runner duckdb
$ lutra interactive --project ./project.lt --runner 'postgres://user:pass@localhost:5432/db'
$ lutra pull --project ./project.lt
```

## `interactive`

Start the interactive REPL shell for a project.

Use `interactive` when you want quick feedback while exploring expressions,
recompiling against a project, or trying a real backend live.

See [Runner selection](#runner-selection).

Common options:

- `--project <file.lt>`
- runner selection flags

Example:

```console
$ lutra interactive --project ./project.lt --runner duckdb
```

## `discover`

Read a project from disk.

This command loads the project source tree and is mainly useful for inspecting
whether Lutra discovers the expected project root.

Common options:

- `--project <file.lt>`

Example:

```console
$ lutra discover --project ./project.lt
```

## `check`

Validate a project.

`check` parses project files, resolves names, and verifies types without
executing a program.

Common options:

- `--project <file.lt>`
- `--program <Lutra expression>` — also check one program expression
- `--print-project` — print debug information about the compiled project

Example:

```console
$ lutra check --project ./project.lt
```

## `compile`

Compile a program.

Use this when you want the compiled runner representation without executing it.

Common options:

- `--project <file.lt>`
- `--program <Lutra expression>`
- `--repr <sql-pg|sql-duckdb|bytecode-lt>`
- `--output <file.rr.ld>`

Example:

```console
$ lutra compile --project ./project.lt --program main --repr sql-pg
```

## `run`

Compile a program and run it.

This is the main command for executing Lutra programs from the CLI.

See [Runner selection](#runner-selection).

Common options:

- `--project <file.lt>`
- `--program <Lutra expression>`
- `--input <file>`
- `--input-format <lt|ld|ltd|csv|parquet|table>`
- `--output <file>`
- `--output-format <lt|ld|ltd|csv|parquet|table>`
- runner selection flags

Notes:

- Input format is inferred from the input file extension when omitted.
- Output format is inferred from the output file extension when omitted.
- When `--output` is omitted, output is written to stdout in Lutra source format.

Examples:

```console
$ lutra run --project ./project.lt --runner interpreter --program main
$ lutra run --project ./project.lt --input rows.csv --program transform
```

## `pull`

Pull runner interface into a project.

This command asks the selected runner for its exposed interface and updates the
project schema module when possible.

See [Runner selection](#runner-selection).

Common options:

- `--project <file.lt>`
- runner selection flags

Example:

```console
$ lutra pull --project ./project.lt --runner 'postgres://user:pass@localhost:5432/db'
```

## `format`

Format source files.

Use this to rewrite project files into Lutra's standard source formatting.

Common options:

- `--project <file.lt>`

Example:

```console
$ lutra format --project ./project.lt
```

## `gen-code`

Generate bindings code from a project.

This command compiles a project and emits Rust or Python bindings depending on
output file extension.

Arguments:

- `<OUTPUT_FILE>` — output path, typically ending in `.rs` or `.py`

Common options:

- `--project <file.lt>`
- `--no-types`
- `--no-encode-decode`
- `--no-function-traits`
- `--programs-bytecode-lt <PROGRAM>`
- `--programs-sql-pg <PROGRAM>`
- `--lutra-bin-path <PATH>`

Examples:

```console
$ lutra gen-code --project ./project.lt ./generated.rs
$ lutra gen-code --project ./project.lt --programs-sql-pg main ./generated.py
```

## `gen-docs`

Generate Markdown reference from a project.

This command renders project documentation into a directory of Markdown pages.

Arguments:

- `<OUTPUT_DIR>` — output directory for generated pages

Common options:

- `--project <file.lt>`

Example:

```console
$ lutra gen-docs --project ./project.lt ./docs
```

## `language-server`

Start the language server (LSP).

Use this command from an editor integration. The current transport is stdio.

Common options:

- `--stdio`

Example:

```console
$ lutra language-server --stdio
```

## `serve`

Serve a runner proxy over the TCP binary protocol.

This starts a TCP server that exposes a selected runner to remote clients.

See [Runner selection](#runner-selection).

Common options:

- `--at <HOST:PORT>` — bind address, default `127.0.0.1:5555`
- runner selection flags

Example:

```console
$ lutra serve --runner duckdb --at 127.0.0.1:5555
```

## See also

- [Command line guide](../../usage/cli.md) for installation and first-use workflow.
- [Runner model](./runner-model.md) for backend execution details.
- [Reference overview](../index.md) for the rest of the project reference.
