---
title: Command line
---

This page lists the main Lutra CLI commands and when to use them.

For an installation and first-use guide, see the [Command line guide](../usage/cli.md).

## Top-level commands

```console
$ lutra --help
Usage: lutra [OPTIONS] <COMMAND>

Commands:
  discover         Read the project
  check            Validate the project
  compile          Compile a program
  run              Compile a program and run it
  interactive      Interactive project environment with live recompilation
  pull             Pull interface from the runner
  codegen          Compile the project and generate bindings code
  format           Format source files
  language-server  Start language server (LSP)
  help             Print this message or the help of the given subcommand(s)
```

## `run`

Compile a program and execute it.

Common options:

- `--interpreter`
- `--postgres <DSN>`
- `--program <Lutra expression>`
- `--project <file.lt>`
- `--input <file.lb>`
- `--output <file.lb>`

Example:

```console
$ lutra run --project ./project.lt --interpreter --program main
```

## `interactive`

Start an interactive project environment with live recompilation.

```console
$ lutra interactive --help
Interactive project environment with live recompilation

Usage: lutra interactive [OPTIONS]

Options:
      --project <PROJECT>  Path to a project file
  -i, --interpreter        Use interpreter runner
      --postgres <DSN>     Use PostgreSQL runner. Requires a postgres:// URL or a libpq-style connection config
      --duckdb <PATH>      Use DuckDB runner. Provide path to database file or ":memory:" for in-memory database
  -h, --help               Print help
```

Use `interactive` when you want quick feedback while exploring a project.

## `check`

Validate a project by parsing files and resolving names and types.

Common options:

- `--program <Lutra expression>`
- `--project <file.lt>`

Example:

```console
$ lutra check --project ./project.lt
```

## `codegen`

Generate bindings for another language.

Arguments:

- `--project <file.lt>`
- output path ending in `.rs` or `.py`

Examples:

```console
$ lutra codegen --project ./project.lt ./lutra.rs
$ lutra codegen --project ./project.lt ./generated.py
```

## Other commands

- `discover` — read the project
- `compile` — compile a program without running it
- `pull` — pull interface information from the runner
- `format` — format source files
- `language-server` — start the language server

## See also

- [Command line guide](../usage/cli.md) for installation and first-use workflow.
- [Learn](../learn/introduction.md) for the language guides.
- [Runner model](./project/runner-model.md) for backend execution details.
