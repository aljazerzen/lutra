---
title: "Use from command line"
---

## Installation

Prerequisites:

- Rust toolchain

To install the CLI, run:

```sh
cargo install --index 'https://codeberg.org/lutra/_cargo-index.git' lutra-cli
```

!!! note

    While Lutra (the project) is in a pre-release state, none of the packages are yet published to crates.io. Instead, I use a custom registry at [codeberg.org](https://codeberg.org/lutra/_cargo-index) to host the packages. This is why we need to use the `--index` flag to tell `cargo` to use this registry.

## Command `run`

The most basic command is `run`, which executes a Lutra program.

Options:

- choice of a runner, which can be set to either:

    * `--interpreter`, or
    * `--postgres <connection-string>`

- `--program <Lutra expression>` which is the program to execute. Default to `main` (a reference to the `main` function).
- `--project <file.lt>` path to a project file.
- `--input <file.lb>` path to the input file in Lutra binary format.
- `--output <file.lb>`path to the output file in Lutra binary format.

### Examples

Execute a program on a local interpreter:

```console
$ lutra run \
    --interpreter \
    --program 'std::fold([1, 2, 3], 0: int32, func (s, v) -> s + v)'
6
```

---

Execute a program on PostgreSQL:

```console
$ lutra run \
    --postgres 'postgresql://postgres:password@localhost:5432/postgres' \
    --program 'std::fold([1, 2, 3], 0: int32, func (s, v) -> s + v)'
6
```

---

Use a project file:

```lt
# project.lt

func hello(name: text): text -> (
    f"Hello, {name}!"
)
```

```console
$ lutra run \
    --interpreter \
    --project ./project.lt \
    --program 'hello("world")'
"Hello, world!"
```

---

Write and read Lutra binary files:

```console
$ lutra run \
    --interpreter \
    --program '{a = false, name = "world"}' \
    --output ./data.lb
Output written to ./data.lb (14 bytes)

$ lutra run \
    --interpreter \
    --input ./data.lb \
    --program 'func(x: {a: bool, name: text}) -> f"Hello, {x.name}!"'
"Hello, world!"
```

## Command `check`

Validates a Lutra project. Parses all files and resolves all names and types.

Options:

- `--program <Lutra expression>` the program to check.
- `--project <file.lt>` path to a project file.

### Examples

Validate a project file:

```console
$ lutra check --project ./project.lt
```

## Command `codegen`

Generates bindings of a Lutra project for a target language (Rust or Python).

Arguments:

- `<project>` path to a project file.
- `<output file>` path to the output file.

The output file path must end with `.rs` or `.py` depending on the target language.

### Examples

Generate Rust bindings:

```console
$ lutra codegen --project ./project.lt ./lutra.rs
Used files:
- project.lt
Output written to lutra.rs
Done.
```

## Other commands

There other, less stable commands.

```console
$ lutra --help
Usage: lutra [OPTIONS] <COMMAND>

Commands:
  discover         Read the project
  check            Validate the project
  compile          Compile a program
  run              Compile a program and run it
  pull             Pull interface from the runner
  codegen          Compile the project and generate bindings code
  format           Format source files
  language-server  Start language server (LSP)
  help             Print this message or the help of the given subcommand(s)

Options:
  -v, --verbose
  -h, --help     Print help
```