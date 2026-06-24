# Contributing

## Architecture

The main flow is:

```text
.lt source -> lutra-compiler -> rr::Program -> runner -> binary output
```

The three central crates are:

- `lutra-compiler`: parses, resolves, type-checks, lowers, and compiles source.
- `lutra-bin`: defines the runtime binary format and shared IR data structures.
- `lutra-runner`: defines `Run` and `RunSync`; concrete runners execute programs.

Design constraints:

- Keep runtime boundaries in `lutra-bin` binary format.
- Avoid runtime type information when you can.
- Prefer compiler-side synthesis over adding new runtime behavior.

## Compiler pipeline

The compiler stages are:

1. Lexer: `lutra-compiler/src/parser/lexer/`
2. Parser: `lutra-compiler/src/parser/`
3. Resolver: `lutra-compiler/src/resolver/`
4. Lowerer: `lutra-compiler/src/intermediate/lowerer.rs`
5. Inliner: `lutra-compiler/src/intermediate/inliner.rs`
6. Backend:
   - Bytecode: `lutra-compiler/src/bytecoding.rs`
   - SQL: `lutra-compiler/src/sql/`
     - clauses (compiles to CR clause reprensentation)
     - queries (compiles to SQL AST)

Key representations:

- `PR`: parser representation used during type checking.
- `IR`: lowered intermediate representation.
- `BR`: bytecode representation.
- `RR`: runner representation used by backends.

When you implement a feature, decide which stage should own it. Do not change a later stage if an earlier stage can express the feature more cleanly.

## Development workflow

Use `just` for the normal workflow.

For iterative work:

```bash
just dev
just dev 'test(name)'
just dev 'test(name)' --no-capture
```

`just dev` runs tests, snapshot review, formatting, `cargo check`, and `clippy`.
It also sets `INSTA_FORCE_PASS=1`, so review snapshot changes carefully.

Before you finish, run:

```bash
just test
```

If PostgreSQL tests fail because the database is unavailable, stop and ask the user to run:

```bash
just pg-up
```

## Testing expectations

When behavior changes, update tests close to the affected layer:

- `tests/tests/corpus/*.lt`: language and backend behavior.
- `tests/src/lowerer.rs`: lowered IR snapshots.
- `tests/src/resolver.rs`: type-checking and inference.
- Crate-local unit tests where appropriate.

Keep backend behavior aligned across the interpreter, DuckDB, and PostgreSQL when the feature is cross-cutting.

Do not accept snapshot changes blindly. Read them.

## Generated code

Some files are generated. Regenerate them only when needed, usually after changes to `lutra-bin` project definitions:

```bash
just generate
just generate-no-compile # use when the project does not compile
```

Do not hand-edit generated files unless you are debugging the generator itself.

## Debugging commands

```bash
# Check an inline expression
cargo run -- --verbose check --program='arbitrary lutra expression'

# Compiler output for a file
cargo run -- --verbose check --project=/tmp/source_file.lt

# Print compiled IR for a program
cargo run -- --verbose compile --project=/tmp/source_file.lt --program='my_func() + 3'

# Run a program on duckdb
cargo run -- --verbose run --program 'hello_world()' --runner duckdb
```


## Change guidelines

When you implement a feature:

- Prefer the smallest layer that can express the change.
- Reuse existing IR nodes and compiler patterns before adding new primitives.
- Update tests and docs together when user-visible behavior changes.
- Keep changes local and readable.
- If a workaround seems necessary, verify the assumption first with logs or a focused test.

## Project management

The project uses Codeberg Forgejo for issues and pull requests. If you need issue metadata from the command line, use `fj`.
