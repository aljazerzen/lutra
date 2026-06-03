---
title: Runner model
---

A runner is the execution target for a Lutra program.
It receives a compiled program, accepts encoded input, and returns encoded output.

This page explains the conceptual boundary between the caller and the runner.
For end users, the main idea is that the same Lutra project can target different
runners, while data crosses that boundary in the binary format.

<figure markdown="1" style="width: 100%">
  ![Sequence diagram of the runner interface](./runner.drawio.svg){width="100%"}
  <figcaption markdown="span">
  Sequence diagram of the runner interface.<br/>
  </figcaption>
</figure>


All data passed through the interface is encoded in the
[Lutra binary format](./binary-format).

## What a runner does

Conceptually, a runner performs two steps:

1. **prepare** a compiled program for one concrete backend,
2. **execute** that prepared program with encoded input.

That means the caller does not pass ordinary Rust, Python, or SQL values across
this boundary. It passes a compiled program plus a binary payload.

## Interface

Trimmed down for brevity, this is the interface from the `lutra-runner` crate:

```rs
/// Ability to execute a Lutra program.
trait Run {
    /// Prepares a program for execution and returns a handle, which can be
    /// used with [Run::execute].
    fn prepare(
        &self,
        program: rr::Program,
    ) -> Result<Self::Prepared, Self::Error>;

    /// Execute a prepared program.
    /// Program's format must match the format supported by this runner.
    fn execute(
        &self,
        program: &Self::Prepared,
        input: &[u8],
    ) -> Result<vec::Vec<u8>, Self::Error>;

    /// Return static interface of this runner as Lutra source code.
    ///
    /// Runners can provide implementations for functions that are not part of
    /// standard library. This function returns definitions of these functions as
    /// Lutra source code.
    ///
    /// For example: interpreter can provide `fs::read_parquet(...)`
    /// and PostgreSQL runner can provide `sql::from(...)`.
    fn pull_schema(&self) -> Result<string::String, Self::Error>;
}

```

## What `prepare` and `execute` mean

`prepare` takes a compiled program and turns it into a backend-specific prepared
form.
For example, a SQL runner might turn the program into prepared SQL statements,
while the interpreter might keep a directly executable representation.

`execute` runs that prepared program with binary input and returns binary output.
The caller is responsible for encoding input values and decoding output values
with the expected schema.

## What the binary format boundary means

The runner boundary is intentionally type-erased at runtime.
Programs, inputs, and outputs cross the boundary in a compact binary encoding,
not as rich in-memory host-language values.

This is what lets different host languages and runners share one execution
model.

## Choosing a runner

Lutra currently has three main runner styles:

- the **interpreter**, for local in-process execution,
- **PostgreSQL**, for SQL-backed execution in a PostgreSQL database,
- **DuckDB**, for SQL-backed execution in DuckDB.

Choose the interpreter when you want local execution or filesystem-oriented
features. Choose a SQL runner when your program should run close to relational
data.

## See also

- [Runners guide](../../learn/runners.md) for a learning-oriented overview
  of backends and when to use each one.
- [Binary format](./binary-format)
- [Format specification](./format-spec)
- [Representations](./representations)
