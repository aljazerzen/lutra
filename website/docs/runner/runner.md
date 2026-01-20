---
title: Runner
---

Lutra runner interface is an abstraction of the execution target of Lutra
programs. It defines functions to pass a program from the caller to the callee,
provide program inputs and return outputs.

<figure markdown="1" style="width: 100%">
  ![Sequence diagram of the runner interface](./runner.drawio.svg){width="100%"}
  <figcaption markdown="span">
  Sequence diagram of the runner interface.<br/>
  </figcaption>
</figure>


All data passed trough the interface is encoded in
[Lutra binary format](./format).

## Interface

Trimmed down for brevity, this the the interface (found in lutra-runner crate):

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
    fn get_interface(&self) -> Result<string::String, Self::Error>;
}

```
