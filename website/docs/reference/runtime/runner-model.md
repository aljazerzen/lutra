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
[Lutra binary format](../internals/binary-format.md).

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
with the expected schema. `Run::execute` still returns one complete encoded
result. Transport framing does not turn this API into a stream of result batches.

## What the binary format boundary means

The runner boundary is intentionally type-erased at runtime.
Programs, inputs, and outputs cross the boundary in a compact binary encoding,
not as rich in-memory host-language values.

This is what lets different host languages and runners share one execution
model.

## Binary transport sessions

The binary runner transport uses `lutra-bin-stream` framing. Each connection
carries one LTDS stream in each direction: request values from the client and
response values from the server. Each frame contains one complete encoded
request or response, including its request ID. Streams use `Compression::None`
by default and a zero type digest.

The server emits the response header lazily, with its first response or EOS,
not as an initial handshake. Clients must send a request or finish their request
stream before waiting for a response. This avoids an initial server write
blocking progress on duplex transports with very small buffers.

Shutdown uses explicit EOS in both directions. The client finishes and flushes
the request stream; the server finishes the response stream after processing
the requests. EOF without EOS is a truncated stream, not a successful shutdown.
A connection cannot carry concatenated LTDS sessions.

### Async clients

The Tokio client serializes complete request/response exchanges under one
session lock, not only writes. Concurrent callers therefore cannot interleave
requests and consume each other's responses. Use `shutdown_stream` to finish
the request stream, receive the response EOS, and close the transport.

Canceling an in-progress exchange or shutdown poisons the session: a later call
cannot safely resume it as another request. Discard the session and establish a
new connection. Cancellation while waiting for the session lock does not start
an exchange.

The embedded client uses the same framing through `embedded_io_async`. Its
errors distinguish framing and transport failures, value-decoding failures,
runner protocol errors, and session-state errors. It tracks pending request IDs
and caches responses for other pending requests while waiting for a particular
response. For example, waiting for a release response does not discard an
execution result that arrives first.

Receive all pending execution responses before calling the embedded client's
`shutdown`. It sends request EOS and waits for response EOS. An interrupted
in-progress operation poisons this client too; do not reuse that session.

### Compatibility and scope

This framed transport is wire-incompatible with the old unframed binary runner
protocol. Both peers must use the framed protocol; there is no automatic fallback.

LTDS envelope compatibility and runner request/response schema compatibility
are separate concerns. Version `0` and the zero digest do **not** establish that
peers share the same runner schema. Runner schema compatibility and negotiation
remain unresolved; use matching schemas on both ends.

Although value streams can carry array batches in separate frames, the runner
protocol still carries one whole execution result in a response. It does not
provide batch streaming for `Run::execute`.

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
- [Binary format](../internals/binary-format.md)
- [Format specification](./format-spec)
- [Representations](./representations)
