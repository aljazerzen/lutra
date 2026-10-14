---
title: Binary format
---

Lutra binary format is a method of serializing structured data.
It is used at the runner boundary to encode program inputs and outputs.

Schema of the data structures is defined with the Lutra language.
This schema is used to generate code in the desired language.

<figure markdown="1">
  ![Data flow diagram](./data-format-flow.drawio.svg)
  <figcaption markdown="span">
  Data flow from runners (databases) to target languages.<br/>
  Binary format is the common interface between them.
  </figcaption>
</figure>

The format supports partial decoding, that is, reading only part of the encoded data structure.

Schema evolution is not supported. To deserialize a structure safely, you must use the same schema that was used to serialize it. Other schemas might appear to work, but the format provides no guarantees.

Currently supported languages:

- Rust
- Python

## Value streams

The Rust crate `lutra-bin-stream` provides readers and writers for streams of
encoded values of one type. Its transport-independent `sans_io` core handles
incremental framing and compression; blocking and async adapters handle I/O.
It adds framing around the existing binary format without changing the value
layout:

```text
"LTDS" | version (1 byte, 0) | header (9 bytes) | frames ... | EOS
```

The magic bytes are `4c 54 44 53`. The header uses the Lutra binary encoding of:

```lt
type StreamHeader: {
  type_digest: Prim64,
  compression: enum {None, Zstd},
}
```

The digest is currently always zero, and readers reject nonzero digests. Type
hashing is separate future work, so this placeholder does **not** detect schema
mismatches. Callers must agree on the value type externally. The compression tag
is one byte: `0` for `None`, `1` for `Zstd`. All multibyte envelope fields are
little-endian. The header is not compressed.

Each frame starts with a four-byte word:

| Word | Meaning |
| --- | --- |
| `0x00000000`–`0x7fffffff` | Payload length in bytes, followed by the payload. |
| `0x80000000` | End of stream (EOS), with no payload. |
| `0x80000001`–`0xffffffff` | Reserved control codes; currently rejected. |

Each data payload represents one complete value. For `T = [Row]`, each frame can
hold a batch of rows without declaring the total row count. An empty array is an
ordinary value, not EOS. Zero-byte values, such as empty tuples, are valid too.

With `Zstd`, each payload contains exactly one independent Zstandard frame,
without external dictionaries or shared history. The length counts compressed
bytes. A zero-byte value therefore has a nonempty compressed payload. Concatenated
Zstandard frames, skippable frames, and trailing payload bytes are rejected.

Both wire and decompressed payloads are limited to `0x7fffffff` bytes (2 GiB − 1).
The decompression limit is enforced incrementally. `Reader::with_limits` accepts
smaller operational limits; Zstandard's default decoder window limit also
applies. These limits do not bound total stream size or inner-value decoding work.

`Writer::write_frame` takes an already-encoded value. Use `Writer::flush` to
flush buffered frames without ending the stream. Call `Writer::finish` to write
EOS and flush the transport; dropping a writer does not finish the stream.
`Reader::read_frame` returns `Some(bytes)` for each decompressed value and `None`
only for explicit EOS. Premature EOF is an error, including EOF between frames.
After an error, the reader or writer cannot be reused.

Concatenated streams are unsupported. A reader stops at EOS without consuming
subsequent bytes, whose meaning is undefined by this protocol. The caller may
close the connection. Framing does not validate inner values or their offsets;
that remains the value decoder's responsibility.

### Rust APIs and features

The existing blocking API remains available: `Reader` and `Writer` at the crate
root re-export `sync::Reader` and `sync::Writer`. The crate provides these feature
flags:

| Feature | Effect |
| --- | --- |
| `std` | Enables the blocking adapters and standard-library integration. Enabled by default. |
| `compression-zstd` | Enables Zstandard compression and requires `std`. Enabled by default. |
| `async` | Enables `r#async::Reader` and `r#async::Writer` over `embedded_io_async` traits. |
| `tokio` | Enables `tokio::TokioIo` to bridge Tokio transports to the async adapters; includes `async` and `std`. |

With default features disabled, the `sans_io` core supports `no_std` with
allocation. The `async` feature also works in this configuration; it does not
require Tokio or `std`. Without `compression-zstd`, selecting or receiving
`Compression::Zstd` returns an unsupported-compression error.

For an allocation-enabled embedded target, use:

```toml
[dependencies]
lutra-bin-stream = { version = "0.6", default-features = false, features = ["async"] }
```

Use the Lutra version that matches your project.

Async readers and writers retain framing state separately from the transport.
Pass a mutable transport to each operation. For Tokio, wrap that borrow in
`lutra_bin_stream::tokio::TokioIo(&mut connection)`; the bridge adds no buffering
or framing state. Async `write_frame`, `flush`, `finish`, and `read_frame` follow
the same frame and EOS semantics as the blocking API.

For custom transport integration, feed incoming bytes into `sans_io::Reader`
and drain pending bytes from `sans_io::Writer`. The core performs no transport
I/O, and input beyond a reported event remains owned by the caller.

## Example

```lt
type Movie: {
  id: Prim16,
  title: Text,
}

let value: Movie = {
  id = 5,
  title = "Gladiator",
}
```

Given the type definition above, the `value` constant would be encoded as:

```
| id     | title                                                  |
| 05 00  | 08 00 00 00 | 09 00 00 00 | 47 6c 61 64 69 61 74 6f 72 |
| Prim16 | offset      | length      | contents                   |
```
