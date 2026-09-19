# C++

Lutra generates self-contained C++20 headers with native types and binary
encoders/decoders. No C++ runtime installation or linking is required; the small
header-only codec is embedded in the output. Use a C++20 compiler and standard
library, such as GCC or Clang.

## Generate and use bindings

Given `types.lt`:

```lt
type Movie: {id: Int64, title: Text}
```

Generate a header (both `.hpp` and `.hxx` select C++):

```sh
lutra gen-code --project ./types.lt ./generated.hpp
```

Use it from `main.cpp`:

```cpp
#include "generated.hpp"
#include <cassert>

int main() {
  lutra_generated::Movie movie{54, "Hello"};
  auto bytes = lutra_cpp::encode(movie);
  auto decoded = lutra_cpp::decode<lutra_generated::Movie>(bytes);
  assert(decoded.title == "Hello");
}
```

```sh
c++ -std=c++20 main.cpp -o example
./example
```

## Type mapping

All source modules live under `lutra_generated`, with nested C++ namespaces.

| Lutra | C++ |
| --- | --- |
| `Bool` | `bool` |
| Signed/unsigned integers | `std::intN_t` / `std::uintN_t` |
| `Float32` / `Float64` | `float` / `double` |
| `Text` | `std::string` (validated UTF-8) |
| `[T]` | `std::vector<T>` |
| Tuple/record | Aggregate struct |
| Enum | Struct containing a `std::variant` named `value` |
| Type alias | `using` alias |

Unnamed tuple fields are named `field0`, `field1`, etc. Inline structured types
receive generated names; prefer aggregate initialization rather than relying on
those names. Temporal and decimal types retain their Lutra structural representation,
not `std::chrono` or a third-party decimal type.

Enums preserve named alternatives, even when multiple alternatives have the same
payload type:

```lt
type Genre: enum {drama, other: Text}
```

```cpp
namespace g = lutra_generated;
g::Genre genre{g::Genre::other{"documentary"}};
auto& text = std::get<g::Genre::other>(genre.value).value;
```

Each alternative wraps its payload in `value`; payload-free alternatives use
`std::monostate`. C++ keywords, reserved identifiers, internal helper names, and
names starting with `lutra_` are escaped as `lutra_id_` followed by hexadecimal
UTF-8 bytes. For example, `class` becomes `lutra_id_636c617373`. A member matching
its enclosing type uses a `lutra_member_` prefix instead. The Lutra `std` module
is also escaped to avoid shadowing the C++ standard namespace.

## Binary compatibility and errors

`lutra_cpp::encode(value)` returns `std::vector<std::uint8_t>`;
`lutra_cpp::decode<T>(bytes)` accepts `std::span<const std::uint8_t>`.
The wire format is compatible with `lutra-bin`, including little-endian scalars,
relative pointers, and enum layouts.

Malformed input (including truncated heads/bodies, invalid UTF-8, and invalid enum
tags) throws `lutra_cpp::error`. Allocation failures use standard C++ exceptions.
As with Rust decoding, trailing bytes are permitted. Applications processing
untrusted data should impose resource limits: bounds checks alone do not bound
the size of decoded values (particularly arrays of zero-sized elements).

## Generation options and embedded programs

- `--no-encode-decode`: omit generated structured-type codec specializations.
- `--no-types`: omit native type declarations and their codecs; configured program
  bytes can still be emitted. The embedded runtime remains available.
- Rust function traits, runner clients, `no_std`, and custom Rust dependency paths
  are unsupported and produce errors. Recursive types, empty enums, and function-valued data types are not
  supported in this initial backend.

The Rust API can also embed compiled programs:

```rust
lutra_codegen::generate(
    &project,
    lutra_codegen::Target::Cpp,
    "generated.hpp",
    lutra_codegen::GenerateOptions::default()
        .generate_programs("api", lutra_codegen::ProgramRepr::BytecodeLt, []),
)?;
```

For a function `api::query`, the header provides
`lutra_generated::api::query::bytes`, `::input_type`, and `::output_type`.
`bytes` contains a serialized `lutra_bin::rr::Program`, **not raw VM bytecode**.
There is no C++ execution engine or runner client yet; submit programs through
an appropriate external runner integration.

The [`examples/cpp`](https://codeberg.org/lutra/lutra/src/branch/main/examples/cpp)
directory contains a complete example. To run the backend tests, use
`cargo test -p lutra-codegen`; set `CXX` to the compiler executable if it is not
available as `c++`.
