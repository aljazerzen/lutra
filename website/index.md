---
permalink: /
layout: default.liquid
title: Lutra
---

<div style="height: 2em"></div>
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Binary format</h2>
</div>


It defines a minimal algebraic type system and binary data format for each of the types.
It provides code generation tools for creating bindings in other programming languages
(currently only Rust is supported).

```
type album: {id: int64, title: text}

type invoice: {id: int64, customer: text, total: float64}
```

<div style="margin-top: 2em">
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Language</h2>
</div>

Lutra programs are a high-level, statically typed language,
which be compiled to lutra-runtime bytecode or
to SQL to be executed by relational databases
(currently only PostgreSQL is supported).

```
type album: {id: int64, title: text}

let get_albums: func (): [album]

let get_album_by_id = func (album_id: int64): album -> (
  get_albums()
  | std::filter(func (this: album) -> this.id == album_id)
  | std::index(0)
)
```

<div style="height: 2em"></div>
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Database driver</h2>
</div>

Can execute Lutra programs on relational databases, while preserving typing information of query parameters and results.

```rust
// main.rs

let client = lutra_db_driver::RunnerSync(...);

let result = client.run(&lutra::my_program(), &4).unwrap();

// result is a Rust struct
println!("id={}, title={}", result.id, result.title);
```
