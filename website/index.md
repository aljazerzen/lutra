---
permalink: /
layout: default.liquid
title: Lutra
---

<div style="margin-top: 2em">
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Binary format</h2>
</div>


It defines a minimal algebraic type system and binary data format for each of the types.
It provides code generation tools for creating bindings in other programming languages
(currently only Rust is supported).

```
type album = {id = int64, title = text}

type invoice = {id = int64, customer = text, total = float64}
```

<div style="height: 1em"></div>

<div style="margin-top: 2em">
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">RPC / API framework</h2>
</div>


The Lutra language specify an API in term of function declarations, which
can be implemented in your language of choice.
These functions can then be invoked by Lutra _programs_.

```
let get_albums: func (): [album]

let get_invoices: func (): [invoice]
```

<div style="margin-top: 2em">
<span class="muted">Lutra is a ...</span><br>
<h2 style="margin-top: 0">Query language</h2>
</div>

Lutra programs are a high-level, statically typed language,
which be compiled to lutra-runtime bytecode or
to SQL to be executed by relational databases
(currently only PostgreSQL is supported).

```
type album = {id = int64, title = text}

let get_albums: func (): [album]

let get_album_by_id = func (album_id: int64): album -> (
  get_albums()
  | std::filter(func (this: album) -> this.id == album_id)
  | std::index(0)
)
```
