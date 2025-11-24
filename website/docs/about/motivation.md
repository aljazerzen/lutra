---
title: 'Motivation'
---


### Query languages suck

Most query languages are not designed as a programming language, but have started as a few very basic queries and then evolved by patching-in more and more features on top. This makes them hard to use for writing complex programs.
SQL is the biggest offender (see
[1](https://www.geldata.com/blog/we-can-do-better-than-sql),
[2](https://carlineng.com/?postid=sql-critique#blog),
[3](https://www.scattered-thoughts.net/writing/against-sql/),
[4](https://aljaz.murerzen.eu/posts/sql-group-by-primary-key/),
[5](https://aljaz.murerzen.eu/posts/sql-param-types/),
[6](https://aljaz.murerzen.eu/posts/sql-edge-case-values/),
[7](https://aljaz.murerzen.eu/posts/sql-column-names/),
[8](https://ambitonline.com/nextrelease/2025/04/sql-the-worst-api-ever/),
[9](https://dev.to/shaqq/10-things-i-hate-about-sql-a9e)
), but many other languages are similarly problematic.

Lutra aims to be a "proper" programming language, with user-defined functions, an algebraic type system, generic function type parameters, Hindley-Milner-like type inference and many small, "quality of life" features.

That all compiles to a [minimal intermediate representation](https://codeberg.org/lutra/lutra/src/branch/main/lutra-bin/src/project/ir.lt) that *should* be easy to compile for different execution targets.
The goal is to support SQL and most major relational databases, but also other *runners* like WebAssembly or [Polars](https://www.pola.rs/).

Because a different compilation model, Lutra compiler can emit better error messages, provide better guarantees about
compiled code and evolve the language without the need to change any database query language.

Essentially, it is treating SQL as an instruction set instead of a user-facing API.

### Type safety

Many challenges in modern programming arise from losing type information within a system. Often this is the fault of the programming language, but in many cases it is caused by passing data through type-erasing points.

For example, API calls that return JSON, reading from files, or calling out to a database all lose type information.
While these interfaces usually have a notion of *expected data format*, most tools fail to express this information in a way that can be used by the toolchain and the programmer.

The Lutra Project aims to provide this tooling and establish a standardized interface for interaction between software operating across different machines, processes, or programming languages.
