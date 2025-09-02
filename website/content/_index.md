---
title: Lutra
---


Lutra is a language for preserving type information between different software components.
It is a high-level, statically typed language, designed for querying data
and expressing data structures.

```lt
type Album: {id: int16, title: text}

func get_albums(): [Album] -> std::sql::from("albums")

func get_album_by_id(album_id: int16): Album -> (
  get_albums()
  | find(func (this) -> this.id == album_id)
)
```

It is minimal and designed to be extended to new execution targets.
Currently, it can run on a reference-implementation interpreter and PostgreSQL.

[More code examples](https://codeberg.org/lutra/lutra/src/branch/main/tests/tests/corpus/language.lt).


## Why


### Because query languages suck

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

### Because type safety

Many challenges in modern programming arise from losing type information within a system. Often this is the fault of the programming language, but in many cases it is caused by passing data through type-erasing points.

For example, API calls that return JSON, reading from files, or calling out to a database all lose type information.
While these interfaces usually have a notion of *expected data format*, most tools fail to express this information in a way that can be used by the toolchain and the programmer.

The Lutra Project aims to provide this tooling and establish a standardized interface for interaction between software operating across different machines, processes, or programming languages.



## Core principles


### Data should always carry type information

The business logic of programs should be fully statically typed.
Each variable should have an associated type, either annotated explicitly
or preferably inferred from context.

```rs
# Without type information
let result = request("GET", "http://my-app.com/v1/movies/" + str(movie_id))
let movie_title = result["title"]

# With type information
let result: Movie = my_app::movies::get(movie_id)
let movie_title = result.title
```

**Rationale**: types improve readability of the program and validate the author's assumptions
about written code. They first guide the author when writing code, and also automate validation
of these assumptions when program is changed in the future.

**Lutra** achieves this by defining types and code in its own language,
which serves as a common interface between different programming languages.


### Type information should be available to the whole toolchain

Development tools like compilers, editors, language servers and code explorers should all
have access to complete type information.

**Rationale**: they should be able to assist development, suggest improvements, organize code and
provide insights into the codebase.

**Lutra** projects can be translated to languages (currently Rust and Python), by generating code for
types and function interfaces.


### Type information should exist only at compile time

Many data formats carry type information into runtime. For example, JSON stores
field names alongside data: `{"id": 3, "title": "Gladiator"}`.
Such runtime type information is inefficient and unnecessary.

**Rationale**: in most situations program already makes assumptions about the value in a variable
(e.g. `let x: Movie`). Additionally, runtime reflection increases the complexity of the codebase and moves
operations that should have been done at compile time into runtime.

**Lutra** defines a binary format, which does not carry type information.
It focuses on simplicity and ease of use, but also provides partial decoding capabilities.



## Project status

Lutra is a personal passion project, currently in a proof-of-concept stage.
It is not ready for production use.
It works, but is not feature complete and will change in inconvenient ways.

Feel free to try it out or come and chat with me at [Zulip](https://lutra.zulipchat.com/).

