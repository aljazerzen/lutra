---
title: 'Design decisions'
---


## Core principles


### Data should always carry type information

The business logic of programs should be fully statically typed. Each variable
should have an associated type, either annotated explicitly or preferably
inferred from context.

```rs
// Without type information
let result = request("GET", format!("http://my-app.com/v1/movies/{movie_id}"))
let movie_title = result.get("title")  // this cannot be type checked

// With type information
let result: Movie = my_app::movies::get(movie_id)
let movie_title = result.title
```

**Rationale**: types improve readability of the program and validate the
author's assumptions about written code. They first guide the author when
writing code, and also automate validation of these assumptions when program is
changed in the future.

**Lutra** achieves this by defining types and code in its own language, which
serves as a common interface between different programming languages.


### Type information should be available to the whole toolchain

Development tools like compilers, editors, language servers and code explorers
should all have access to complete type information.

**Rationale**: they should be able to assist development, suggest improvements,
organize code and provide insights into the codebase.

**Lutra** projects can be translated to languages (currently Rust and Python),
by generating code for types and function interfaces.


### Type information should exist only at compile time

Many data formats carry type information into runtime. For example, JSON stores
field names alongside data: `{"id": 3, "title": "Gladiator"}`.
Such runtime type information is inefficient and unnecessary.

**Rationale**: in most situations program already makes assumptions about the
value in a variable (e.g. `let x: Movie`). Additionally, runtime reflection
increases the complexity of the codebase and moves operations that should have
been done at compile time into runtime.

**Lutra** defines a binary format, which does not carry type information.
It focuses on simplicity and ease of use, but also provides partial decoding
capabilities.


## Lutra is a query language

Lutra is a query language in the sense that it is designed to be executed on
platforms that provide their own data collections and have a limited access to
the environment.

For example, executing in a PostgreSQL database does give access to SQL tables,
but provides no access to the file system, network or any other POSIX interface.

So the language focuses on being as "pure" as possible: provide only functions
for dealing with data (arithmetics, text operations) and an access to data.

It is not possible, for example, to make HTTP requests from within your Lutra
program. There is no standard library that would give access to any networking
calls or even system calls. It is, however, possible to write a function
in a "native" language (e.g. Rust) and then call that function from Lutra.
Programs using such functions could not be executed on PostgreSQL, but only on
interpreters that have access to the native function.

In essence, Lutra provides access to native functions (which also includes SQL
tables) and the means to compose and manipulate the results of those functions.


## Where is `print` function?

When programming in C, one would use POSIX process arguments or read from stdin
to retrieve program inputs. To output something, one would write to stdout or
to a file. But Lutra does not have access to stdin/stdout or any POSIX interface
(at least not generally).

The only core interface for emitting messages is the program output.

This is so by design; a limited language interface makes it easier to implement
a new execution target. For example, PostgreSQL as an execution target of SQL
queries, does not provide any way to print/log from within SQL queries.


## Why the binary format?

Lutra programs executed on SQL databases consume and produce an *interesting*
relational data representation. The binary format is a way to unify these
representations and provide a common interface for dealing with databases.

The *interesting* representation is a consequence of Lutra's type system, which
provides an algebraic and **composable** type system. Composable means that any
"container" type (tuple, array or enum) can contain any other type. So we must
support all of the following:

- `[bool]`
- `{text, bool}`
- `{text, {text}, bool}`
- `{text, [text], bool}`
- `[{text, [{bool, bool, [text]}], bool}]`

To support all such types, it means to have a representation of these types
within SQL queries, in query results and query parameters. We try to use
efficient representations, but then fallback to encoding values in JSON.

Here are a few examples of query result representations:

- `[bool]`: many rows, one column of type `bool`,
- `{text, bool}`: one row, two columns of types `text` and `bool`,
- `{text, {text}, bool}`: one row, three columns (the inner tuple is flattened
  into the parent),
- `{text, [text], bool}`: one row, three columns of types `text`, `jsonb`, and
  `bool`,
- `[{text, [{bool, bool, [text]}], bool}]`: many rows, three columns of types
  `text`, `jsonb`, and `bool`.
  The json contains an array of arrays which have 3 elements: a boolean,
  a boolean, and an array of strings.

The representations of these types within queries or as query parameters have
subtle differences, because we need to work around what SQL databases support.

Using the database directly and working with these representations would be very
inconvenient and error prone. Instead, `lutra-runner-postgres` can transcode
between the Lutra binary format and these "relational" representations.

The result is abstraction of PostgreSQL as the *runner* interface. It consumes
inputs and produces outputs of arbitrary Lutra types, encoded in the Lutra
binary format.

