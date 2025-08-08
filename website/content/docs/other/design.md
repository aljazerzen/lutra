---
title: 'Design decisions'
weight: 1
---


## Lutra is a query language

Lutra is a query language in the sense that it is designed to be executed on platforms that provide their own data collections and have a limited access to the environment.

For example, executing in a PostgreSQL database does give access to SQL tables, but provides no access to the file system, network or any other POSIX interface.

So the language focuses on being as "pure" as possible: provide only functions for dealing with data (arithmetics, text operations) and an access to data.

It is not possible, for example, to make HTTP requests from within your Lutra program.
There is no standard library that would give access to any networking calls or even system calls.
It is, however, possible to write a function in a "native" language (e.g. Rust) and then call that function from Lutra.
Programs using such functions could not be executed on PostgreSQL, but only on interpreters that have access to the native function.

In essence, Lutra provides access to native functions (which also includes SQL tables) and the means to compose and manipulate the results of those functions.


## Where is `print` function?

When programming in C, one would use POSIX process arguments or read from stdin to retrieve program inputs.
To output something, one would write to stdout or to a file.
But Lutra does not have access to stdin/stdout or any POSIX interface (at least not generally).

The only core interface for emitting messages is the program output.

This is so by design; a limited language interface makes it easier to implement a new execution target.
For example, PostgreSQL as an execution target of SQL queries, does not provide any way to print/log from within SQL queries.


## Why the binary format?

Lutra programs executed on SQL databases consume and produce an *interesting* relational data representation.
The binary format is a way to unify these representations and provide a common interface for dealing with databases.

The *interesting* representation is a consequence of Lutra's type system,
which provides an algebraic and **composable** type system.
Composable means that any "container" type (tuple, array or enum) can contain any other type.
So we must support all of the following:
- `[bool]`
- `{text, bool}`
- `{text, {text}, bool}`
- `{text, [text], bool}`
- `[{text, [{bool, bool, [text]}], bool}]`

To support all such types, it means to have a representation of these types within SQL queries, in query results and query parameters.
We try to use efficient representations, but then fallback to encoding values in JSON.

Here are a few examples of query result representations:
- `[bool]`: many rows, one column of type `bool`,
- `{text, bool}`: one row, two columns of types `text` and `bool`,
- `{text, {text}, bool}`: one row, three columns (the inner tuple is flattened into the parent),
- `{text, [text], bool}`: one row, three columns of types `text`, `jsonb` and `bool`,
- `[{text, [{bool, bool, [text]}], bool}]`: many rows, three columns of types `text`, `jsonb` and `bool`.
   The json contains an array of arrays which have 3 elements: a boolean, a boolean and an array of strings.

The representations of these types within queries or as query parameters have subtle differences, because we need to
work around what SQL databases support.

Using the database directly and working with these representations would be very inconvenient and error prone.
Instead, `lutra-runner-postgres` can transcode between the Lutra binary format and these "relational" representations.

The result is abstraction of PostgreSQL as the *runner* interface.
It consumes inputs and produces outputs of arbitrary Lutra types, encoded in the Lutra binary format.
