---
title: Getting started
---

This guide will:

- show you how to run Lutra programs with CLI,
- teach you about basics of the Lutra language,
- describe how to use Lutra on tabular data.

Prerequisite:

- install [Lutra CLI](../usage/command-line)

---

Let's create a file named `example.lt` with the following content:

```lt
# example.lt

func main() -> "Hello world!"
```

This file contains a single function called `main`, which returns text `Hello world!`.

!!! note

    In Lutra, there is no `print` function. Instead, we evaluate a function and the CLI prints the result.
    This is because Lutra is designed to run on remote machines, without an output stream that could
    carry print messages. For more info, see [runner interface](../runner).

Now run it:

```console
$ lutra run --project example.lt --interpreter
const output = "Hello world!"
```

This command has compiled the project, found the `main` function and executed it on the local interpreter.
Throughout this guide we will see many code snippets that must be placed into the `example.lt` file and executed with `lutra run`.
The easiest way to do this is to repeatedly running lutra with watch command:

```console
$ watch lutra run --project example.lt --interpreter
Every 2,0s: lutra run ...
Compiling...
Executing...
const output = "Hello world!"
```

## Basic concepts

### Constants vs functions

Lutra supports both constants and functions.

Constants are immutable values that are defined using the `const` keyword:

```lt
const user_name = "John Doe"
const pi: float32 = 3.14159

func main() -> pi
```

!!! note

    Constant `pi` requires a type annotation, because we have to decide if we want `3.13159` to be `float32` or `float64`

Constants cannot contain any computation (function calls, control flow, f-strings). Instead, computation is done in functions.
Functions are reusable blocks of code that can take inputs, perform operations, and return values. They are defined using the `func` keyword:

```lt
func greet(name: text) -> f"Hello, {name}!"

func add(a: int64, b: int64) -> a + b

func main() -> {
  greet("Aljaz"),
  add(3, 4),
}
```

### Types

Every variable, parameter and function body has an associated type,
which tells us what kind of values can be placed into that variable.

Types can be (and sometimes must be) annotated either on const definitions,
func definitions or within expressions.

```lt
const name: text = "Aljaz"

func main(): text -> "Hello world!"

const altitude = 2864: int32
```

Possible types include:

- Primitives: `bool`, `int32`, `int64`, `float32`, `float64`, `text`

- Tuples: fixed-size collections of values of different types

- Arrays: variable-size collections of values of the same type

- Enums: one of a fixed set of named values


### Tuples

Tuples are the product type of other types. They use curly braces `{ ... }`
and are used to combine multiple values together, so they can be returned from
functions as a single value.

```lt
func main() -> {"Hello world!", true, 42: int32}
```

Tuples can contain any number of fields of any type - even another nested tuple.
Fields can also be named, for example:

```lt
func main() -> {id = 5: int32, title = "Prestige", 2006: int32}
```

A very similar syntax is used to describe tuples in type annotations:

```lt
const movie: {id: int32, title: text, int32} = {5, "Prestige", 2006}

func main() -> movie
```

!!! note

    Because the constant has annotated type, the tuple itself does not need
    field names or number annotations.

To get field values, we use `.title` or `.2` to refer to a field by name or by
position (starting with 0).

```lt
const movie: {id: int32, title: text, int32} = {5, "Prestige", 2006}

func main() -> {movie.title, movie.2}
```

Here we pick movie title and release year at position 2.

A major constraint of tuples is that they can only have a fixed number of
fields. For example, if we wanted to return all actors of a movie and decided to
store them in a tuple, each movie would have to have the same number of actors.

We don't want that, so we will use an array instead.

### Arrays

Arrays are containers that can contain many items of the same type.
They use square brackets `[]` and contain items delimited by commas.

```lt
const actors = [
  "Hugh Jackman",
  "Christian Bale",
  "Michael Caine",
]

func main() -> actors
```

!!! note

    Arrays, tuples, and enums can contain trailing commas.
    This is a treat of working with a designed modern language.

Array items must all be of the same type. We couldn't place a `5`, `true`, or
`{name = "Piper Parabo"}` into the array above.

```
const actors = [
  "Hugh Jackman",
  true,
  ──┬─
    ╰─── expected type `text`, but found type `bool`
]
```

But array items can be of any type, including tuples and nested array.
This is used to represent tabular data as an array of tuples:

```lt
const movies: [{id: int32, title: text, release_year: int16}] = [
  {5, "Prestige", 2006},
  {6, "Her", 2013},
]

func main() -> movies
```

We could go the other way and represent the same data as a tuple of arrays,
but it would not be exactly the same. Because arrays are of variable size,
the compiler cannot validate that they are of the same length.

```lt
const movies: {id: [int32], title: [text], release_year: [int16]} = {
  id = [5, 6],
  title = ["Prestige", "Her", "Conclave"],  # oops, one too many
  release_year = [2006, 2013]
}

func main() -> movies
```


### Enums

Enums are containers that contain exactly one of the named variants.

```lt
type Color: enum {
  red,
  blue,
  green,
}

const colors: [Color] = [.blue, .blue, .green, .red]

func main() -> colors
```

Variants of the enum can also contain inner types. This is useful when the inner
value only applied to one variant, but not that others.

```lt
type Status: enum {
  pending,
  in_transit: text,
  arrived: Date,
  cancelled: {reason: text, is_refunded: bool},
}

const orders: [Status] = [
  .pending,
  .in_transit("warehouse 1"),
  .arrived(@2025-12-24),
  .cancelled({reason = "lost", is_refunded = true}),
]

func main() -> orders
```

Here, `enum Status` can be exactly one of its variants: `pending`, `in_transit`,
`arrived`, or `cancelled`.
When order is in transit, it must also contain some textual value.
It is not clear what this text means, so it is recommended to use nested tuples,
as they are used for cancelled orders.


### Null

Lutra does not have `NULL` or `NA` values.
Instead, we use a *option* enum (sometimes also called *maybe* or *optional*):

<!-- TODO: make this example use Some and None without prefix. -->
<!-- TODO: don't use type defs (we have not introduced this yet) -->

```lt
type Movie: {
  id: int32,
  title: text,

  # unreleased movies don't have a release_year
  release_year: enum {none, some: int16}
}

const movie1: Movie = {5, "Prestige", .some(2006)}
const movie2: Movie = {9, "Silmarillion", .none}

func main() -> [movie1, movie2]
```


### Modules and imports

Lutra uses modules to organize related code into separate namespaces.
Each file is treated as a separate module.

To use code from another module, you need to import it using the `import` keyword.
This allows you to access functions, types, and constants from the imported modules.

```lt
import std::math::(pi64, pow)

const radius = 5.0: float64

# compute area
func main() -> pi64 * pow(radius, 2.0)
```

### Pipes

When we have many nested function calls the flow of data becomes hard to follow.
For example, in the snippet below, to understand the main function, we have to
start reading at `10` then read left and when we get to `add`, just to the `5`
at the end.

```lt
func add_one(x: int32) -> x + 1

func add(x: int32, y: int32) -> x + y

# this returns 18
func main() -> add(add_one(add_one(add_one(10))), 5)
```

Instead, Lutra has the pipe operator `|`, which places left operand as the first
argument to the right operand.

So instead of `add_one(10)`, we write `10 | add_one`.
Instead of `add(10, 5)`, we write `10 | add(5)`.

If we rewrite the example above, we get this:

```lt
# this is equivalent to the snippet above
func main() -> 10 | add_one | add_one | add_one | add(5)
```

Right side of pipe operator can either be an identifier of a function
(e.g. `add_one`), a function call (e.g. `add(5)`), or an inline function.


## Tabular data

One can get most out of Lutra language in the domain of tabular data (i.e. tables, relations, data frames).
That's because pipelines express natural flow of data from inputs to computation to results, and type checking and inference
make sure that every step of the pipelines is correct and makes sense. 

This section will show how Lutra can be used on tabular data on the example of chinook database.


### Setup

The data is available in the [lutra/chinook](https://codeberg.org/lutra/chinook) repository.
Chinook is an imaginary store which sells music albums.
We will load it into PostgreSQL and query it from there.
To set it up, follow the instructions in the [README](https://codeberg.org/lutra/chinook#readme). 

If all is setup correctly, you should be able to run this command:

```console
$ lutra run --postgres postgres://postgres:pass@localhost \
    --project . \
    --program pg::from_albums
```

This is quite verbose, so the justfile provides a shorthand:

```console
$ just pg-run pg::from_albums
```
  
```lt
const output = [
  {
    album_id = 1,
    title = "For Those About To Rock We Salute You",
    artist_id = 1,
  },
  ...
]
```

For this guide, we will create a submodule in file `tutorial.lt`, where we can experiment and explore.


```lt title="tutorial.lt"
submodule 

func main() -> project::pg::from_albums()
```

!!! note

    In the first line, `submodule` connects this file to the parent `module.lt`.
    This is needed to access `project::pg`, which contains type definitions of
    tables in PostgreSQL.

To invoke our new main function, we use this command:

```console
$ just pg-run tutorial::main
```
  
```lt
const output = [
  {
    album_id = 1,
    title = "For Those About To Rock We Salute You",
    artist_id = 1,
  },
  ...
]
```

### Slice

To limit the number of table rows, we can use `std::slice`.
It takes start and end params and returns a subrange of all items in the array.

```lt
import project::pg
import std::slice

func main() -> (
  pg::from_albums()
  | slice(0, 5)
)
```

Here, we:

1. used imports to make the main program shorter,
1. used `|` to pipe albums into the `slice` function,
1. picked positions `[0, 5)`, or in other words, first 5 albums.

### Filter

If slice selects items based on position, filter selects based on a predicate.

```lt
import std::(filter, text)

func main() -> (
  pg::from_albums()
  | filter(a -> text::starts_with(a.title, "The Best Of"))
)
```
  
The predicate is specified as a function, that is given each album in parameter `a`.
It checks that album's title starts with `"The Best Of"`.

We could also use the pipe operator here, to make it read from left-to-right:

```lt
...
  | filter(a -> a.title | text::starts_with("The Best Of"))
...
```

### Map

Map is used to, well, map each array item into something else.
When working with tabular data, items are generally tuples, so we are mapping
an input tuple to an output tuple.

```lt
import std::(map, slice, text)

func main() -> (
  pg::from_albums()
  | slice(0, 2)
  | map(a -> { # curly braces mean we return a new tuple
    album_id = a.id, # rename id to album_id
    title_len = a.title | text::length(), # compute title length
  })
)
```

```lt
const output = [
  {
    album_id = 1,
    title_len = 37,
  },
  {
    album_id = 2,
    title_len = 17,
  },
]
```

### Joining

To join data from another table, Lutra has no special syntax or function.
Instead, this is accomplisheed with a normal mapping operation.

```lt
import std::(map, slice)

func main() -> (
  pg::from_albums()
  | slice(0, 2)
  | map(a -> {
    a.title,

    # this function find artist by the primary key
    artist = pg::from_artists_by_id(a.artist_id), 
  })
)
```

```lt
const output = [
  {
    title = "For Those About To Rock We Salute You",
    artist = some({
      id = 1,
      name = "AC/DC",
    }),
  },
  {
    title = "Balls to the Wall",
    artist = some({
      id = 2,
      name = "Accept",
    }),
  },
]
```

There is a few interesting things happening here:

1. Each result item is a tuple that contains a nested `artist` tuple.
   This groups related fields together and allows handling the whole `artist` as one value.
1. `artist` is wrapped into a option, which would be `none` if the artist would not exist.
   In this case, that is not really needed, because we have a foreign key on `artist_id` and there will
   always be an artist for each album. To get rid of the option, let's use `option::or_default()`.
   
```lt hl_lines="1 8"
import std::(map, slice, option)

func main() -> (
  pg::from_albums()
  | slice(0, 2)
  | map(a -> {
    a.title,
    artist = pg::from_artists_by_id(a.artist_id) | option::or_default, 
  })
)
```

```lt
const output = [
  {
    title = "For Those About To Rock We Salute You",
    artist = {
      id = 1,
      name = "AC/DC",
    },
  },
  {
    title = "Balls to the Wall",
    artist = {
      id = 2,
      name = "Accept",
    },
  },
]
```

In Lutra, each type has a *default* value, which 0 or empty string or false.
We used `option::or_default()`, which would inject default value for the artist
in the case it was missing. We can see that in action here:

```lt
import std::option

func main() -> {
  pg::from_artists_by_id(1) | option::or_default(),
  pg::from_artists_by_id(666) | option::or_default(),
}
```

```lt
const output = {
  {
    id = 1,
    name = "AC/DC",
  },
  {
    id = 0,
    name = "",
  },
}
```

<div style="height: 5em"></div>

---

**Incomplete**

This guide is currently lacking in depth.
I would still want to cover more topics, such as:

- sort,
- group,
- flat_map,
- control flow (match, if),
- SQL guide (from, insert, expr),
- interpreter guide (fs module & Parquet)



