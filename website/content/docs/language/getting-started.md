---
title: Getting started
weight: 1
---

Easiest way to try out the language is to run .lt files with the CLI. To do that, first install the CLI:

```sh
$ cargo install --git https://codeberg.org/aljazerzen/lutra lutra-cli
```

Now create a file named `example.lt` with the following content:

```lt
let main = func () -> "Hello world!"
```

This file contains a single function called `main`, which just returns text `Hello world!`. Run the function:

```sh
$ lutra run example.lt
"Hello world!"
```

This guide will show different examples, each of which can be
placed into a `.lt` file and executed with `lutra run`.

<!-- Note that there is no `print` function, we just evaluate the function and the CLI prints the result. -->

## Tuples

There are 3 container types: tuples, arrays and enums. Let's start with tuples, the product type.

Tuples are used to combine multiple values together, so they can be returned from functions as a single value.

```lt
let main = func () -> {"Hello world!", true, 42}
```

In the body of the main function, we now use curly braces `{}`, with three tuple fields.

In the braces, we can place any number of fields of any type - even another nested tuple. Fields can also be named, for example:

```lt
let main = func () -> {id = 5, title = "Prestige", 2006}
```

Instead of constructing this tuple in the `main` function, let's extract it into a constant, named `movie`:

```lt
let movie = {id = 5, title = "Prestige", 2006}

let main = func () -> movie
```

That did not change the result, but it allows us to show how to pick just `title` and release year out of the `movie`:


```lt
let movie = {id = 5, title = "Prestige", 2006}

let main = func () -> {movie.title, movie.2}
```

We can obviously refer to named fields by name, but to pick release year, we must use its position. In this case, the position is 2, because we start counting at 0.

A major constraint of tuples is that they can only have a
fixed number of fields. For example, if we wanted to return all actors of a movie and decided to store them in a tuple, each movie would have to have the same number of actors.

We don't want that, so we will use an array instead.

## Arrays

Arrays are containers that can contain many items.
They use square brackets `[]` and items are delimited by commas, with optional trailing comma (this applies to tuples too).

```lt
let actors = [
  "Hugh Jackman",
  "Christian Bale",
  "Michael Caine",
]

let main = func () -> actors
```

Note that all items of the array must be of the same type. We couldn't place a `5`, `true`, or `{name = ""Piper Parabo"}` into the array above.

## Incomplete

This guide is incomplete. We should also include following examples:

```lt
let actors = ...

let main = func () -> std::slice(actors, 0, 2)
```

```lt
let actors = ...

let main = func () -> (
    actors | std::slice(0, 2)
)
```

```lt
let movie1 = {
  id = 5,
  title = "Prestige",
  2006,
  actors = [
    "Hugh Jackman",
    "Christian Bale",
    "Michael Caine",
  ],
}

let movie2 = {
  id = 6,
  title = "Her",
  2013,
  actors = [
    "Joaquin Phoenix",
    "Scarlett Johansson",
  ],
}

let movies = [movie1, movie2]

let main = func () -> movies
```


```lt
...

type Movie = {
  id = int,
  title = text,
  int,
  actors = [text],
}

let main = func (): [Movie] -> [movie1, movie2]
```


```lt
...

let get_title = func (m: Movie) -> m.title

let main = func () -> get_title(movie1)
```

```lt
...

let main = func () -> std::map(movies, get_title)
```

```lt
...

let main = func () -> (movies | std::map(get_title))
```

