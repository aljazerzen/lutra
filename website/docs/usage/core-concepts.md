---
title: "Core concepts"
---

## Project

The starting point for Lutra is a **project**.
Usually, this is a directory of `.lt` files, which contain type definitions and the code of the functions.

```lt
# project.lt

type Movie: {id: int32, title: text}

const my_movies: [Movie] = [
  {id = 3, title = "Forrest Gump"},
  {id = 7, title = "Conclave"},
]

func get_movie(param: int32) -> (
  my_movies
  | std::find(func (x: Movie) -> x.id == param)
)

func my_program() -> get_movie(3)
```

The compiler can read these files, perform name resolution, type checking, inference, and report any issues found in the code.
Once the project is successfully checked, it can then generate executable programs.

```
> lutra check project.lt
All good.
```

## Program

A **program** consists of compiled code along with type annotations for its input and output values.
It is produced by compiler and embedded into the calling program.

For example, when using Rust, it is most convenient to compile Lutra projects from `build.rs` script
and include them in the compiled Rust binary via `include_bytes!`.

## Runner

A **runner** executes programs by taking input and producing output, both in binary format.
Currently, there are two runners available:

- `lutra-interpreter`: a local interpreter that executes programs in the same process,
- `lutra-runner-postgres`: a PostgreSQL runner that executes SQL programs in a database.

```
> lutra run --project project.lt --interpreter --program my_program
{
  id = 3,
  title = "Forrest Gump",
}
```
