---
title: 'Use from Python'
---

Let's assume we have a Python application and a PostgreSQL database server were we want to
fetch data from. Using Lutra will help us use a modern query language to access the database
while cascading type information from our database out to the Python codebase.


## Project structure

First we need to setup the Lutra project alongside Python files.
A basic modern Python project would have a `pyproject.toml` and either
`main.py` or `src/` directory.

For lutra, we similarly need either a single `.lt` file or a directory.
So let's name it `main.lt`, so we have following project structure.

```
my_project/
  pyproject.toml
  main.py
  main.lt
```

To access our PostgreSQL, we need to know it's schema.
Let's say that the database has been created with this DDL:

```sql
CREATE TABLE movies (
  id INTEGER NOT NULL PRIMARY KEY,
  title TEXT,
  release_year int2
);
```

We want to translate that to Lutra so the compiler will be able to
validate our code and generate Python models.

First, we define the type of the table row and then define a function that
provides the table values.

```lt
# main.lt

# the type definition of table row
type Movie: {
  id: int32,
  title: text,
  release_year: int16,
}

# function named get_movies that returns an array of movie
func get_movies(): [Movie] -> (
  # it is implemented by just reading the sql table
  std::sql::from("movies");
)
```

That should be enough.


## Generating Python models

We now need to turn the Lutra type definitions into Python model, which will
provide serialization codecs and type hints for type checkers (mypy) and editors.

To do that, we will use lutra CLI:

```
> lutra codegen main.lt generated.py
```

This will compile `main.lt`, check it for errors and output python code into
`generated.py`. This file should probably be excluded from version control
and should not be manually modified. It would contain something like this:

```py
@dataclasses.dataclass()
class Movie:
    id: int
    title: str
    release_year: int

    def get_codec(self) -> MovieCodec: ...

class MovieCodec(lutra_bin.Codec): ...

def get_movies() -> lutra_bin.TypedProgram[(), list[Movie]]:
    ...
```

There is our `Movie` type, which has been translated to a Python `@dataclass` and
a corresponding codec. Then there is a `get_movies` function which returns a typed program.

To run such a program, we need the last piece of the puzzle: a runner.


## PostgreSQL runner

For the runner we will using a PostgreSQL client, wrapped into an interface that will encode
program inputs, decode program outputs and execute the program by executing the SQL query
from the compiled program.

For that we will need two dependencies:
- `lutra-bin`, for endocing and decoding, and
- `lutra-runner-postgres`, for connecting to the PostgreSQL.

```sh
$ uv add lutra-bin lutra-runner-postgres
```

We can now initialize the runner in our `main.py` file:

```py
import lutra_bin
import lutra_runner_postgres as l_pg

async def main():
    runner = l_pg.Runner('postgres://user:pass@localhost:5432').await


main() # TODO: how do I await here?
```

To run our `get_movies`, we have to import it and then use `execute()` method on the runner:

```py
import generated as g

movies = runner.execute(g.get_movies(), ()).await

print(movies)
```

When we place this snippet into the main function, we can run the Python script, and we should
see something like this:

```sh
$ python main.py
[Movie(id=1, title="...", release_year=2009), ...]
```

And that, dear reader, is the simplest type-safe way of querying PostgreSQL.

---

Things this page is missing:

- show how lutra queries might return derived types, and how this translates to Python dataclasses,
- show program inputs,
