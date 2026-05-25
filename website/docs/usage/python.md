---
title: Python guide
---

Use Lutra from Python when you want to:

- define typed queries and transformations in `.lt` files,
- generate Python models from Lutra types,
- run compiled programs against PostgreSQL or another runner,
- keep query logic out of raw SQL strings and preserve type information.

This page shows the basic workflow: write a small Lutra project, generate Python
bindings, then run a program from Python.

## Project layout

A simple project might look like this:

```text
my_project/
├── pyproject.toml
├── main.py
└── main.lt
```

The Lutra source lives in `main.lt`. Your Python code imports the generated file
and calls the compiled programs.

## Write a small Lutra project

Suppose you have a PostgreSQL table:

```sql
CREATE TABLE movies (
  id INTEGER NOT NULL PRIMARY KEY,
  title TEXT,
  release_year int2
);
```

You can model that row shape in Lutra and define a function that reads the
table:

```lt title="main.lt"
type Movie: {
  id: Int32,
  title: Text,
  release_year: Int16,
}

func get_movies(): [Movie] -> sql::from("movies")
```

This gives the compiler enough information to type-check the query and generate
matching Python models.

## Generate Python bindings

Use the CLI to compile the project and write the generated Python code:

```console
$ lutra codegen --project ./main.lt ./generated.py
```

The generated file should usually be treated as build output:

- do not edit it by hand,
- regenerate it when the Lutra project changes,
- decide whether to commit it based on your packaging workflow.

The generated file contains:

- Python dataclasses for your Lutra types,
- codecs for encoding and decoding values,
- typed program constructors such as `get_movies()`.

Conceptually, the generated API will look like this:

```py
@dataclasses.dataclass()
class Movie:
    id: int
    title: str
    release_year: int


def get_movies() -> lutra_bin.TypedProgram[(), list[Movie]]:
    ...
```

## Add Python dependencies

Install the runtime packages you need:

```console
$ uv add lutra-bin lutra-runner-postgres
```

Use `lutra-bin` for encoding and decoding values, and
`lutra-runner-postgres` for running SQL-backed Lutra programs on PostgreSQL.

## Run a program from Python

Now you can call the generated program from Python.

```py title="main.py"
import asyncio

import generated as g
import lutra_runner_postgres as l_pg


async def main() -> None:
    runner = await l_pg.Runner("postgres://user:pass@localhost:5432")
    movies = await runner.execute(g.get_movies(), ())
    print(movies)


if __name__ == "__main__":
    asyncio.run(main())
```

The flow is:

1. import the generated program,
2. create a runner,
3. execute the program with typed input,
4. receive typed Python output.

For `get_movies()`, the input is `()`, because the program takes no arguments.
The result is a `list[Movie]`.

## Pass program inputs

Programs can also take typed inputs.

```lt title="main.lt"
func get_movies_after(year: Int16): [Movie] -> (
  get_movies()
  | filter(m -> m.release_year >= year)
)
```

From Python, you pass the input value when executing the program:

```py
movies = await runner.execute(g.get_movies_after(), 2020)
```

That keeps the interface typed on both sides.

## End-to-end workflow

A larger Python workflow often uses more than one Lutra program.
For example, you might:

1. run one program on PostgreSQL to fetch and aggregate data,
2. pass the typed result back into Python,
3. run another program locally for post-processing or file output.

<figure markdown="1" style="width: 100%">
  ![](./python-overview.drawio.svg){width="100%"}
  <figcaption markdown="span">
  Example Python workflow with PostgreSQL and a local runner.<br/>
  One Lutra project can define programs for more than one execution target.
  </figcaption>
</figure>

A project for that workflow might look like this:

```lt title="main.lt"
func from_transactions(): [Transaction] -> sql::from("transactions")

type Transaction: {category: Text, amount: Decimal, created_at: Date}

func compute_breakdown(since: Date): [Breakdown] -> (
  from_transactions()
  | filter(t -> t.created_at >= since)
  | group_map(
    t -> t.category,
    func (key, transactions) -> {
      category = key,
      total_amount = transactions | map(t -> t.amount) | sum(),
    }
  )
)

type Breakdown: {category: Text, total_amount: Decimal}

func write_breakdown(breakdown: [Breakdown]) -> (
  breakdown
  | sort(x -> x.total_amount)
  | fs::write_parquet("summary.parquet")
)
```

From Python, you would call the two programs separately:

```py
import asyncio

import generated as g
import lutra_runner_interpreter as l_int
import lutra_runner_postgres as l_pg


async def main() -> None:
    postgres = await l_pg.Runner("postgres://user:pass@localhost:5432")
    breakdown = await postgres.execute(
        g.compute_breakdown(),
        g.ComputeBreakdownInput(since="2025-12-23"),
    )

    local = await l_int.Runner(".")
    await local.execute(g.write_breakdown(), breakdown)


if __name__ == "__main__":
    asyncio.run(main())
```

The important idea is that Python stays in charge of orchestration, while Lutra
holds the typed transformation logic.

## When to use this workflow

This workflow is a good fit when you want to:

- keep query logic in Lutra instead of raw SQL strings,
- share typed data structures between query code and application code,
- generate Python-facing models from a single source of truth,
- combine database-backed programs with other runners later.

## See also

- [Command line guide](cli.md) for installation and first-use workflow.
- [Tabular data basics](../learn/tabular-data.md) and [Aggregations](../learn/aggregations.md) for the data-oriented language guides.
- [Reference: Modules](../reference/language/modules.md) if you are organizing larger Lutra projects.
- [CLI reference](../reference/runtime/cli.md) for exact `codegen` and `run` usage.
[Runner model](../reference/runtime/runner-model.md) if you want to understand the runtime boundary.
