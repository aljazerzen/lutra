---
title: "Core concepts"
---

## Overview

Lutra can be used in different ways, given that it is a general-purpose query language.

It is most useful in data engineering, where is can replace data frame libraries
(Pandas, Polars, dplyr) and SQL engines (currently only PostgreSQL).

![](./python-overview.drawio.svg){width="100%"}

### Project

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

```
> lutra check project.lt
All good.
```

Once the project is successfully checked, it can then generate executable programs.

### Program

A **program** consists of compiled code along with type annotations for its input and output values.
It is produced by compiler and embedded into the calling program.

When using Python, Lutra CLI can be used to generate a `.py` file with type definitions, encoders, and compiled programs. This file must then be included in the distribution package of the Python project.
The CLI codegen can either be invoked manually or hooked into [build systems such as hatchling](https://hatch.pypa.io/latest/config/build/#build-hooks).

When using Rust, it is most convenient to compile Lutra projects from `build.rs` script
and include them in the compiled Rust binary via `include_bytes!`.

### Runner

A **runner** executes programs by taking input and producing output, both in binary format.
Currently, there are two runners available:

- `lutra-interpreter`: a local interpreter that executes programs in the same process,
- `lutra-runner-postgres`: a runner that executes SQL programs in as a query in PostgreSQL database.

```
> lutra run --project project.lt --interpreter --program my_program
{
  id = 3,
  title = "Forrest Gump",
}
```

## Usage example

Let's start with Python and a data-engineering task of reading data from PostgreSQL
and writing it to a local Parquet file.

```py
import pandas as pd
import sqlalchemy

# query PostgreSQL
engine = sqlalchemy.create_engine("postgresql+psycopg2://localhost:5432")
df = pd.read_sql(
    """
    SELECT category, SUM(amount) AS total_amount
    FROM transactions
    WHERE created_at >= :since
    GROUP BY category
    """,
    engine,
    params={"since": "2024-01-01"}
)

# apply a few transformations
df["total_amount"] = df["total_amount"].astype(float)
df = df.sort_values("total_amount", ascending=False)
df["total_amount_usd"] = df["total_amount"] * 1.1

# write to Parquet
df.to_parquet("breakdown.parquet", index=False)
```

Now, let's replace SQL and Pandas with Lutra programs:

```py
import lutra_runner_postgres as l_pg
import lutra_runner_interpreter as l_int

import generated_project as p

# query PostgreSQL
runner = l_pg.Runner('postgres://localhost:5432').await
breakdown = runner.execute(
    p.compute_breakdown(),  # compiled program
    p.ComputeBreakdownInput(  # program inputs
        since='2025-12-23'
    ),
).await

# print results
for b in breakdown:
    print(b)  # b is p.Breakdown data class

# transform and write using a local interpreter
runner = l_int.Runner('./').await
runner.execute(
    p.write_breakdown(),
    breakdown,
).await
```

The script calls two Lutra programs: one is executed on PostgreSQL
and the other locally.
Both programs are written in `main.lt` file:

```lt
## PostgreSQL part: read transactions table
func from_transactions(): [Transaction] -> sql::from("transactions")

type Transaction: {category: text, amount: Decimal, created_at: Date}

func compute_breakdown(since: Date): [Breakdown] -> (
  from_transactions()
  | filter(func (t) -> t.created_at >= since)
  | group_map(
    func (t) -> t.category,
    func (key, transactions) -> {
      category = key,
      total_amount = transactions | map(func (t) -> t.amount) | sum,
    }
  )
)

type Breakdown: {category: text, total_amount: Decimal}

## Local part: write breakdown to Parquet file
func write_breakdown(breakdown: [Breakdown]) -> (
  breakdown
  | sort(func (x) -> x.total_breakdown)
  | map(func (x) -> {
    ..x,
    total_amount_usd = x.total_amount * 1.1,
  })
  | fs::write_parquet("summary.parquet")
)
```

Lutra language is great for expressing types and high-level operations.
It might be verbose, but it is makes it clear what the programs do - even if
we are only looking at source code, without knowledge of the database schema.

Before running the Python script, we have to compile the Lutra project and generate `generated_project.py`, which will be imported into Python:

```sh
$ lutra codegen main.lt generated_project.py
```
