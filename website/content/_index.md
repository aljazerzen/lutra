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
  | find(this -> this.id == album_id)
)
```

It is minimal and designed to be extended to new execution targets.
Currently, it can run on a reference-implementation interpreter and PostgreSQL.

```lt
import std::(filter, sort, slice)

# Load all the data at once
func landing_page() -> {
  user = get_current_user(),
  posts = (
    get_posts()
    | filter(p -> !p.is_draft)
    | sort(p -> -p.created_at)
    | slice(0, 10)
  ),
}
```

It is verbose in exchange for type safety, readability and composability.
```lt
import std::(Date, find, filter, map, group, mean, sum, count, sort, slice)

# Constant
const transaction_fees: float64 = 0.8

# Type definition
type Invoice: {
  customer_id: int32,
  invoice_date: Date,
  total: float64,
}

# Function that reads a table
func get_invoices(): [Invoice] -> (
  std::sql::from("invoices")
)

type Customer: {
  id: int32,
  first_name: text,
  last_name: text,
}

# Function that performs an index lookup
func get_customer(id: int32): Customer -> (
  std::sql::from("customers")
  | find(c -> c.id == id)
  | std::or_default()
)

func main() -> (
  get_invoices()
  | filter(i -> (i.invoice_date: Date).0 >= @1970-01-16.0)
  | map(func (i: Invoice) -> {
    ..i,
    income = i.total - transaction_fees
  })
  | filter(i -> i.income > 1.0)
  | group(i -> i.customer_id)
  | map(group -> {
    customer_id = group.key,
    total = group.values | map(i -> i.total | std::to_int64) | mean,
    sum_income = group.values | map(func(i) -> i.income | std::to_int64) | sum,
    ct = count(group.values),
  })
  | sort(i -> -i.sum_income)
  | slice(0, 10)
  | map(i -> {
    i.customer_id,
    customer = get_customer(i.customer_id), # customer is a tuple
    i.sum_income,
  })
  | map(i -> {
    i.customer_id,
    name = f"{i.customer.last_name}, {i.customer.first_name}",
    sum_income = i.sum_income,
  })
)
```

Again, this all compiles to SQL and can be executed on PostgreSQL.

## See more

<ul class="card-list">

{% card(title="Core concepts", href="/docs/usage/core-concepts/", label="Docs") %}
    How can Lutra be used? How does it interact with other tools?
{% end %}

{% card(title="Try it out", href="/docs/usage/command-line/", label="Docs") %}
    Install CLI that can check, compile and run Lutra programs.
{% end %}

{% card(title="Advent of SQL 2024", href="https://codeberg.org/lutra/advent-of-sql", label="Examples") %}
    In December of 2025 I've been solving Advent of SQL 2024 in Lutra. See many examples there.
{% end %}

{% card(title="Language test corpus", href="https://codeberg.org/lutra/lutra/src/branch/main/tests/tests/corpus/complex.lt", label="Examples") %}
    Test suite covers all that is possible with the Lutra language.
{% end %}

{% card(title="Zulip Chat", href="https://lutra.zulipchat.com/", label="Community") %}
    If you want to get involved or just have feedback join the Zulip workspace I've created.
{% end %}

</ul>