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

```lt
import std::(filter, sort, slice)

# Load all the data at once
func landing_page() -> {
  user = get_current_user(),
  posts = (
    get_posts()
    | filter(func (p) -> !p.is_draft)
    | sort(func (p) -> -p.created_at)
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
  | find(func (c) -> c.id == id)
  | std::or_default()
)

func main() -> (
  get_invoices()
  | filter(func (i) -> (i.invoice_date: Date).0 >= @1970-01-16.0)
  | map(func (i: Invoice) -> {
    ..i,
    income = i.total - transaction_fees
  })
  | filter(func (i) -> i.income > 1.0)
  | group(func (i) -> i.customer_id)
  | map(func (group) -> {
    customer_id = group.key,
    total = group.values | map(func (i) -> i.total | std::to_int64) | mean,
    sum_income = group.values | map(func(i) -> i.income | std::to_int64) | sum,
    ct = count(group.values),
  })
  | sort(func (i) -> -i.sum_income)
  | slice(0, 10)
  | map(func (i) -> {
    i.customer_id,
    customer = get_customer(i.customer_id), # customer is a tuple
    i.sum_income,
  })
  | map(func (i) -> {
    i.customer_id,
    name = f"{i.customer.last_name}, {i.customer.first_name}",
    sum_income = i.sum_income,
  })
)
```

Again, this all compiles to SQL and can be executed on PostgreSQL.

[More code examples](https://codeberg.org/lutra/lutra/src/branch/main/tests/tests/corpus/complex.lt)

