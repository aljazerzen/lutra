# `std::sql` - SQL interface

Module available only when targeting SQL databases.
Supported SQL targets are DuckDB (`repr:sql-duckdb`) and PostgreSQL
(`repr:sql-pg`). Storage details differ between runners; see the
DuckDB reference and the type representations reference.

## `func` from

```lutra
func from(const table_name: text): [R]
where R: {..}
```

Reads rows from a table.

The order of table rows is unknown and might change over repeated invocations.

Table identifier can contain slashes to denote table namespaces.
Any additional slashes will remain in table name verbatim.
For example:
- `from("my_schema/invoices")` will read from `my_schema.invoices`,
- `from("my_schema/hello/world")` will read from `my_schema."hello/world"`.

## `func` insert

```lutra
func insert(rows: [R], const table_name: text): {}
where R: {..}
```

Inserts rows into a table.

For table identifier format, see documentation of [`module::from`](#func-from).

## `func` update

```lutra
func update(const table_name: text, updater: func (R): enum {none, some: R}): {}
where R: {..}
```

Updates rows in a table.

The `updater` function is applied to each row in the table. When it returns
`.none` the row is not updated, and when it return `.some(new_value)` it is
updated to the new value.

The function receives the current row values and can use them to decide
whether to update and what the new values should be.

Example:
```lt
sql::update(
  "users",
  func (u: User) -> if u.age > 65 then (
    .some({status = "senior", ..u})
  ) else (
    .none
  )
)
```

For table identifier format, see documentation of [`from`](#func-from).

## `func` raw

```lutra
func raw(const sql_source: text): R
where R
```

Evaluates raw SQL.

This function is an escape hatch for accessing SQL directly.
It should be used as a last resort, because it circumvents type checking
and can cause program panic on malformed SQL or incorrect resulting type.

Resulting type of the expression must match the SQL representation of the
Lutra type for the selected runner.

