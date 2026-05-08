---
title: Type representations
slug: "repr"
---

Lutra programs can be executed on many engines, each of which can have multiple
representations (repr) of the same logical value.

For example, when executing on PostgreSQL, tables contain "postgres repr", which
is converted into computation friendly "query repr" and at the end,
lutra-runner-postgres converts that into "binary repr"

## Binary repr

**Purpose**: storage of Lutra values in buffers of bytes. Interoperability.

**Location**: `lutra-bin`

**Rules**: See [Lutra format specification](./format-spec/).

---

## Query repr

**Purpose:** Internal representation during SQL query computation.

**Location:** `lutra-compiler/src/sql/utils/projection.rs`

**Rules:**

- Primitive → 1 column
- Tuple → columns from field positions `_0, _1, ...`, flattened recursively
- Array (top-level) → `index` column + columns of the inner type
- Array (nested) → 1 column (in serialized repr)
- Enum (optional primitive) → 1 nullable column
- Enum (other) → `_t` tag (int2) + columns of variants

| Lutra type                                 | Query repr columns        |
|--------------------------------------------|---------------------------|
| `int32`                                    | `value`                   |
| `{x: int32, y: int16}`                     | `_0, _1`                  |
| `{x: int32, y: {a: int16, b: int16}}`      | `_0, _1_0, _1_1`          |
| `[int32]`                                  | `index, value` (per row)  |
| `[{x: int32, y: int16}]`                   | `index, _0, _1` (per row) |
| `{id: int32, tags: [text]}`                | `_0, _1`                  |
| `enum {none, some: int32}`                 | `value` (nullable)        |
| `enum {a: int32, b: {x: int16, y: int16}}` | `_t, _0, _1_0, _1_1`      |

---

## Postgres repr

**Purpose:** PostgreSQL table storage (`sql::from`, `sql::insert`, `sql::raw`).

**Location:** `lutra-compiler/src/sql/queries/repr_pg.rs`

**Rules:**

- Primitive → 1 column
- Tuple → columns from field names `id, address, ...`, flattened recursively
- Array (top-level) → columns of the inner type, unordered
- Array (nested) → 1 column (in serialized repr)
- Enum (optional primitive) → 1 nullable column
- Enum (other) → tag (text) + columns of variants

| Lutra type                            | Postgres repr columns           |
|---------------------------------------|---------------------------------|
| `int32`                               | `value: int4`                   |
| `{x: int32, y: int16}`                | `x::int4, y::int2`              |
| `{x: int32, y: {a: int16, b: int16}}` | `x::int4, y.a::int2, y.b::int2` |
| `{id: int32, tags: [text]}`           | `id::int4, tags::jsonb`         |
| `[int32]`                             | `value::int4` (per row)         |
| `[{x: int32, y: int16}]`              | `x::int4, y::int2` (per row)    |
| `enum {none, some: int32}`            | `value::int4` (nullable)        |
| `enum {a: int32, b: text}`            | `value::text, a::int4, b::text` |

---

## JSON repr

**Purpose:** Serialized format for complex values, used by PostgreSQL dialect.

**Location:** `lutra-compiler/src/sql/queries/serialization.rs`

**Rules:**

- Primitive → JSON value
- Array → JSON array `[item0, item1, ...]`
- Tuple → JSON array `[field0, field1, ...]` (positional)
- Enum → `{"tag": variant_inner}`, where tag is variant position.

| Lutra Type                            | JSON repr     |
|---------------------------------------|---------------|
| `42: int32`                           | `42`          |
| `"hello": text`                       | `"hello"`     |
| `{x: 1, y: 2}`                        | `[1, 2]`      |
| `{x: 1, y: {a: 2, b: 3}}`             | `[1, [2, 3]]` |
| `[1, 2, 3]: [int32]`                  | `[1, 2, 3]`   |
| `[{x: 1}, {x: 2}]`                    | `[[1], [2]]`  |
| `.none: enum {none, some: int32}`     | `{"0": []}`   |
| `.some(42): enum {none, some: int32}` | `{"1": 42}`   |
| `.a(4200): enum {a: int32, b: text}`  | `{"0": 4200}` |
| `.b("hi"): enum {a: int32, b: text}`  | `{"1": "hi"}` |

---

## DuckDB repr

**Purpose:** DuckDB table storage (`sql::from`, `sql::insert`, `sql::raw`).

**Location:** `lutra-compiler/src/sql/queries/repr_duckdb.rs`

**Rules:**

- Primitive → 1 column
- Tuple (top-level) → columns from field names `id, address, ...`, not flattened
- Tuple (nested) → STRUCT type
- Array (top-level) → columns of the inner type
- Array (nested) → LIST type
- Enum (optional primitive) → 1 nullable column
- Enum (other) → UNION type

| Lutra Type                            | DuckDB Table Columns                 |
|---------------------------------------|--------------------------------------|
| `int32`                               | `value: int4`                        |
| `{x: int32, y: int16}`                | `x: int4, y: int2`                   |
| `{x: int32, y: {a: int16, b: int16}}` | `x: int4, y: STRUCT(a int2, b int2)` |
| `{id: int32, tags: [text]}`           | `id: int4, tags: text[]`             |
| `[int32]`                             | `value: int4` (per row)              |
| `[{x: int32, y: int16}]`              | `x: int4, y: int2` (per row)         |
| `enum {none, some: int32}`            | `value: int4` (nullable)             |
| `enum {a: int32, b: text}`            | `value: UNION(a int4, b text)`       |

---

## Arrow repr

**Purpose:** returned by DuckDB queries, can be converted to/from parquet.

**Location:** `lutra-arrow`

**Rules:** Same as DuckDB repr
