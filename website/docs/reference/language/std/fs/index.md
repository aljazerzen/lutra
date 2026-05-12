# `std::fs` - File system operations

## `func` read_parquet

```lutra
func read_parquet(text): [R]
where R: {..}
```

Reads rows from a Parquet file at `file_name`.

## `func` write_parquet

```lutra
func write_parquet([R], text): {}
where R: {..}
```

Writes `data` to a Parquet file at `file_name`.

