# `std::fs` - File system operations

## `func` read_parquet

```lutra
func read_parquet(file_name: text): [R]
where R: {..}
```

Reads rows from a Parquet file at `file_name`.

## `func` write_parquet

```lutra
func write_parquet(data: [R], file_name: text): {}
where R: {..}
```

Writes `data` to a Parquet file at `file_name`.

