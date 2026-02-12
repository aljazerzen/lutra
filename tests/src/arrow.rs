use std::fs;

use arrow::array::{
    ArrayRef, BooleanArray, Float64Array, Int32Array, Int64Array, RecordBatch, StringArray,
    StructArray,
};
use arrow::datatypes::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::file::properties::WriterProperties;
use temp_dir::TempDir;

fn _write_parquet_file(dir: &TempDir, filename: &str, batch: RecordBatch) {
    let path = dir.path().join(filename);
    let file = fs::File::create(path).unwrap();
    let props = WriterProperties::builder().build();
    let mut writer = ArrowWriter::try_new(file, batch.schema(), Some(props)).unwrap();
    writer.write(&batch).unwrap();
    writer.close().unwrap();
}

#[test]
fn test_interface_primitives_no_nulls() {
    let test_dir = TempDir::new().unwrap();

    // Create a parquet file with primitive types, no nulls
    let schema = Schema::new(vec![
        Field::new("id", DataType::Int32, false),
        Field::new("name", DataType::Utf8, false),
        Field::new("score", DataType::Float64, false),
        Field::new("active", DataType::Boolean, false),
    ]);

    let batch = RecordBatch::try_new(
        std::sync::Arc::new(schema),
        vec![
            std::sync::Arc::new(Int32Array::from(vec![1, 2, 3])) as ArrayRef,
            std::sync::Arc::new(StringArray::from(vec!["Alice", "Bob", "Charlie"])) as ArrayRef,
            std::sync::Arc::new(Float64Array::from(vec![95.5, 87.3, 92.1])) as ArrayRef,
            std::sync::Arc::new(BooleanArray::from(vec![true, false, true])) as ArrayRef,
        ],
    )
    .unwrap();

    _write_parquet_file(&test_dir, "users.parquet", batch);

    // Generate interface
    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    insta::assert_snapshot!(interface, @r#"
    func users(): [{
      id: int32,
      name: text,
      score: float64,
      active: bool,
    }] -> std::fs::read_parquet("users.parquet")

    "#);
}

#[test]
fn test_interface_optional_with_nulls() {
    let test_dir = TempDir::new().unwrap();

    // Create a parquet file with optional fields that contain nulls
    let schema = Schema::new(vec![
        Field::new("id", DataType::Int32, false),
        Field::new("nickname", DataType::Utf8, true), // nullable
    ]);

    let batch = RecordBatch::try_new(
        std::sync::Arc::new(schema),
        vec![
            std::sync::Arc::new(Int32Array::from(vec![1, 2, 3])) as ArrayRef,
            std::sync::Arc::new(StringArray::from(vec![Some("nick1"), None, Some("nick3")]))
                as ArrayRef,
        ],
    )
    .unwrap();

    _write_parquet_file(&test_dir, "users_with_nulls.parquet", batch);

    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    // Should wrap nullable field in option enum because it has nulls
    insta::assert_snapshot!(interface, @r#"
    func users_with_nulls(): [{
      id: int32,
      nickname: enum {none, some: text},
    }] -> std::fs::read_parquet("users_with_nulls.parquet")

    "#);
}

#[test]
fn test_interface_optional_without_nulls() {
    let test_dir = TempDir::new().unwrap();

    // Create a parquet file with optional field but no actual nulls
    let schema = Schema::new(vec![
        Field::new("id", DataType::Int32, false),
        Field::new("nickname", DataType::Utf8, true), // nullable in schema
    ]);

    let batch = RecordBatch::try_new(
        std::sync::Arc::new(schema),
        vec![
            std::sync::Arc::new(Int32Array::from(vec![1, 2, 3])) as ArrayRef,
            std::sync::Arc::new(StringArray::from(vec![
                Some("nick1"),
                Some("nick2"),
                Some("nick3"),
            ])) as ArrayRef,
        ],
    )
    .unwrap();

    _write_parquet_file(&test_dir, "users_no_nulls.parquet", batch);

    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    // Should NOT wrap in option enum because there are no nulls
    insta::assert_snapshot!(interface, @r#"
    func users_no_nulls(): [{
      id: int32,
      nickname: text,
    }] -> std::fs::read_parquet("users_no_nulls.parquet")

    "#);
}

#[test]
fn test_interface_nested_struct() {
    let test_dir = TempDir::new().unwrap();

    // Create a parquet file with nested struct
    let inner_schema = vec![
        Field::new("street", DataType::Utf8, false),
        Field::new("city", DataType::Utf8, false),
        Field::new("zip", DataType::Int32, true), // optional nested field
    ];

    let schema = Schema::new(vec![
        Field::new("id", DataType::Int32, false),
        Field::new(
            "address",
            DataType::Struct(inner_schema.clone().into()),
            false,
        ),
    ]);

    let address_data = StructArray::from(vec![
        (
            std::sync::Arc::new(Field::new("street", DataType::Utf8, false)),
            std::sync::Arc::new(StringArray::from(vec!["123 Main St", "456 Oak Ave"])) as ArrayRef,
        ),
        (
            std::sync::Arc::new(Field::new("city", DataType::Utf8, false)),
            std::sync::Arc::new(StringArray::from(vec!["NYC", "LA"])) as ArrayRef,
        ),
        (
            std::sync::Arc::new(Field::new("zip", DataType::Int32, true)),
            std::sync::Arc::new(Int32Array::from(vec![Some(10001), Some(90001)])) as ArrayRef,
        ),
    ]);

    let batch = RecordBatch::try_new(
        std::sync::Arc::new(schema),
        vec![
            std::sync::Arc::new(Int32Array::from(vec![1, 2])) as ArrayRef,
            std::sync::Arc::new(address_data) as ArrayRef,
        ],
    )
    .unwrap();

    _write_parquet_file(&test_dir, "users_with_address.parquet", batch);

    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    // Nested optional fields are conservatively wrapped in option
    insta::assert_snapshot!(interface, @r#"
    func users_with_address(): [{
      id: int32,
      address: [{
        street: text,
        city: text,
        zip: int32,
    }],
    }] -> std::fs::read_parquet("users_with_address.parquet")
    "#);
}

#[test]
fn test_interface_multiple_files() {
    let test_dir = TempDir::new().unwrap();

    // Create multiple parquet files
    let schema1 = Schema::new(vec![Field::new("id", DataType::Int32, false)]);
    let batch1 = RecordBatch::try_new(
        std::sync::Arc::new(schema1),
        vec![std::sync::Arc::new(Int32Array::from(vec![1, 2])) as ArrayRef],
    )
    .unwrap();
    _write_parquet_file(&test_dir, "file1.parquet", batch1);

    let schema2 = Schema::new(vec![Field::new("name", DataType::Utf8, false)]);
    let batch2 = RecordBatch::try_new(
        std::sync::Arc::new(schema2),
        vec![std::sync::Arc::new(StringArray::from(vec!["a", "b"])) as ArrayRef],
    )
    .unwrap();
    _write_parquet_file(&test_dir, "file2.parquet", batch2);

    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    // Should have both functions (order may vary)
    assert!(interface.contains("func file1()"));
    assert!(interface.contains("func file2()"));
    assert!(interface.contains("id: int32"));
    assert!(interface.contains("name: text"));
}

#[test]
fn test_interface_nested_directories() {
    let test_dir = TempDir::new().unwrap();
    let sub_dir = test_dir.path().join("subdir");
    fs::create_dir_all(&sub_dir).unwrap();

    // Create file in subdirectory
    let schema = Schema::new(vec![Field::new("value", DataType::Int64, false)]);
    let batch = RecordBatch::try_new(
        std::sync::Arc::new(schema),
        vec![std::sync::Arc::new(Int64Array::from(vec![100, 200])) as ArrayRef],
    )
    .unwrap();

    let path = sub_dir.join("data.parquet");
    let file = fs::File::create(path).unwrap();
    let props = WriterProperties::builder().build();
    let mut writer = ArrowWriter::try_new(file, batch.schema(), Some(props)).unwrap();
    writer.write(&batch).unwrap();
    writer.close().unwrap();

    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    // Should find nested file with path
    insta::assert_snapshot!(interface, @r#"
    func data(): [{
      value: int64,
    }] -> std::fs::read_parquet("subdir/data.parquet")

    "#);
}

#[test]
fn test_interface_empty_directory() {
    let test_dir = TempDir::new().unwrap();

    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    // Should return empty string for empty directory
    assert_eq!(interface, "");
}

#[test]
fn test_interface_non_parquet_files() {
    let test_dir = TempDir::new().unwrap();

    // Create some non-parquet files
    fs::write(test_dir.path().join("readme.txt"), "test").unwrap();
    fs::write(test_dir.path().join("data.csv"), "a,b,c").unwrap();

    let interface = lutra_arrow::get_interface(test_dir.path()).unwrap();

    // Should ignore non-parquet files
    assert_eq!(interface, "");
}
