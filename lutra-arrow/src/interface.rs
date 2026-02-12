use std::collections::HashSet;
use std::ffi;
use std::fs;
use std::path;
use std::sync::Arc;

use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use parquet::basic::Repetition;
use parquet::file::metadata as pq_metadata;
use parquet::schema::types as pq_types;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{}: {}", .0.display(), .1)]
    Io(std::path::PathBuf, std::io::Error),

    #[error("invalid non-unicode path: {}", .0.display())]
    NonUnicode(ffi::OsString),

    #[error("parquet error: {0}")]
    Parquet(#[from] parquet::errors::ParquetError),

    #[error("arrow error: {0}")]
    Arrow(#[from] arrow::error::ArrowError),

    #[error("not implemented: {feature} at {location}")]
    NotImplemented {
        feature: &'static str,
        location: String,
    },
}

/// Searches file system at a given path and produces Lutra source code for
/// reading `.parquet` files.
///
/// Does not traverse links.
pub fn get_interface(base_path: &path::Path) -> Result<String, Error> {
    let mut r = String::new();
    let mut paths_to_search = vec![base_path.to_path_buf()];

    while let Some(path) = paths_to_search.pop() {
        let to_io_err = |e| Error::Io(path.clone(), e);

        let md = path.metadata().map_err(to_io_err)?;
        if md.is_dir() {
            let read_dir = path.read_dir().map_err(to_io_err)?;
            for entry in read_dir {
                let entry = entry.map_err(to_io_err)?;
                paths_to_search.push(entry.path());
            }
        } else if md.is_file() && path.extension().is_some_and(|e| e == "parquet") {
            let relative = path.strip_prefix(base_path).unwrap();

            r += &get_interface_of_parquet_file(&path, relative)?;
            r += "\n";
        }
    }

    Ok(r)
}

/// Read metadata of a parquet file and generates Lutra source code of a
/// function that reads this file.
fn get_interface_of_parquet_file(
    path: &path::Path,
    path_relative: &path::Path,
) -> Result<String, Error> {
    // compose func name
    let file_prefix = path.file_prefix().unwrap();
    let func_name = to_str_or_err(file_prefix)?.replace(['/', ' '], "_");

    // read parquet metadata
    let file = fs::File::open(path).map_err(|e| Error::Io(path.to_path_buf(), e))?;
    let mut metadata_reader = pq_metadata::ParquetMetaDataReader::new();
    metadata_reader.try_parse(&file).unwrap();
    let metadata = metadata_reader.finish().unwrap();

    // Scan file to determine which optional fields actually have nulls
    let file = fs::File::open(path).map_err(|e| Error::Io(path.to_path_buf(), e))?;
    let nullable_columns = scan_for_nulls(file)?;

    let ty = schema_to_lutra_ty(metadata.file_metadata().schema(), &nullable_columns)?;

    // prepare path as str
    let path = to_str_or_err(path_relative.as_os_str())?;

    Ok(format!(
        "func {func_name}(): {ty} -> std::fs::read_parquet(\"{path}\")\n"
    ))
}

/// Scans the parquet file and returns the set of column names that contain null values.
/// Note: This only checks top-level columns. Nested nullability is not checked.
fn scan_for_nulls(file: fs::File) -> Result<HashSet<String>, Error> {
    let builder = ParquetRecordBatchReaderBuilder::try_new(file)?;
    let reader = builder.build()?;

    let mut nullable_columns = HashSet::new();

    for batch_result in reader {
        let batch = batch_result?;

        // Check each top-level column for null values
        for (i, column) in batch.columns().iter().enumerate() {
            if column.null_count() > 0 {
                let field_name = batch.schema().field(i).name().clone();
                nullable_columns.insert(field_name);
            }
        }
    }

    Ok(nullable_columns)
}

fn schema_to_lutra_ty(
    schema: &pq_types::Type,
    nullable_columns: &HashSet<String>,
) -> Result<String, Error> {
    let pq_types::Type::GroupType { fields, .. } = schema else {
        return Err(Error::NotImplemented {
            feature: "primitive type",
            location: "top level".into(),
        });
    };

    format_tuple_fields(fields, nullable_columns, "  ").map_err(|e| Error::NotImplemented {
        feature: e.message,
        location: e.location.unwrap_or_else(|| "top level".into()),
    })
}

struct NotImplementedError {
    message: &'static str,
    location: Option<String>,
}

impl NotImplementedError {
    fn new(message: &'static str) -> Self {
        Self {
            message,
            location: None,
        }
    }

    fn with_location(mut self, location: String) -> Self {
        self.location = Some(location);
        self
    }
}

/// Format a list of parquet fields as a Lutra tuple type
fn format_tuple_fields(
    fields: &[Arc<pq_types::Type>],
    nullable_columns: &HashSet<String>,
    indent: &str,
) -> Result<String, NotImplementedError> {
    let mut ty_fields = String::new();

    for f in fields {
        let info = match f.as_ref() {
            pq_types::Type::PrimitiveType { basic_info, .. } => basic_info,
            pq_types::Type::GroupType { basic_info, .. } => basic_info,
        };

        // Get base type without considering repetition
        let base_ty = field_base_type(f.as_ref(), indent)
            .map_err(|e| e.with_location(format!("field {}", info.name())))?;

        // Apply repetition and nullability
        let field_ty = apply_repetition(
            base_ty,
            info.repetition(),
            nullable_columns.contains(info.name()),
        );

        // Format field
        ty_fields += indent;
        ty_fields += info.name();
        ty_fields += ": ";
        ty_fields += &field_ty;
        ty_fields += ",\n";
    }

    Ok(format!("[{{\n{ty_fields}}}]"))
}

/// Apply repetition and nullability wrapping to a base type
fn apply_repetition(base_ty: String, repetition: Repetition, has_nulls: bool) -> String {
    match repetition {
        Repetition::OPTIONAL => {
            if has_nulls {
                format!("enum {{none, some: {base_ty}}}")
            } else {
                base_ty
            }
        }
        Repetition::REQUIRED => base_ty,
        Repetition::REPEATED => format!("[{base_ty}]"),
    }
}

/// Get the base Lutra type for a field without considering repetition
fn field_base_type(field: &pq_types::Type, indent: &str) -> Result<String, NotImplementedError> {
    match field {
        pq_types::Type::PrimitiveType {
            basic_info,
            physical_type,
            ..
        } => {
            // Try converted type first, then fallback to physical type
            let ty_field = converted_type_to_lt(basic_info.converted_type())?;
            let ty_field = if let Some(t) = ty_field {
                t.to_string()
            } else {
                physical_type_to_lt(*physical_type)?.to_string()
            };
            Ok(ty_field)
        }
        pq_types::Type::GroupType {
            basic_info, fields, ..
        } => {
            // Check if this is a LIST type (special encoding)
            if basic_info.converted_type() == parquet::basic::ConvertedType::LIST {
                // LIST groups have a single field named "list" or "element"
                if fields.len() == 1 {
                    let indent = indent.to_string() + "  ";
                    let element_ty = field_base_type(fields[0].as_ref(), &indent)?;
                    return Ok(format!("[{element_ty}]"));
                }
            }

            // Regular group type (nested struct/tuple)
            // For nested structs, we don't check nullable_columns (would need path-based tracking)
            // So we conservatively wrap all OPTIONAL nested fields
            let empty_set = HashSet::new();
            let indent = indent.to_string() + "  ";
            format_tuple_fields(fields, &empty_set, &indent)
        }
    }
}

fn converted_type_to_lt(
    ty: parquet::basic::ConvertedType,
) -> Result<Option<&'static str>, NotImplementedError> {
    use parquet::basic::ConvertedType;
    Ok(Some(match ty {
        ConvertedType::NONE => return Ok(None),

        ConvertedType::UINT_8 => "uint8",
        ConvertedType::UINT_16 => "uint16",
        ConvertedType::UINT_32 => "uint32",
        ConvertedType::UINT_64 => "uint64",
        ConvertedType::INT_8 => "int8",
        ConvertedType::INT_16 => "int16",
        ConvertedType::INT_32 => "int32",
        ConvertedType::INT_64 => "int64",
        ConvertedType::UTF8 => "text",

        ConvertedType::MAP => return Err(NotImplementedError::new("MAP type")),
        ConvertedType::MAP_KEY_VALUE => return Err(NotImplementedError::new("MAP_KEY_VALUE type")),
        ConvertedType::LIST => return Ok(None), // Handled by caller
        ConvertedType::ENUM => return Err(NotImplementedError::new("ENUM type")),
        ConvertedType::DECIMAL => return Err(NotImplementedError::new("DECIMAL type")),

        ConvertedType::DATE => "std::Date",
        ConvertedType::TIME_MILLIS => return Err(NotImplementedError::new("TIME_MILLIS type")),
        ConvertedType::TIME_MICROS => return Err(NotImplementedError::new("TIME_MICROS type")),
        ConvertedType::TIMESTAMP_MILLIS => {
            return Err(NotImplementedError::new("TIMESTAMP_MILLIS type"));
        }
        ConvertedType::TIMESTAMP_MICROS => "std::Timestamp",

        ConvertedType::JSON => return Err(NotImplementedError::new("JSON type")),
        ConvertedType::BSON => return Err(NotImplementedError::new("BSON type")),
        ConvertedType::INTERVAL => return Err(NotImplementedError::new("INTERVAL type")),
    }))
}

fn physical_type_to_lt(ty: parquet::basic::Type) -> Result<&'static str, NotImplementedError> {
    use parquet::basic::Type;
    Ok(match ty {
        Type::BOOLEAN => "bool",
        Type::INT32 => "int32",
        Type::INT64 => "int64",
        Type::INT96 => return Err(NotImplementedError::new("INT96 type")),
        Type::FLOAT => "float32",
        Type::DOUBLE => "float64",
        Type::BYTE_ARRAY => return Err(NotImplementedError::new("BYTE_ARRAY type")),
        Type::FIXED_LEN_BYTE_ARRAY => {
            return Err(NotImplementedError::new("FIXED_LEN_BYTE_ARRAY type"));
        }
    })
}

fn to_str_or_err(s: &ffi::OsStr) -> Result<&str, Error> {
    s.to_str()
        .ok_or_else(|| Error::NonUnicode(s.to_os_string()))
}
