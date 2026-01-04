use std::ffi;
use std::fs;
use std::path;

use parquet::file::metadata as pq_metadata;
use parquet::schema::types as pq_types;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{}: {}", .0.display(), .1)]
    Io(std::path::PathBuf, std::io::Error),

    #[error("invalid non-unicode path: {}", .0.display())]
    NonUnicode(ffi::OsString),

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
    let mut reader = pq_metadata::ParquetMetaDataReader::new();
    reader.try_parse(&file).unwrap();
    let metadata = reader.finish().unwrap();

    let ty = schema_to_lutra_ty(metadata.file_metadata().schema())?;

    // prepare path as str
    let path = to_str_or_err(path_relative.as_os_str())?;

    Ok(format!(
        "func {func_name}(): {ty} -> std::fs::read_parquet(\"{path}\")\n"
    ))
}

fn schema_to_lutra_ty(schema: &pq_types::Type) -> Result<String, Error> {
    let pq_types::Type::GroupType { fields, .. } = schema else {
        return Err(Error::NotImplemented {
            feature: "primitive type",
            location: "top level".into(),
        });
    };

    let mut ty_fields = String::new();
    for f in fields {
        // extract basic info
        let info = match f.as_ref() {
            pq_types::Type::PrimitiveType { basic_info, .. } => basic_info,
            pq_types::Type::GroupType { basic_info, .. } => basic_info,
        };
        let not_implemented = |feature| Error::NotImplemented {
            feature,
            location: format!("field {}", info.name()),
        };

        // validate that it's a primitive
        let physical_ty = match f.as_ref() {
            pq_types::Type::PrimitiveType { physical_type, .. } => *physical_type,
            pq_types::Type::GroupType { .. } => {
                return Err(not_implemented("group type"));
            }
        };

        // map Parquet type to a Lutra type
        // TODO: use LogicalType and only fallback to ConvertedType
        let ty_field = converted_type_to_lt(info.converted_type()).map_err(not_implemented)?;
        let ty_field = if let Some(t) = ty_field {
            t
        } else {
            physical_type_to_lt(physical_ty).map_err(not_implemented)?
        };

        // TODO: use f.repetition() and wrap into enum {None, Some=ty_field}

        // compose Lutra tuple type field

        ty_fields += "  ";
        ty_fields += info.name();
        ty_fields += ": ";
        ty_fields += ty_field;
        ty_fields += ",\n";
    }

    Ok(format!("[{}\n{ty_fields}{}]", '{', '}'))
}

fn converted_type_to_lt(
    ty: parquet::basic::ConvertedType,
) -> Result<Option<&'static str>, &'static str> {
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

        ConvertedType::MAP => return Err("MAP type"),
        ConvertedType::MAP_KEY_VALUE => return Err("MAP_KEY_VALUE type"),
        ConvertedType::LIST => return Err("LIST type"),
        ConvertedType::ENUM => return Err("ENUM type"),
        ConvertedType::DECIMAL => return Err("DECIMAL type"),

        ConvertedType::DATE => "std::Date",
        ConvertedType::TIME_MILLIS => return Err("TIME_MILLIS type"),
        ConvertedType::TIME_MICROS => return Err("TIME_MICROS type"),
        ConvertedType::TIMESTAMP_MILLIS => return Err("TIMESTAMP_MILLIS type"),
        ConvertedType::TIMESTAMP_MICROS => "std::Timestamp",

        ConvertedType::JSON => return Err("JSON type"),
        ConvertedType::BSON => return Err("BSON type"),
        ConvertedType::INTERVAL => return Err("INTERVAL type"),
    }))
}

fn physical_type_to_lt(ty: parquet::basic::Type) -> Result<&'static str, &'static str> {
    use parquet::basic::Type;
    Ok(match ty {
        Type::BOOLEAN => "bool",
        Type::INT32 => "int32",
        Type::INT64 => "int64",
        Type::INT96 => return Err("INT96 type"),
        Type::FLOAT => "float32",
        Type::DOUBLE => "float64",
        Type::BYTE_ARRAY => return Err("BYTE_ARRAY type"),
        Type::FIXED_LEN_BYTE_ARRAY => return Err("FIXED_LEN_BYTE_ARRAY type"),
    })
}

fn to_str_or_err(s: &ffi::OsStr) -> Result<&str, Error> {
    s.to_str()
        .ok_or_else(|| Error::NonUnicode(s.to_os_string()))
}
