//! Schema introspection for DuckDB
//!
//! Generates Lutra type definitions and accessor functions from DuckDB database schema.

use lutra_bin::ir;

use crate::{Error, Runner};

#[allow(dead_code)]
mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

/// Pull database schema and generate Lutra source code
pub async fn pull_interface(runner: &Runner) -> Result<String, Error> {
    use lutra_runner::Run;

    let program = lutra::pull_interface();
    let mut schemas = runner.run(&program, &()).await?.unwrap();

    let mut output = String::new();

    // Order schemas by name (put the default schema first)
    const DEFAULT_SCHEMA_NAME: &str = "main";
    let is_default = |x: &lutra::PullInterfaceOutputItems| x.schema_name != DEFAULT_SCHEMA_NAME;
    schemas.sort_by(|a, b| {
        (is_default(a).cmp(&is_default(b))).then(a.schema_name.cmp(&b.schema_name))
    });

    for schema in schemas {
        let mut indent = "";
        if schema.schema_name != DEFAULT_SCHEMA_NAME {
            output += "\n";
            output += &format!("module {} {{\n", schema.schema_name);
            indent = "  ";
        }

        for table in schema.tables {
            let t_name = &table.table_name;
            let table_ty = tuple_from_columns(&table.columns);

            let ty_name = table_type_name(t_name);
            let snake = crate::case::to_snake_case(t_name);

            output += "\n";
            output += &format!("{indent}## Row of table {t_name}\n");
            output += &format!(
                "{indent}type {ty_name}: {}\n",
                lutra_bin::ir::print_ty(&table_ty)
            );
            output += &format!("{indent}## Read from table {t_name}\n");
            output += &format!(
                "{indent}func from_{snake}(): [{ty_name}] -> std::sql::from(\"{t_name}\")\n",
            );
            output += &format!("{indent}## Write into table {t_name}\n");
            output += &format!(
                "{indent}func insert_{snake}(values: [{ty_name}]) -> std::sql::insert(values, \"{t_name}\")\n"
            );

            // Collect all lookups (constraints first, then indexes)
            let mut lookups = Vec::new();
            lookups.extend(collect_constraint_lookups(&table));
            lookups.extend(collect_index_lookups(&table));

            // Generate lookup functions
            for lookup in lookups {
                generate_lookup_function(&mut output, indent, &table, &ty_name, &snake, &lookup);
            }
        }

        if schema.schema_name != DEFAULT_SCHEMA_NAME {
            output += "}\n";
        }
    }

    Ok(output)
}

/// Translates columns into a Lutra tuple type
fn tuple_from_columns(
    columns: &[lutra::PullInterfaceOutputItemstablesItemscolumnsItems],
) -> ir::Ty {
    let fields = columns
        .iter()
        .map(|c| ir::TyTupleField {
            name: Some(c.name.clone()),
            ty: ty_from_duckdb_type(&c.data_type, c.is_nullable),
        })
        .collect();

    ir::Ty::new(ir::TyKind::Tuple(fields))
}

/// Convert DuckDB type string to Lutra type
fn ty_from_duckdb_type(data_type: &str, is_nullable: bool) -> ir::Ty {
    // Check for STRUCT types (e.g., "STRUCT(name VARCHAR, age INTEGER)")
    if data_type.starts_with("STRUCT(") && data_type.ends_with(")") {
        let struct_ty = parse_struct_type(data_type);
        return if is_nullable {
            wrap_in_option(struct_ty)
        } else {
            struct_ty
        };
    }

    // Check for array types (e.g., "INTEGER[]", "VARCHAR[][]")
    if let Some(item_type_str) = data_type.strip_suffix("[]") {
        let item_ty = ty_from_duckdb_type(item_type_str, false);
        let array_ty = ir::Ty::new(ir::TyKind::Array(Box::new(item_ty)));
        return if is_nullable {
            wrap_in_option(array_ty)
        } else {
            array_ty
        };
    }

    let base_ty = match data_type.to_uppercase().as_str() {
        // Boolean
        "BOOLEAN" | "BOOL" => ir::Ty::new(ir::TyPrimitive::bool),

        // Signed integers
        "TINYINT" | "INT1" => ir::Ty::new(ir::TyPrimitive::int8),
        "SMALLINT" | "INT2" => ir::Ty::new(ir::TyPrimitive::int16),
        "INTEGER" | "INT4" | "INT" => ir::Ty::new(ir::TyPrimitive::int32),
        "BIGINT" | "INT8" => ir::Ty::new(ir::TyPrimitive::int64),
        "HUGEINT" | "INT128" => ir::Ty::new(ir::TyPrimitive::int64), // fallback, no int128 in Lutra

        // Unsigned integers
        "UTINYINT" | "UINT8" => ir::Ty::new(ir::TyPrimitive::uint8),
        "USMALLINT" | "UINT16" => ir::Ty::new(ir::TyPrimitive::uint16),
        "UINTEGER" | "UINT32" => ir::Ty::new(ir::TyPrimitive::uint32),
        "UBIGINT" | "UINT64" => ir::Ty::new(ir::TyPrimitive::uint64),
        "UHUGEINT" | "UINT128" => ir::Ty::new(ir::TyPrimitive::uint64), // fallback

        // Floating point
        "FLOAT" | "FLOAT4" | "REAL" => ir::Ty::new(ir::TyPrimitive::float32),
        "DOUBLE" | "FLOAT8" => ir::Ty::new(ir::TyPrimitive::float64),

        // Text types
        "VARCHAR" | "TEXT" | "STRING" | "CHAR" | "BPCHAR" | "NAME" => {
            ir::Ty::new(ir::TyPrimitive::text)
        }

        // Date/time types
        "DATE" => ir::Ty::new(ir::Path(vec!["std".into(), "Date".into()])),
        "TIMESTAMP" | "TIMESTAMP WITHOUT TIME ZONE" | "DATETIME" => {
            ir::Ty::new(ir::Path(vec!["std".into(), "Timestamp".into()]))
        }
        "TIME" | "TIME WITHOUT TIME ZONE" => {
            ir::Ty::new(ir::Path(vec!["std".into(), "Time".into()]))
        }

        // Decimal
        s if s.starts_with("DECIMAL") || s.starts_with("NUMERIC") => {
            ir::Ty::new(ir::Path(vec!["std".into(), "Decimal".into()]))
        }

        // VARCHAR with length
        s if s.starts_with("VARCHAR") || s.starts_with("CHAR") || s.starts_with("BPCHAR") => {
            ir::Ty::new(ir::TyPrimitive::text)
        }

        // Unknown types fall back to text
        _ => ir::Ty::new(ir::TyPrimitive::text),
    };

    if is_nullable {
        wrap_in_option(base_ty)
    } else {
        base_ty
    }
}

/// Wrap a type in option enum: enum {none, some: T}
fn wrap_in_option(ty: ir::Ty) -> ir::Ty {
    let variants = vec![
        ir::TyEnumVariant {
            name: "none".into(),
            ty: ir::Ty::new_unit(),
        },
        ir::TyEnumVariant {
            name: "some".into(),
            ty,
        },
    ];
    ir::Ty::new(ir::TyKind::Enum(variants))
}

/// Parse STRUCT type string into Lutra tuple type
/// Example: "STRUCT(name VARCHAR, age INTEGER)" -> {name: text, age: int32}
fn parse_struct_type(data_type: &str) -> ir::Ty {
    // Remove "STRUCT(" prefix and ")" suffix
    let inner = &data_type[7..data_type.len() - 1];

    let fields = parse_struct_fields(inner);
    ir::Ty::new(ir::TyKind::Tuple(fields))
}

/// Parse comma-separated struct fields, handling nested STRUCT and quoted names
fn parse_struct_fields(input: &str) -> Vec<ir::TyTupleField> {
    let mut fields = Vec::new();
    let mut current_field = String::new();
    let mut paren_depth = 0;
    let mut in_quotes = false;

    for ch in input.chars() {
        match ch {
            '"' => {
                in_quotes = !in_quotes;
                current_field.push(ch);
            }
            '(' if !in_quotes => {
                paren_depth += 1;
                current_field.push(ch);
            }
            ')' if !in_quotes => {
                paren_depth -= 1;
                current_field.push(ch);
            }
            ',' if !in_quotes && paren_depth == 0 => {
                // End of field
                if let Some(field) = parse_single_struct_field(current_field.trim()) {
                    fields.push(field);
                }
                current_field.clear();
            }
            _ => {
                current_field.push(ch);
            }
        }
    }

    // Last field
    if !current_field.is_empty()
        && let Some(field) = parse_single_struct_field(current_field.trim())
    {
        fields.push(field);
    }

    fields
}

/// Parse a single field: "name VARCHAR" or '"quoted name" INTEGER'
fn parse_single_struct_field(field_str: &str) -> Option<ir::TyTupleField> {
    // Find the last space that's not inside parentheses (for nested STRUCT)
    let mut paren_depth = 0;
    let mut last_space_idx = None;

    for (i, ch) in field_str.char_indices() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth -= 1,
            ' ' if paren_depth == 0 => last_space_idx = Some(i),
            _ => {}
        }
    }

    let space_idx = last_space_idx?;
    let name_part = field_str[..space_idx].trim();
    let type_part = field_str[space_idx + 1..].trim();

    // Remove quotes from field name if present
    let field_name = if name_part.starts_with('"') && name_part.ends_with('"') {
        &name_part[1..name_part.len() - 1]
    } else {
        name_part
    };

    Some(ir::TyTupleField {
        name: Some(field_name.to_string()),
        ty: ty_from_duckdb_type(type_part, false),
    })
}

/// Generate type name from table name
fn table_type_name(table_name: &str) -> String {
    if let Some(n) = table_name.strip_suffix("s") {
        crate::case::to_pascal_case(n)
    } else {
        format!("{}Row", crate::case::to_pascal_case(table_name))
    }
}

/// Metadata for generating a lookup function
struct Lookup {
    /// Name to use in comment (e.g., "idx_name" or "users_id_pkey")
    name: String,
    /// Type of lookup ("index" or "constraint")
    kind: &'static str,
    /// Column names in the lookup
    columns: Vec<String>,
    /// Whether this is a unique lookup (PRIMARY KEY, UNIQUE index, etc.)
    is_unique: bool,
}

/// Helper to look up column type from table schema
fn get_column_type(table: &lutra::PullInterfaceOutputItemstablesItems, col_name: &str) -> ir::Ty {
    table
        .columns
        .iter()
        .find(|c| c.name == col_name)
        .map(|c| ty_from_duckdb_type(&c.data_type, false))
        .unwrap_or_else(|| ir::Ty::new(ir::TyPrimitive::text)) // fallback
}

/// Unified function to generate a lookup function from Lookup metadata
fn generate_lookup_function(
    output: &mut String,
    indent: &str,
    table: &lutra::PullInterfaceOutputItemstablesItems,
    ty_name: &str,
    snake: &str,
    lookup: &Lookup,
) {
    let by = lookup.columns.join("_and_");

    // Build parameters and condition using iterators
    let params: Vec<String> = lookup
        .columns
        .iter()
        .map(|col_name| {
            let col_ty = get_column_type(table, col_name);
            format!("{}: {}", col_name, ir::print_ty(&col_ty))
        })
        .collect();
    let params = params.join(", ");

    let conditions: Vec<String> = lookup
        .columns
        .iter()
        .map(|col_name| format!("x.{} == {}", col_name, col_name))
        .collect();
    let cond = conditions.join(" && ");

    // Generate function
    *output += &format!(
        "{indent}## Lookup in {} by {} {}\n",
        table.table_name, lookup.kind, lookup.name
    );

    if lookup.is_unique {
        // Unique lookup: return option
        *output += &format!(
            "{indent}func from_{snake}_by_{by}({params}): enum {{none, some: {ty_name}}} -> (\n"
        );
        *output += &format!("{indent}  from_{snake}() | std::find(x -> {cond})\n");
    } else {
        // Non-unique lookup: return array
        *output += &format!("{indent}func from_{snake}_by_{by}({params}): [{ty_name}] -> (\n");
        *output += &format!("{indent}  from_{snake}() | std::filter(x -> {cond})\n");
    }
    *output += &format!("{indent})\n");
}

/// Collect constraint lookups (PRIMARY KEY, UNIQUE)
fn collect_constraint_lookups(table: &lutra::PullInterfaceOutputItemstablesItems) -> Vec<Lookup> {
    table
        .constraints
        .iter()
        .filter(|c| !c.columns.is_empty())
        .map(|constraint| Lookup {
            name: constraint.constraint_name.clone(),
            kind: "constraint",
            columns: constraint.columns.clone(),
            is_unique: constraint.constraint_type == "PRIMARY KEY"
                || constraint.constraint_type == "UNIQUE",
        })
        .collect()
}

/// Collect index lookups
fn collect_index_lookups(table: &lutra::PullInterfaceOutputItemstablesItems) -> Vec<Lookup> {
    table
        .indexes
        .iter()
        .filter_map(|index| {
            // Parse column names from CREATE INDEX SQL
            let columns = parse_index_columns(&index.sql)?;

            if columns.is_empty() {
                return None;
            }

            Some(Lookup {
                name: index.index_name.clone(),
                kind: "index",
                columns,
                is_unique: index.is_unique,
            })
        })
        .collect()
}

/// Parse column names from CREATE INDEX SQL statement
/// Example: "CREATE INDEX idx_name ON users(name);" -> Some(vec!["name"])
/// Example: "CREATE INDEX idx ON users(last_name, first_name);" -> Some(vec!["last_name", "first_name"])
fn parse_index_columns(sql: &str) -> Option<Vec<String>> {
    // Find "ON table_name(" in the SQL
    let on_pos = sql.find(" ON ")?;
    let paren_start = sql[on_pos..].find('(')?;
    let paren_end = sql[on_pos + paren_start..].find(')')?;

    // Extract the column list between parentheses
    let start = on_pos + paren_start + 1;
    let end = on_pos + paren_start + paren_end;
    let columns_str = &sql[start..end];

    // Split by comma and clean up each column name
    let columns: Vec<String> = columns_str
        .split(',')
        .map(|col| {
            let col = col.trim();
            // Remove quotes if present
            if (col.starts_with('"') && col.ends_with('"'))
                || (col.starts_with('\'') && col.ends_with('\''))
            {
                col[1..col.len() - 1].to_string()
            } else {
                col.to_string()
            }
        })
        .collect();

    Some(columns)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ty_from_duckdb_type() {
        // Test primitives
        assert!(matches!(
            ty_from_duckdb_type("INTEGER", false).kind,
            ir::TyKind::Primitive(ir::TyPrimitive::int32)
        ));
        assert!(matches!(
            ty_from_duckdb_type("BIGINT", false).kind,
            ir::TyKind::Primitive(ir::TyPrimitive::int64)
        ));
        assert!(matches!(
            ty_from_duckdb_type("VARCHAR", false).kind,
            ir::TyKind::Primitive(ir::TyPrimitive::text)
        ));
        assert!(matches!(
            ty_from_duckdb_type("BOOLEAN", false).kind,
            ir::TyKind::Primitive(ir::TyPrimitive::bool)
        ));

        // Test nullable wraps in option
        let nullable_int = ty_from_duckdb_type("INTEGER", true);
        assert!(matches!(nullable_int.kind, ir::TyKind::Enum(_)));
    }

    #[test]
    fn test_table_type_name() {
        assert_eq!(table_type_name("users"), "User");
        assert_eq!(table_type_name("movies"), "Movie");
        assert_eq!(table_type_name("person"), "PersonRow");
        assert_eq!(table_type_name("data"), "DataRow");
    }

    #[test]
    fn test_parse_index_columns() {
        // Single column with quotes
        assert_eq!(
            parse_index_columns(r#"CREATE INDEX idx_name ON users("name");"#),
            Some(vec!["name".to_string()])
        );

        // Single column without quotes
        assert_eq!(
            parse_index_columns("CREATE INDEX idx_email ON users(email);"),
            Some(vec!["email".to_string()])
        );

        // Multiple columns with quotes
        assert_eq!(
            parse_index_columns(r#"CREATE INDEX idx ON users("last_name", "first_name");"#),
            Some(vec!["last_name".to_string(), "first_name".to_string()])
        );

        // Multiple columns without quotes
        assert_eq!(
            parse_index_columns("CREATE INDEX idx ON users(last_name, first_name);"),
            Some(vec!["last_name".to_string(), "first_name".to_string()])
        );

        // UNIQUE index
        assert_eq!(
            parse_index_columns(r#"CREATE UNIQUE INDEX users_email_key ON users(email);"#),
            Some(vec!["email".to_string()])
        );

        // With schema prefix
        assert_eq!(
            parse_index_columns(r#"CREATE INDEX idx ON main.users(name);"#),
            Some(vec!["name".to_string()])
        );
    }
}
