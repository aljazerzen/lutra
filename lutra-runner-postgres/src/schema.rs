use lutra_bin::ir;
use postgres_types as pg_ty;

#[cfg(feature = "postgres")]
use postgres::Error;
#[cfg(not(feature = "postgres"))]
use tokio_postgres::Error;

#[cfg(feature = "tokio-postgres")]
use crate::RunnerAsync;

#[allow(dead_code)]
mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

#[cfg(feature = "tokio-postgres")]
pub async fn pull_interface<C>(runner: &RunnerAsync<C>) -> Result<String, tokio_postgres::Error>
where
    C: tokio_postgres::GenericClient,
{
    use lutra_runner::Run;

    let program = lutra::pull_interface();
    let mut schemas = runner.run(&program, &()).await.unwrap().unwrap();

    let mut output = String::new();

    // order schemas by name (and put the default schema first)
    const DEFAULT_SCHEMA_NAME: &str = "public";
    let is_default = |x: &lutra::PullInterfaceOutputItems| x.schema_name != DEFAULT_SCHEMA_NAME;
    schemas.sort_by(|a, b| {
        (is_default(a).cmp(&is_default(b))).then(a.schema_name.cmp(&b.schema_name))
    });

    for schema in schemas {
        // TODO: implement this in the Lutra query
        if schema.schema_name.starts_with("pg_toast_") || schema.schema_name.starts_with("pg_temp_")
        {
            continue;
        }

        let mut indent = "";
        if schema.schema_name != DEFAULT_SCHEMA_NAME {
            output += "\n";
            output += &format!("module {} {{\n", schema.schema_name);
            indent = "  ";
        }

        for table in schema.tables {
            let t_name = &table.table_name;
            let table_ty = tuple_from_pg_columns(&table.columns, "")?;

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

            generate_index_lookup(&mut output, indent, table, ty_name, snake);
        }
        if schema.schema_name != DEFAULT_SCHEMA_NAME {
            output += "}\n";
        }
    }
    Ok(output)
}

/// Translates a slice of columns into a Lutra type.
/// Before column name is considered, it must be stripped of `namespace`.
fn tuple_from_pg_columns(
    columns: &[lutra::PullInterfaceOutputItemstablesItemscolumnsItems],
    namespace: &str,
) -> Result<ir::Ty, Error> {
    let mut fields = Vec::new();

    let mut i = 0;
    while i < columns.len() {
        let c = &columns[i];

        if let Some((name, len)) = group_tuple_columns(&columns[i..], namespace) {
            // group columns for a nested tuple

            // recurse
            let strip_prefix = format!("{namespace}{name}.");
            let ty = tuple_from_pg_columns(&columns[i..i + len], &strip_prefix)?;

            fields.push(ir::TyTupleField {
                name: Some(name.to_string()),
                ty,
            });
            i += len;
        } else {
            // no group, add a single field

            let name = c.name.strip_prefix(namespace).unwrap().to_string();

            let ty = ty_from_pg_column(c);

            fields.push(ir::TyTupleField {
                name: Some(name),
                ty,
            });
            i += 1;
        }
    }

    Ok(ir::Ty::new(ir::TyKind::Tuple(fields)))
}

/// Given a slice of columns, finds a consecutive group of columns that share a prefix.
/// For example, in slice `[hello.a, hello.b, world]`, we would find prefix `hello` and group of length 2.
fn group_tuple_columns<'a>(
    columns: &'a [lutra::PullInterfaceOutputItemstablesItemscolumnsItems],
    namespace: &str,
) -> Option<(&'a str, usize)> {
    let c_name = columns[0].name.strip_prefix(namespace).unwrap();
    let (prefix, _) = c_name.split_once('.')?;

    // find group end
    let mut len = 1;
    while len < columns.len() {
        let c_name = columns[len].name.strip_prefix(namespace).unwrap();
        if !c_name.contains('.') {
            break;
        }
        len += 1;
    }
    Some((prefix, len))
}

fn ty_from_pg_column(c: &lutra::PullInterfaceOutputItemstablesItemscolumnsItems) -> ir::Ty {
    let pg_ty = match c.typ_id {
        // special cases
        13226 => pg_ty::Type::INT4,        // cardinal number
        13229 => pg_ty::Type::TEXT,        // character_data
        13231 => pg_ty::Type::TEXT,        // sql_identifier
        13237 => pg_ty::Type::TIMESTAMPTZ, // time_stamp
        13239 => pg_ty::Type::TEXT,        // yes_or_no
        10029 => pg_ty::Type::TEXT,        // pg_statistic

        // general case
        _ => pg_ty::Type::from_oid(c.typ_id as u32)
            .unwrap_or_else(|| panic!("unknown type with oid: {}", c.typ_id)),
    };
    let tn = pg_ty.name();

    match tn {
        "boolean" | "bool" => ir::Ty::new(ir::TyPrimitive::bool),
        "smallint" | "int2" => ir::Ty::new(ir::TyPrimitive::int16),
        "integer" | "int4" | "oid" => ir::Ty::new(ir::TyPrimitive::int32),
        "bigint" | "int8" => ir::Ty::new(ir::TyPrimitive::int64),
        "real" | "float4" => ir::Ty::new(ir::TyPrimitive::float32),
        "double precision" | "float8" => ir::Ty::new(ir::TyPrimitive::float64),

        "date" => ir::Ty::new(ir::Path(vec!["std".into(), "Date".into()])),
        "timestamp" | "timestamp without time zone" => {
            ir::Ty::new(ir::Path(vec!["std".into(), "Timestamp".into()]))
        }
        "time" | "time without time zone" => {
            ir::Ty::new(ir::Path(vec!["std".into(), "Time".into()]))
        }
        // "interval" => None,

        // "bytea" => None,
        // "bit" | "bit varying" | "varbit" => None,
        "text" | "varchar" | "char" | "bpchar" | "name" => ir::Ty::new(ir::TyPrimitive::text),

        // _ if ty.starts_with("bit") => None,
        _ if tn.starts_with("varchar") | tn.starts_with("char") | tn.starts_with("bpchar") => {
            ir::Ty::new(ir::TyPrimitive::text)
        }
        _ if tn.starts_with("decimal") | tn.starts_with("numeric") => {
            ir::Ty::new(ir::Path(vec!["std".into(), "Decimal".into()]))
        }

        // _ => None,
        _ => ir::Ty::new(ir::TyPrimitive::text),
    }
}

fn table_type_name(table_name: &str) -> String {
    if let Some(n) = table_name.strip_suffix("s") {
        crate::case::to_pascal_case(n)
    } else {
        format!("{}Row", crate::case::to_pascal_case(table_name))
    }
}

fn generate_index_lookup(
    output: &mut String,
    indent: &'static str,
    table: lutra::PullInterfaceOutputItemstablesItems,
    ty_name: String,
    snake: String,
) {
    for index in &table.indexes {
        let by = index.columns.join("_and_");

        let mut params = String::new();
        let mut cond = String::new();

        for (i, c) in index.columns.iter().enumerate() {
            if i != 0 {
                params += ", ";
            }
            params += &format!("{c}: int32");

            if i != 0 {
                cond += " && ";
            }
            cond += &format!("x.{c} == {c}");
        }

        *output += &format!(
            "{indent}## Lookup in {} by index {}\n",
            table.table_name, index.index_name
        );
        if index.is_unique {
            *output += &format!(
                "{indent}func from_{snake}_by_{by}({params}): enum {{none, some: {ty_name}}} -> (\n"
            );
            *output += &format!("{indent}  from_{snake}() | std::find(x -> {cond})\n");
            *output += &format!("{indent})\n");
        } else {
            *output += &format!("{indent}func from_{snake}_by_{by}({params}): [{ty_name}] -> (\n");
            *output += &format!("{indent}  from_{snake}() | std::filter(x -> {cond})\n");
            *output += &format!("{indent})\n");
        };
    }
}
