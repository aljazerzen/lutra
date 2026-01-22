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
            let table_ty = tuple_ty_from_introspection(&table.columns, "")?;

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

fn tuple_ty_from_introspection(
    columns: &[lutra::PullInterfaceOutputItemstablesItemscolumnsItems],
    strip_prefix: &str,
) -> Result<ir::Ty, Error> {
    let mut fields = Vec::new();

    let mut i = 0;
    while i < columns.len() {
        let c = &columns[i];
        let c_name = c.name.strip_prefix(strip_prefix).unwrap();

        if let Some((prefix, _)) = c_name.split_once('.') {
            // group columns for a nested tuple

            // find group end
            let mut end = i + 1;
            while end < columns.len() {
                if !columns[end].name.contains('.') {
                    break;
                }
                end += 1;
            }

            let strip_prefix = format!("{strip_prefix}{prefix}.");
            fields.push(ir::TyTupleField {
                name: Some(prefix.to_string()),
                ty: tuple_ty_from_introspection(&columns[i..end], &strip_prefix)?,
            });
            i = end;
        } else {
            fields.push(tuple_field_from_introspection(c, strip_prefix));
            i += 1;
        }
    }

    Ok(ir::Ty::new(ir::TyKind::Tuple(fields)))
}

fn tuple_field_from_introspection(
    c: &lutra::PullInterfaceOutputItemstablesItemscolumnsItems,
    strip_prefix: &str,
) -> ir::TyTupleField {
    let ty = match c.typ_id {
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
    let ty = col_ty_from_introspection(ty.name())
        .unwrap_or_else(|| panic!("cannot translate SQL type {ty} to a Lutra type"));

    // TODO: arrays
    // if is_array {
    // ty = ir::Ty::new(ir::TyKind::Array(Box::new(ty)));
    // }

    // TODO: nullable

    let name = Some(c.name.strip_prefix(strip_prefix).unwrap().to_string());

    ir::TyTupleField { name, ty }
}

fn col_ty_from_introspection(ty: &str) -> Option<ir::Ty> {
    let prim = match ty {
        "boolean" | "bool" => ir::TyPrimitive::bool,
        "smallint" | "int2" => ir::TyPrimitive::int16,
        "integer" | "int4" | "oid" => ir::TyPrimitive::int32,
        "bigint" | "int8" => ir::TyPrimitive::int64,
        "real" | "float4" => ir::TyPrimitive::float32,
        "double precision" | "float8" => ir::TyPrimitive::float64,

        "date" => return Some(ir::Ty::new(ir::Path(vec!["std".into(), "Date".into()]))),
        "timestamp" | "timestamp without time zone" => {
            return Some(ir::Ty::new(ir::Path(vec![
                "std".into(),
                "Timestamp".into(),
            ])));
        }
        "time" | "time without time zone" => {
            return Some(ir::Ty::new(ir::Path(vec!["std".into(), "Time".into()])));
        }
        // "interval" => return None,

        // "bytea" => return None,
        // "bit" | "bit varying" | "varbit" => return None,
        "text" | "varchar" | "char" | "bpchar" | "name" => ir::TyPrimitive::text,

        _ if ty.starts_with("bit") => return None,
        _ if ty.starts_with("varchar") | ty.starts_with("char") | ty.starts_with("bpchar") => {
            ir::TyPrimitive::text
        }
        _ if ty.starts_with("decimal") | ty.starts_with("numeric") => {
            return Some(ir::Ty::new(ir::Path(vec!["std".into(), "Decimal".into()])));
        }

        // _ => return None,
        _ => ir::TyPrimitive::text,
    };
    Some(ir::Ty::new(prim))
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
