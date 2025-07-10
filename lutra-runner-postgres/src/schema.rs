use lutra_bin::ir;
use postgres_types as pg_ty;

pub fn table_list(client: &mut postgres::Client) -> Result<Vec<String>, postgres::Error> {
    let query = "
        SELECT relname
        FROM pg_class
        JOIN pg_namespace ON (relnamespace = pg_namespace.oid)
        WHERE nspname = current_schema AND relkind = 'r'
    ";
    let rows = client.query(query, &[])?;

    Ok(rows.into_iter().map(|r| r.get(0)).collect())
}

pub fn table_get(
    client: &mut postgres::Client,
    table_name: &str,
) -> Result<ir::Ty, postgres::Error> {
    let query = "
        SELECT attname, atttypid, attnotnull
        FROM pg_attribute
        JOIN pg_class ON (attrelid = pg_class.oid)
        JOIN pg_namespace ON (relnamespace = pg_namespace.oid)
        WHERE nspname = current_schema AND relname = $1 AND attnum > 0 AND atttypid > 0
        ORDER BY attnum;
    ";
    let rows = client.query(query, &[&table_name.to_string()])?;

    let fields: Vec<_> = rows
        .into_iter()
        .map(|row| -> _ {
            let name: String = row.get(0);
            let typid: u32 = row.get(1);
            let not_null: bool = row.get(2);

            let ty = pg_ty::Type::from_oid(typid).unwrap_or_else(|| panic!("custom types"));

            pg_column_to_ty(name, &ty, !not_null)
        })
        .collect();

    Ok(ir::Ty::new(ir::TyKind::Tuple(fields)))
}

fn pg_column_to_ty(name: String, db_ty: &pg_ty::Type, _nullable: bool) -> ir::TyTupleField {
    // TODO: nullable

    let ty = from_db_type(db_ty.name()).unwrap();

    ir::TyTupleField {
        name: Some(name),
        ty,
    }
}

fn from_db_type(ty: &str) -> Option<ir::Ty> {
    let prim = match ty {
        "boolean" | "bool" => ir::TyPrimitive::bool,
        "smallint" | "int2" => ir::TyPrimitive::int16,
        "integer" | "int4" => ir::TyPrimitive::int32,
        "bigint" | "int8" => ir::TyPrimitive::int64,
        "real" | "float4" => ir::TyPrimitive::float32,
        "double precision" | "float8" => ir::TyPrimitive::float64,

        "timestamp" | "timestamp without time zone" => return None,

        "timestamptz" | "timestamp with time zone" => return None,
        "date" => return None,
        "time" | "time without time zone" => return None,
        "interval" => return None,

        "bytea" => return None,
        "bit" | "bit varying" | "varbit" => return None,

        "text" | "varchar" | "char" | "bpchar" => ir::TyPrimitive::text,

        _ if ty.starts_with("bit") => return None,
        _ if ty.starts_with("varchar") | ty.starts_with("char") | ty.starts_with("bpchar") => {
            ir::TyPrimitive::text
        }
        _ if ty.starts_with("decimal") | ty.starts_with("numeric") => return None,

        _ => return None,
    };
    Some(ir::Ty::new(prim))
}

/*

/// Request over PG protocol. Executable either sync or async.
trait Request {
    type Output: Sized;

    fn compose_query(&self) -> &'static str;

    fn convert_result(&self, rows: Vec<tokio_postgres::Row>) -> Self::Output;
}

async fn execute_async<R: Request>(
    request: &R,
    client: &tokio_postgres::Client,
) -> Result<R::Output, tokio_postgres::Error> {
    let query = request.compose_query();
    let result = client.query(query, &[]).await?;
    Ok(request.convert_result(result))
}

fn execute_sync<R: Request>(
    request: &R,
    client: &mut postgres::Client,
) -> Result<R::Output, postgres::Error> {
    let query = request.compose_query();
    let result = client.query(query, &[])?;
    Ok(request.convert_result(result))
}

macro_rules! facade_async {
    ($func: ident, $request_impl: ident) => {
        pub async fn $func(
            client: &tokio_postgres::Client,
        ) -> Result<Vec<String>, tokio_postgres::Error> {
            execute_async(&$request_impl, client).await
        }
    };
}
macro_rules! facade_sync {
    ($func: ident, $request_impl: ident) => {
        pub async fn $func(client: &mut postgres::Client) -> Result<Vec<String>, postgres::Error> {
            execute_sync(&$request_impl, client)
        }
    };
}

facade_async!(table_list_async, TableList);
facade_sync!(table_list_sync, TableList);

struct TableList;

impl Request for TableList {
    type Output = Vec<String>;

    fn compose_query(&self) -> &'static str {
        "SELECT relname
        FROM pg_class
        JOIN pg_namespace ON (relnamespace = pg_namespace.oid)
        WHERE nspname = current_schema AND relkind = 'r'"
    }

    fn convert_result(&self, rows: Vec<tokio_postgres::Row>) -> Self::Output {
        rows.into_iter().map(|r| r.get::<_, String>(0)).collect()
    }
}
*/
