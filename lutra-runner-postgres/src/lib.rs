//! PostgreSQL Lutra runner

#[cfg(not(any(feature = "postgres", feature = "tokio-postgres")))]
compile_error!("At least one of 'postgres' or 'tokio-postgres' features has to be enabled.");

mod params;
mod result;

#[cfg(feature = "tokio-postgres")]
mod schema;

use lutra_bin::rr;

#[cfg(feature = "postgres")]
pub fn execute(
    client: &mut postgres::Client,
    program: &rr::SqlProgram,
    input: &[u8],
) -> Result<Vec<u8>, postgres::Error> {
    // prepare
    let stmt = client.prepare(&program.sql)?;

    // pack input into query args
    let args = params::to_sql(program, input);

    // execute
    let rows = client.query(&stmt, &args.as_refs())?;

    // convert result from sql
    Ok(result::from_sql(program, &rows))
}

#[cfg(feature = "tokio-postgres")]
pub struct RunnerAsync(pub tokio_postgres::Client);

#[cfg(feature = "tokio-postgres")]
impl lutra_runner::Run for RunnerAsync {
    type Error = tokio_postgres::Error;

    async fn execute_raw(
        &self,
        program: &lutra_bin::rr::Program,
        input: &[u8],
    ) -> Result<std::vec::Vec<u8>, Self::Error> {
        let program = program.as_sql_pg().unwrap();

        // parse
        let stmt = self.0.prepare(&program.sql).await?;

        // pack input into query args
        let args = params::to_sql(program, input);

        let rows = self.0.query(&stmt, &args.as_refs()).await?;

        // convert result from sql
        Ok(result::from_sql(program, &rows))
    }

    async fn get_interface(&self) -> Result<std::string::String, Self::Error> {
        let mut output = String::new();

        let tables = schema::table_list(&self.0).await?;
        for table in tables {
            let table_ty = schema::table_get(&self.0, &table).await?;

            let ty_name = if let Some(n) = table.strip_suffix("s") {
                n.to_string()
            } else {
                format!("{table}_item")
            };

            output += "\n";
            output += &format!("type {ty_name} = {}\n", lutra_bin::ir::print_ty(&table_ty));
            output += &format!("let {table}: func (): [{ty_name}]\n");
        }
        Ok(output)
    }
}
