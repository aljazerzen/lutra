//! PostgreSQL Lutra runner

#[cfg(not(any(feature = "postgres", feature = "tokio-postgres")))]
compile_error!("At least one of 'postgres' or 'tokio-postgres' features has to be enabled.");

mod params;
mod result;

#[cfg(feature = "tokio-postgres")]
mod schema;

#[cfg(feature = "postgres")]
use postgres::Error as PgError;
use thiserror::Error;
#[cfg(not(feature = "postgres"))]
use tokio_postgres::Error as PgError;

use lutra_bin::rr;

#[derive(Error, Debug)]
pub enum Error {
    #[error("bad result: {}", .0)]
    BadDatabaseResponse(&'static str),
    #[error("postgres: {}", .0)]
    Postgres(#[from] PgError),
}

#[cfg(feature = "postgres")]
pub fn execute(
    client: &mut postgres::Client,
    program: &rr::SqlProgram,
    input: &[u8],
) -> Result<Vec<u8>, Error> {
    // prepare
    let stmt = client.prepare(&program.sql)?;

    // pack input into query args
    let args = params::to_sql(program, input);

    // execute
    let rows = client.query(&stmt, &args.as_refs())?;

    // convert result from sql
    result::from_sql(program, &rows)
}

#[cfg(feature = "tokio-postgres")]
pub struct RunnerAsync {
    client: tokio_postgres::Client,
}

impl RunnerAsync {
    pub fn new(client: tokio_postgres::Client) -> Self {
        RunnerAsync { client }
    }

    /// Helper for [tokio_postgres::connect] and [RunnerAsync::new].
    pub async fn connect_no_tls(config: &str) -> Result<Self, Error> {
        let (client, conn) = tokio_postgres::connect(config, tokio_postgres::NoTls).await?;
        tokio::task::spawn(async {
            if let Err(e) = conn.await {
                eprintln!("{e}");
            }
        });

        Ok(Self::new(client))
    }
}

pub struct PreparedProgram {
    program: rr::SqlProgram,
    stmt: tokio_postgres::Statement,
}

#[cfg(feature = "tokio-postgres")]
impl lutra_runner::Run for RunnerAsync {
    type Error = Error;
    type Prepared = PreparedProgram;

    async fn prepare(&self, program: rr::Program) -> Result<Self::Prepared, Self::Error> {
        let program = *program.into_sql_pg().unwrap();

        let stmt = self.client.prepare(&program.sql).await?;

        Ok(PreparedProgram { program, stmt })
    }

    async fn execute(
        &self,
        handle: &Self::Prepared,
        input: &[u8],
    ) -> Result<std::vec::Vec<u8>, Self::Error> {
        // pack input into query args
        let args = params::to_sql(&handle.program, input);

        let rows = self.client.query(&handle.stmt, &args.as_refs()).await?;

        // convert result from sql
        result::from_sql(&handle.program, &rows)
    }

    async fn get_interface(&self) -> Result<std::string::String, Self::Error> {
        let mut output = String::new();

        let tables = schema::table_list(&self.client).await?;
        for table in tables {
            let table_ty = schema::table_get(&self.client, &table).await?;

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
