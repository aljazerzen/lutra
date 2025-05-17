#[cfg(not(any(feature = "postgres", feature = "tokio-postgres")))]
compile_error!("At least one of 'postgres' or 'tokio-postgres' features needs to be enabled.");

mod params;
mod result;
mod schema;

pub use schema::{table_get, table_list};

use bytes::Bytes;
use lutra_bin::sr;

#[cfg(feature = "postgres")]
pub fn query_sync(
    client: &mut postgres::Client,
    program: &sr::Program,
    input: &[u8],
) -> Result<Bytes, postgres::Error> {
    // prepare
    let stmt = client.prepare(&program.sql)?;

    // pack input into query args
    let args = params::to_sql(program, input);

    // execute
    let rows = client.query(&stmt, &args.as_refs())?;

    // convert result from sql
    Ok(result::from_sql(program, &rows))
}

#[cfg(feature = "postgres")]
pub struct RunnerSync(pub postgres::Client);

#[cfg(feature = "postgres")]
impl sr::SyncRun for &mut RunnerSync {
    type Error = postgres::Error;

    fn run_binary(
        self,
        program: &lutra_bin::sr::Program,
        input: &[u8],
    ) -> Result<lutra_bin::bytes::Bytes, Self::Error> {
        query_sync(&mut self.0, program, input)
    }
}

#[cfg(feature = "tokio-postgres")]
pub async fn query_async(
    client: &tokio_postgres::Client,
    program: &sr::Program,
    input: &[u8],
) -> Result<Bytes, tokio_postgres::Error> {
    // parse
    let stmt = client.prepare(&program.sql).await?;

    // pack input into query args
    let args = params::to_sql(program, input);

    let rows = client.query(&stmt, &args.as_refs()).await?;

    // convert result from sql
    Ok(result::from_sql(program, &rows))
}

#[cfg(feature = "tokio-postgres")]
pub struct RunnerAsync(pub tokio_postgres::Client);

#[cfg(feature = "tokio-postgres")]
impl sr::AsyncRun for &RunnerAsync {
    type Error = tokio_postgres::Error;

    async fn run_binary(
        self,
        program: &lutra_bin::sr::Program,
        input: &[u8],
    ) -> Result<lutra_bin::bytes::Bytes, Self::Error> {
        query_async(&self.0, program, input).await
    }
}
