//! DuckDB Lutra runner

mod params;

pub use lutra_runner::Run;

use lutra_bin::{ir, rr};
use std::{collections::HashMap, sync::Arc};
use thiserror::Error;

/// DuckDB runner for executing Lutra programs
///
/// Uses async-duckdb which provides a proper async wrapper around DuckDB.
/// The Client handles connection pooling and threading internally.
pub struct Runner {
    client: async_duckdb::Client,
}

impl Runner {
    pub fn new(client: async_duckdb::Client) -> Self {
        Self { client }
    }

    /// Open a file-based DuckDB database
    pub async fn open(path: &str) -> Result<Self, Error> {
        let client = async_duckdb::ClientBuilder::new().path(path).open().await?;
        Ok(Self::new(client))
    }

    /// Create an in-memory DuckDB database
    pub async fn in_memory() -> Result<Self, Error> {
        let client = async_duckdb::ClientBuilder::new().open().await?;
        Ok(Self::new(client))
    }
}

pub struct PreparedProgram {
    program: Arc<rr::SqlProgram>,
}

impl lutra_runner::Run for Runner {
    type Error = Error;
    type Prepared = PreparedProgram;

    async fn prepare(&self, program: rr::Program) -> Result<Self::Prepared, Self::Error> {
        // Accept both SqlDuckDB (preferred) and SqlPg (backward compatibility)
        let program = program
            .into_sql_duck_db()
            .or_else(|p| p.into_sql_postgres())
            .map_err(|_| Error::UnsupportedFormat)?;

        // Don't prepare a statement, because we cannot cache it for later anyway.
        // That's because statement borrows connection, which means we cannot use it for other queries.

        Ok(PreparedProgram {
            program: Arc::from(program),
        })
    }

    async fn execute(&self, handle: &Self::Prepared, input: &[u8]) -> Result<Vec<u8>, Self::Error> {
        let program = handle.program.clone();

        // I hate this clone
        let input = input.to_vec();

        self.client
            .conn(move |conn| {
                let ctx = Context::new(&program.defs);

                // Convert input to SQL params
                let args = match params::to_sql(&input, &program.input_ty, &ctx) {
                    Ok(a) => a,
                    Err(e) => return Ok(Err(e)),
                };

                // Execute query and get Arrow RecordBatches
                let mut stmt = conn.prepare(&program.sql)?;
                let arrow = stmt.query_arrow(args.as_params())?;
                let batches: Vec<_> = arrow.collect();

                // Convert Arrow to Lutra format
                let output =
                    match lutra_arrow::arrow_to_lutra(batches, &program.output_ty, &program.defs) {
                        Ok(o) => o,
                        Err(e) => return Ok(Err(Error::ArrowConversion(e.to_string()))),
                    };

                Ok(Ok(output.to_vec()))
            })
            .await?
    }

    async fn get_interface(&self) -> Result<String, Self::Error> {
        // TODO
        Ok(String::new())
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("bad result: {}", .0)]
    BadDatabaseResponse(&'static str),
    #[error("duckdb: {}", .0)]
    DuckDB(#[from] async_duckdb::Error),
    #[error("unsupported program format")]
    UnsupportedFormat,
    #[error("unsupported data type: {}", .0)]
    UnsupportedDataType(&'static str),
    #[error("arrow conversion: {}", .0)]
    ArrowConversion(String),
}

impl Error {
    #[allow(dead_code)]
    pub(crate) fn from_duck(e: async_duckdb::duckdb::Error) -> Self {
        Self::DuckDB(async_duckdb::Error::Duckdb(e))
    }
}

pub(crate) struct Context<'a> {
    pub types: HashMap<&'a ir::Path, &'a ir::Ty>,
}

impl<'a> Context<'a> {
    pub fn new(ty_defs: &'a [ir::TyDef]) -> Self {
        Context {
            types: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
        }
    }

    pub fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_in_memory() {
        let _runner = Runner::in_memory().await.unwrap();
        // Basic smoke test
    }

    #[tokio::test]
    async fn test_file_based() {
        let temp = std::env::temp_dir().join("test_lutra_duckdb.duckdb");
        let _runner = Runner::open(temp.to_str().unwrap()).await.unwrap();
        // Test persistence
        std::fs::remove_file(temp).ok();
    }
}
