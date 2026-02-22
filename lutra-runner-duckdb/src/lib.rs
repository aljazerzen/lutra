//! DuckDB Lutra runner

mod case;
mod params;
mod schema;

pub use lutra_runner::{Run, RunSync};

use lutra_bin::{ir, rr};
use std::{collections::HashMap, path, sync::Arc};
use thiserror::Error;

/// DuckDB runner for executing Lutra programs
///
/// Uses the synchronous duckdb crate. DuckDB operations are CPU-bound
/// and don't involve actual async I/O, making this suitable for RunSync.
pub struct Runner {
    conn: duckdb::Connection,
}

impl Runner {
    pub fn new(
        conn: duckdb::Connection,
        file_system: Option<path::PathBuf>,
    ) -> Result<Self, Error> {
        if let Some(fs_path) = file_system {
            let set_fs_access = format!("SET file_search_path = '{}'", fs_path.display());
            conn.execute(&set_fs_access, [])?;
        }
        Ok(Self { conn })
    }

    /// Open a file-based DuckDB database
    pub fn open(path: &str, file_system: Option<path::PathBuf>) -> Result<Self, Error> {
        let conn = duckdb::Connection::open(path)?;
        Self::new(conn, file_system)
    }

    /// Create an in-memory DuckDB database
    pub fn in_memory(file_system: Option<path::PathBuf>) -> Result<Self, Error> {
        let conn = duckdb::Connection::open_in_memory()?;
        Self::new(conn, file_system)
    }
}

#[derive(Clone)]
pub struct PreparedProgram {
    program: Arc<rr::SqlProgram>,
}

impl lutra_runner::RunSync for Runner {
    type Error = Error;
    type Prepared = PreparedProgram;

    fn prepare_sync(&mut self, program: rr::Program) -> Result<Self::Prepared, Self::Error> {
        // Accept both SqlDuckDB (preferred) and SqlPg (backward compatibility)
        let program = program
            .into_sql_duck_db()
            .map_err(|_| Error::UnsupportedFormat)?;

        // Don't prepare a statement, because we cannot cache it for later anyway.
        // That's because statement borrows connection, which means we cannot use it for other queries.

        Ok(PreparedProgram {
            program: Arc::from(program),
        })
    }

    fn execute_sync(
        &mut self,
        handle: &Self::Prepared,
        input: &[u8],
    ) -> Result<Vec<u8>, Self::Error> {
        let ctx = Context::new(&handle.program.defs);

        // Convert input to SQL params
        let args = params::to_sql(input, &handle.program.input_ty, &ctx)?;

        // Execute query and get Arrow RecordBatches
        let mut stmt = self.conn.prepare(&handle.program.sql)?;
        let arrow = stmt.query_arrow(args.as_params())?;
        let batches: Vec<_> = arrow.collect();

        // Convert Arrow to Lutra format
        let output =
            lutra_arrow::arrow_to_lutra(batches, &handle.program.output_ty, &handle.program.defs)
                .map_err(|e| Error::ArrowConversion(e.to_string()))?;

        Ok(output.to_vec())
    }

    fn get_interface_sync(&mut self) -> Result<String, Self::Error> {
        schema::pull_interface(self)
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("bad result: {}", .0)]
    BadDatabaseResponse(&'static str),
    #[error("duckdb: {}", .0)]
    DuckDB(#[from] duckdb::Error),
    #[error("unsupported program format")]
    UnsupportedFormat,
    #[error("unsupported data type: {}", .0)]
    UnsupportedDataType(&'static str),
    #[error("arrow conversion: {}", .0)]
    ArrowConversion(String),
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

    /// Checks if an enum is an "option" enum. Must match [lutra_compiler::sql::utils::is_option].
    fn is_option(&self, variants: &[ir::TyEnumVariant]) -> bool {
        if variants.len() != 2 || !variants[0].ty.is_unit() {
            return false;
        }
        let some_ty = self.get_ty_mat(&variants[1].ty);
        some_ty.kind.is_primitive() || some_ty.kind.is_array()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_in_memory() {
        let _runner = Runner::in_memory(None).unwrap();
        // Basic smoke test
    }

    #[test]
    fn test_file_based() {
        let temp = std::env::temp_dir().join("test_lutra_duckdb.duckdb");
        let _runner = Runner::open(temp.to_str().unwrap(), None).unwrap();
        // Test persistence
        std::fs::remove_file(temp).ok();
    }
}
