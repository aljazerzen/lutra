//! PostgreSQL Lutra runner

#[cfg(not(any(feature = "postgres", feature = "tokio-postgres")))]
compile_error!("At least one of 'postgres' or 'tokio-postgres' features has to be enabled.");

mod case;
mod params;
mod result;

#[cfg(feature = "tokio-postgres")]
mod schema;

use std::collections::HashMap;
use thiserror::Error;

#[cfg(feature = "postgres")]
use postgres::Error as PgError;
#[cfg(not(feature = "postgres"))]
use tokio_postgres::Error as PgError;

use lutra_bin::{ir, rr};

#[derive(Error, Debug)]
pub enum Error {
    #[error("bad result: {}", .0)]
    BadDatabaseResponse(&'static str),
    #[error("postgres: {}", .0)]
    Postgres(#[from] PgError),
}

#[cfg(feature = "postgres")]
pub fn execute(
    client: &mut impl postgres::GenericClient,
    program: &rr::SqlProgram,
    input: &[u8],
) -> Result<Vec<u8>, Error> {
    // prepare
    let def = client.prepare(&program.sql)?;

    let ctx = Context::new(&program.defs);

    // pack input into query args
    let args = params::to_sql(program, input, &ctx);

    // execute
    let rows = client.query(&def, &args.as_refs())?;

    // convert result from sql
    result::from_sql(program, &rows, &ctx)
}

#[cfg(feature = "tokio-postgres")]
pub struct RunnerAsync<C: tokio_postgres::GenericClient = tokio_postgres::Client> {
    client: C,
}

impl<C> RunnerAsync<C>
where
    C: tokio_postgres::GenericClient,
{
    pub fn new(client: C) -> Self {
        RunnerAsync { client }
    }
}

impl RunnerAsync<tokio_postgres::Client> {
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
impl<C> lutra_runner::Run for RunnerAsync<C>
where
    C: tokio_postgres::GenericClient,
{
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
        let ctx = Context::new(&handle.program.defs);

        // pack input into query args
        let args = params::to_sql(&handle.program, input, &ctx);

        let rows = self.client.query(&handle.stmt, &args.as_refs()).await?;

        // convert result from sql
        result::from_sql(&handle.program, &rows, &ctx)
    }

    async fn get_interface(&self) -> Result<std::string::String, Self::Error> {
        Ok(crate::schema::pull_interface(self).await?)
    }
}

struct Context<'a> {
    pub types: HashMap<&'a ir::Path, &'a ir::Ty>,
}

impl<'a> Context<'a> {
    fn new(ty_defs: &'a [ir::TyDef]) -> Self {
        Context {
            types: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
        }
    }

    fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }
}

/// Checks if an enum is a "maybe" enum. Must match [lutra_compiler::sql::utils::is_maybe].
pub fn is_maybe(variants: &[ir::TyEnumVariant]) -> bool {
    variants.len() == 2 && variants[0].ty.is_unit() && variants[1].ty.kind.is_primitive()
}
