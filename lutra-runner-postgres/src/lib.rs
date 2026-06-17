//! PostgreSQL Lutra runner

pub use lutra_runner::Run;

#[cfg(not(any(feature = "postgres", feature = "tokio-postgres")))]
compile_error!("At least one of 'postgres' or 'tokio-postgres' features has to be enabled.");

mod case;
mod params;
mod result;

#[cfg(feature = "tokio-postgres")]
mod schema;

use lutra_bin::{ir, rr};
use lutra_runner::proto;
use thiserror::Error;

use std::collections::HashMap;
#[cfg(feature = "tokio-postgres")]
use std::sync::Mutex;

#[cfg(feature = "postgres")]
use postgres as pg;
#[cfg(not(feature = "postgres"))]
use tokio_postgres as pg;

#[derive(Error, Debug)]
pub enum Error {
    #[error("bad result: {}", .0)]
    BadDatabaseResponse(&'static str),
    #[error("postgres: {:?}", .0)]
    Postgres(#[from] pg::Error),
}

impl From<Error> for proto::Error {
    fn from(e: Error) -> Self {
        proto::Error {
            display: format!("{}", e),
            code: None,
        }
    }
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
    next_program_id: std::sync::atomic::AtomicU32,
    programs: Mutex<HashMap<u32, PreparedProgram>>,
}

#[cfg(feature = "tokio-postgres")]
impl<C> RunnerAsync<C>
where
    C: tokio_postgres::GenericClient,
{
    pub fn new(client: C) -> Self {
        RunnerAsync {
            client,
            next_program_id: std::sync::atomic::AtomicU32::new(0),
            programs: Mutex::new(HashMap::new()),
        }
    }

    pub fn into_inner(self) -> C {
        self.client
    }
}

#[cfg(feature = "tokio-postgres")]
type ConnNoTls = tokio_postgres::Connection<pg::Socket, pg::tls::NoTlsStream>;

#[cfg(feature = "tokio-postgres")]
impl RunnerAsync<tokio_postgres::Client> {
    /// Helper for [tokio_postgres::connect] and [RunnerAsync::new].
    pub async fn connect_no_tls(config: &str) -> Result<(Self, ConnNoTls), Error> {
        let (client, conn) = tokio_postgres::connect(config, tokio_postgres::NoTls).await?;
        Ok((Self::new(client), conn))
    }
}

#[cfg(feature = "tokio-postgres")]
#[derive(Clone)]
struct PreparedProgram {
    program: rr::SqlProgram,
    stmt: tokio_postgres::Statement,
}

#[cfg(feature = "tokio-postgres")]
impl<C> lutra_runner::Run for RunnerAsync<C>
where
    C: tokio_postgres::GenericClient,
{
    async fn prepare(&self, program: rr::Program) -> Result<u32, proto::Error> {
        let program_id = self
            .next_program_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let program = *program.into_sql_postgres().unwrap();

        let stmt = self
            .client
            .prepare(&program.sql)
            .await
            .map_err(Error::from)?;

        self.programs
            .lock()
            .unwrap()
            .insert(program_id, PreparedProgram { program, stmt });
        Ok(program_id)
    }

    async fn execute(
        &self,
        program_id: u32,
        input: &[u8],
    ) -> Result<std::vec::Vec<u8>, proto::Error> {
        let prepared = {
            let guard = self.programs.lock().unwrap();
            guard
                .get(&program_id)
                .cloned()
                .ok_or_else(|| proto::Error::program_not_found(program_id))?
        };

        let ctx = Context::new(&prepared.program.defs);
        let args = params::to_sql(&prepared.program, input, &ctx);

        let rows = self
            .client
            .query(&prepared.stmt, &args.as_refs())
            .await
            .map_err(Error::from)?;

        result::from_sql(&prepared.program, &rows, &ctx).map_err(Into::into)
    }

    async fn release(&self, program_id: u32) -> Result<(), proto::Error> {
        self.programs.lock().unwrap().remove(&program_id);
        Ok(())
    }

    async fn pull_schema(&self) -> Result<std::string::String, proto::Error> {
        crate::schema::pull_interface(self)
            .await
            .map_err(|e| Error::from(e).into())
    }

    async fn get_externals(&self) -> Result<std::vec::Vec<std::string::String>, proto::Error> {
        Ok(vec!["repr:sql-pg".into(), "std::sql".into()])
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

    fn get_ty(&self, ident: &'a ir::Path) -> &'a ir::Ty {
        self.types.get(ident).unwrap()
    }

    fn get_ty_mat(&self, mut ty: &'a ir::Ty) -> &'a ir::Ty {
        while let ir::TyKind::Ident(ident) = &ty.kind {
            if ir::TyStd::try_new(ident).is_some() {
                return ty;
            }
            ty = self.get_ty(ident);
        }
        ty
    }

    /// Checks if an enum is an "option" enum. Must match [lutra_compiler::sql::utils::is_option].
    fn is_option(&self, variants: &[ir::TyEnumVariant]) -> bool {
        if variants.len() != 2 || !variants[0].ty.is_unit() {
            return false;
        }
        let some_ty = self.get_ty_mat(&variants[1].ty);
        some_ty.kind.is_primitive() || some_ty.kind.is_ident() || some_ty.kind.is_array()
    }
}
