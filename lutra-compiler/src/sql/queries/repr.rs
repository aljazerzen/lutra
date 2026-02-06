//! Representation dispatch for SQL dialects.
//!
//! This module provides clean dispatch functions that route to the appropriate
//! dialect-specific representation handlers (PostgreSQL or DuckDB).

use crate::sql::queries::Context;
use lutra_bin::ir;
use std::borrow::Cow;

impl<'a> Context<'a> {
    /// Constructs a projection that imports from table repr into query repr.
    pub(super) fn repr_import_projection(
        &self,
        rel_var: Option<&str>,
        ty: &ir::Ty,
    ) -> Vec<sql_ast::SelectItem> {
        match self.dialect() {
            super::super::Dialect::Postgres => self.pg_repr_import_projection(rel_var, ty),
            super::super::Dialect::DuckDB => self.duck_repr_import_projection(rel_var, ty),
        }
    }

    /// Returns columns of ty in the table repr.
    pub(super) fn repr_columns<'t>(&'t self, ty: &'t ir::Ty) -> Vec<(String, Cow<'t, ir::Ty>)> {
        match self.dialect() {
            super::super::Dialect::Postgres => self.pg_repr_columns(ty),
            super::super::Dialect::DuckDB => self.duck_repr_columns(ty),
        }
    }

    /// Exports a value into table repr from query repr.
    pub(super) fn repr_export(&self, expr: sql_ast::Expr, ty: &ir::Ty) -> sql_ast::Expr {
        match self.dialect() {
            super::super::Dialect::Postgres => self.pg_repr_export(expr, ty),
            super::super::Dialect::DuckDB => self.duck_repr_export(expr, ty),
        }
    }
}
