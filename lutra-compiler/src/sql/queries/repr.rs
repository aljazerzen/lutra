//! Representation conversion.
//!
//! Converts between different representations of Lutra values. There are:
//! - query repr,
//! - Postgres repr,
//! - JSON repr,
//! - DuckDB repr,
//! - arrow repr.

use crate::sql::Dialect;
use crate::sql::queries::Context;
use crate::sql::utils::Node;
use lutra_bin::ir;
use std::borrow::Cow;

impl<'a> Context<'a> {
    /// Converts a relation from "native repr" into "query repr".
    /// Native repr is either the postgres repr or duckdb repr.
    pub fn native_import(&mut self, node: Node, ty: &ir::Ty) -> Node {
        match self.dialect() {
            Dialect::Postgres => self.pg_import(node, ty),
            Dialect::DuckDB => self.duck_import(node, ty),
        }
    }

    /// Returns columns in the native repr.
    pub fn native_cols<'t>(&'t self, ty: &'t ir::Ty) -> Vec<(String, Cow<'t, ir::Ty>)> {
        match self.dialect() {
            Dialect::Postgres => self.pg_cols(ty),
            Dialect::DuckDB => self.duck_cols(ty),
        }
    }

    /// Converts a relation from "query repr" to "native repr".
    /// Native repr is either the postgres repr or duckdb repr.
    pub fn native_export(&mut self, node: Node, ty: &ir::Ty) -> Node {
        match self.dialect() {
            Dialect::Postgres => self.pg_export(node, ty),
            Dialect::DuckDB => self.duck_export(node, ty),
        }
    }

    /// Convert a relation in "query repr" into a column in "serialized repr".
    ///
    /// What this means depends on the dialect:
    /// - on postgres, this will serialize to json
    /// - on duckdb, this will construct STRUCT/LIST/UNION types
    pub fn serialize(&mut self, node: Node, ty: &ir::Ty) -> Node {
        match self.dialect() {
            Dialect::Postgres => self.pg_serialize(node, ty),
            Dialect::DuckDB => self.duck_serialize(node, ty),
        }
    }

    /// Convert a column in "serialized repr" into a relation in "query repr".
    ///
    /// What this means depends on the dialect:
    /// - on postgres, this will unpack json
    /// - on duckdb, this will deconstruct STRUCT/LIST/UNION types
    pub fn deserialize(&mut self, input: Node, input_ty: &ir::Ty, ty: &ir::Ty) -> Node {
        match self.dialect() {
            Dialect::Postgres => self.pg_deserialize(input, input_ty, ty),
            Dialect::DuckDB => self.duck_deserialize(input, ty),
        }
    }
}
