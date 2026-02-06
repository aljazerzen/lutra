//! DuckDB representation of Lutra values.
//!
//! When using sql::from, sql::insert or sql::raw with DuckDB, this module
//! handles conversion between DuckDB's native types and Lutra's query representation.
//!
//! DuckDB table representation:
//! - Arrays: LIST[] (native DuckDB list type)
//! - Nested tuples: STRUCT() (native DuckDB struct type)
//! - Flat tuples: Flattened columns (same as query repr)
//! - Enums: UNION(variant1 T1, variant2 T2, ...) (DuckDB native union type)
//! - Maybe: Nullable column (NULL = none, value = some)
//!
//! Query representation (computation):
//! - Arrays: Multiple rows with index column
//! - Tuples: Flattened columns
//! - Enums: int2 tag column + flattened variant payload columns
//! - Maybe: Nullable column (same as table repr)

use std::borrow::Cow;

use lutra_bin::ir;

use crate::sql::queries;
use crate::sql::utils;

impl<'a> queries::Context<'a> {
    /// Constructs a projection that imports from DuckDB repr into query repr.
    pub(super) fn duck_repr_import_projection(
        &self,
        rel_var: Option<&str>,
        ty: &ir::Ty,
    ) -> Vec<sql_ast::SelectItem> {
        let mut values = Vec::new();

        // index column for arrays
        if self.get_ty_mat(ty).kind.is_array() {
            values.push(utils::new_index(None));
        }

        // table columns
        values.extend(
            self.duck_repr_columns(ty)
                .into_iter()
                .map(move |(f_name, f_ty)| {
                    let ident = utils::identifier(rel_var, f_name);
                    self.duck_repr_import(ident, f_ty.as_ref())
                }),
        );
        self.projection(ty, values)
    }

    /// Returns columns of ty in the DuckDB repr.
    /// This is the top-level entry point for determining table columns.
    pub(super) fn duck_repr_columns<'t>(&'t self, ty: &'t ir::Ty) -> Vec<(String, Cow<'t, ir::Ty>)> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Enum(_) => {
                self.duck_repr_columns_nested(ty, "".into())
            }

            ir::TyKind::Array(item) => {
                // Arrays from tables are represented as multiple rows, not a LIST column
                // So we look at the item type to determine columns
                self.duck_repr_columns(item)
            }

            ir::TyKind::Tuple(fields) => {
                // Top-level tuples always flatten to columns
                // Nested fields (arrays, tuples, enums) become STRUCT/LIST/UNION columns
                fields
                    .iter()
                    .enumerate()
                    .flat_map(|(i, f)| {
                        let name = if let Some(n) = &f.name {
                            n.clone()
                        } else {
                            format!("c{i}")
                        };
                        self.duck_repr_columns_nested(&f.ty, name)
                    })
                    .collect()
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Returns columns of ty in the DuckDB repr,
    /// for a type that is nested within another type.
    /// When called, the type should always be represented as a single column
    /// (STRUCT for tuples, LIST for arrays, UNION for enums, or primitive).
    fn duck_repr_columns_nested<'t>(
        &'t self,
        ty: &'t ir::Ty,
        prefix: String,
    ) -> Vec<(String, Cow<'t, ir::Ty>)> {
        fn terminal(prefix: String) -> String {
            if prefix.is_empty() {
                "value".to_string()
            } else {
                prefix
            }
        }

        if is_special_framed(ty) {
            return vec![(terminal(prefix), Cow::Borrowed(ty))];
        }

        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) => {
                // Primitives are a single column
                vec![(terminal(prefix), Cow::Borrowed(ty))]
            }

            ir::TyKind::Array(_) => {
                // Nested arrays stored as LIST[] - single column
                vec![(terminal(prefix), Cow::Borrowed(ty))]
            }

            ir::TyKind::Tuple(_) => {
                // Nested tuples are always stored as STRUCT - single column
                vec![(terminal(prefix), Cow::Borrowed(ty))]
            }

            ir::TyKind::Enum(variants) if utils::is_maybe(variants) => {
                // Maybe/Option - nullable column, unwrap to inner type
                self.duck_repr_columns_nested(&variants[1].ty, prefix)
            }

            ir::TyKind::Enum(_) => {
                // Enums stored as UNION - single column
                vec![(terminal(prefix), Cow::Borrowed(ty))]
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Imports a value from DuckDB repr into query repr.
    /// For example, UNION type is converted into int2 tag + variant columns.
    pub(super) fn duck_repr_import(&self, expr_duck: sql_ast::Expr, ty: &ir::Ty) -> sql_ast::Expr {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            return sql_ast::Expr::Source(format!("({expr_duck}::date - '1970-01-01'::date)"));
        }
        // special case: std::Time & std::Timestamp
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && (ty_ident.0 == ["std", "Time"] || ty_ident.0 == ["std", "Timestamp"])
        {
            return sql_ast::Expr::Source(format!("(EXTRACT(EPOCH FROM {expr_duck})*1000000)::int8"));
        }
        // special case: std::Decimal
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Decimal"]
        {
            return sql_ast::Expr::Source(format!("({expr_duck}*100)::int8"));
        }

        // import of an enum tag from UNION type
        if let ir::TyKind::Enum(variants) = &ty.kind {
            // Get tag from UNION as ENUM, then convert to int2
            let tag_enum = format!("union_tag({expr_duck})");
            let operand = Some(Box::new(sql_ast::Expr::Source(format!("{tag_enum}::text"))));
            let mut cases = Vec::with_capacity(variants.len());
            for (tag, v) in variants.iter().enumerate() {
                let variant_name = sql_ast::escape_string(&v.name, '\'');
                cases.push(sql_ast::CaseWhen {
                    condition: sql_ast::Expr::Source(format!("'{variant_name}'")),
                    result: sql_ast::Expr::Source(format!("{tag}")),
                });
            }

            let case = sql_ast::Expr::Case {
                operand,
                cases,
                else_result: None,
            };
            return sql_ast::Expr::Source(format!("{case}::int2"));
        }

        // Arrays and nested tuples are handled at the FROM level, not here
        // Just pass through with type cast
        sql_ast::Expr::Source(format!("{expr_duck}::{}", self.compile_ty_name(ty)))
    }

    /// Exports a value into DuckDB repr from query repr.
    /// For example, int2 tag is converted to UNION type.
    pub(super) fn duck_repr_export(&self, expr: sql_ast::Expr, ty: &ir::Ty) -> sql_ast::Expr {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            return sql_ast::Expr::Source(format!("('1970-01-01'::date + {expr})"));
        }
        // special case: std::Time
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Time"]
        {
            return sql_ast::Expr::Source(format!(
                "('00:00'::time + INTERVAL '1 microsecond' * {expr})"
            ));
        }
        // special case: std::Timestamp
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Timestamp"]
        {
            return sql_ast::Expr::Source(format!("to_timestamp({expr}::float8/1000000.0)"));
        }
        // special case: std::Decimal
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Decimal"]
        {
            return sql_ast::Expr::Source(format!("({expr}::decimal(20, 0)/100)"));
        }

        // export of an enum tag to UNION type from int2
        if let ir::TyKind::Enum(variants) = &ty.kind {
            let operand = Some(Box::new(sql_ast::Expr::Source(format!("{expr}::int2"))));

            let mut cases = Vec::with_capacity(variants.len());
            for (tag, v) in variants.iter().enumerate() {
                let variant_name = sql_ast::escape_string(&v.name, '\'');
                // Create UNION value using union_value(tag := payload)
                // For now, we need the payload expression - this will be expanded in the full implementation
                // TODO: This needs to extract the payload from variant columns
                cases.push(sql_ast::CaseWhen {
                    condition: sql_ast::Expr::Source(format!("{tag}")),
                    result: sql_ast::Expr::Source(format!("union_value({variant_name} := NULL)")),
                });
            }

            let case = sql_ast::Expr::Case {
                operand,
                cases,
                else_result: None,
            };
            return case;
        }

        // general case: noop
        expr
    }
}

/// Framed types for which there is a "specialized" DuckDB repr.
const SPECIAL_FRAMED_TYPES: &[&[&str]] = &[
    &["std", "Date"],
    &["std", "Time"],
    &["std", "Timestamp"],
    &["std", "Decimal"],
];

fn is_special_framed(ty: &ir::Ty) -> bool {
    let ir::TyKind::Ident(name) = &ty.kind else {
        return false;
    };

    SPECIAL_FRAMED_TYPES.iter().any(|f| name.0 == *f)
}
