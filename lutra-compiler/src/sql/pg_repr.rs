//! PostgreSQL representation of Lutra values.
//!
//! When using sql::from, sql::insert or sql::raw, Lutra expects the values to
//! be expressed in a very specific format, nicknamed "postgres repr".
//!
//! This format:
//! - is obvious for simple data types,
//! - has nuanced specifics around values that don't have a conventional SQL repr,
//! - is slightly different than the representation that Lutra compiler uses for
//!   computation, which is nicknamed as "query repr".
//!

use std::borrow::Cow;

use lutra_bin::ir;

use crate::sql::utils;

impl<'a> super::queries::Context<'a> {
    /// Constructs a projection that imports from postgres repr into query repr.
    pub(super) fn pg_repr_import_projection(
        &self,
        rel_var: Option<&str>,
        ty: &ir::Ty,
    ) -> Vec<sql_ast::SelectItem> {
        let mut values = Vec::new();

        // index
        if self.get_ty_mat(ty).kind.is_array() {
            values.push(utils::new_index(None));
        }

        // table columns
        values.extend(
            self.pg_repr_columns(ty)
                .into_iter()
                .map(move |(f_name, f_ty)| {
                    let ident = utils::identifier(rel_var, f_name);
                    self.pg_repr_import(ident, f_ty.as_ref())
                }),
        );
        self.projection(ty, values)
    }

    /// Returns columns of ty in the postgres repr.
    pub(super) fn pg_repr_columns<'t>(&'t self, ty: &'t ir::Ty) -> Vec<(String, Cow<'t, ir::Ty>)> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) | ir::TyKind::Enum(_) => {
                self.pg_repr_columns_nested(ty, "".into())
            }

            ir::TyKind::Array(item) => {
                // no index column
                self.pg_repr_columns_nested(item, "".into())
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Returns columns of ty in the postgres repr,
    /// for a type that is nested within another type.
    fn pg_repr_columns_nested<'t>(
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
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                // primitives are a single column
                // array is a encoded as json

                vec![(terminal(prefix), Cow::Borrowed(ty))]
            }

            ir::TyKind::Tuple(fields) => fields
                .iter()
                .enumerate()
                .flat_map(|(i, f)| {
                    let name = if let Some(n) = &f.name {
                        n.clone()
                    } else {
                        format!("c{i}")
                    };
                    let name = if prefix.is_empty() {
                        name
                    } else {
                        format!("{prefix}.{name}")
                    };
                    self.pg_repr_columns_nested(&f.ty, name)
                })
                .collect(),

            ir::TyKind::Enum(variants) if utils::is_maybe(variants) => {
                // nullable column
                self.pg_repr_columns_nested(&variants[1].ty, prefix)
            }

            ir::TyKind::Enum(variants) => {
                let mut r = vec![(terminal(prefix.clone()), Cow::Borrowed(ty_mat))];

                for v in variants {
                    let prefix = if prefix.is_empty() {
                        v.name.clone()
                    } else {
                        format!("{prefix}.{}", v.name)
                    };

                    r.extend(self.pg_repr_columns_nested(&v.ty, prefix));
                }

                r
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Imports a value from postgres repr into query repr.
    /// For example, pg type `date` is converted into `int4`.
    pub(super) fn pg_repr_import(&self, expr_pg: sql_ast::Expr, ty: &ir::Ty) -> sql_ast::Expr {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            return sql_ast::Expr::Source(format!("({expr_pg}::date - '1970-01-01'::date)"));
        }
        // special case: std::Time & std::Timestamp
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && (ty_ident.0 == ["std", "Time"] || ty_ident.0 == ["std", "Timestamp"])
        {
            return sql_ast::Expr::Source(format!("(EXTRACT(EPOCH FROM {expr_pg})*1000000)::int8"));
        }
        // special case: std::Decimal
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Decimal"]
        {
            return sql_ast::Expr::Source(format!("({expr_pg}*100)::int8"));
        }

        // import of an enum tag, that is text in postgres repr, but an int16 in query repr
        if let ir::TyKind::Enum(variants) = &ty.kind {
            let operand = Some(Box::new(sql_ast::Expr::Source(format!("{expr_pg}::text"))));
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

        // general case: noop
        // (but with a type cast, just to be safe)
        sql_ast::Expr::Source(format!("{expr_pg}::{}", self.compile_ty_name(ty)))
    }

    /// Exports a value into postgres repr from query repr.
    /// For example, type std::Date is exported from pg type `int4` into `date`.
    pub(super) fn pg_repr_export(&self, expr: sql_ast::Expr, ty: &ir::Ty) -> sql_ast::Expr {
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
        // special case: std::Timestamp
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Decimal"]
        {
            return sql_ast::Expr::Source(format!("({expr}::decimal(20, 0)/100)"));
        }

        // export of an enum tag, that is text in postgres repr, but an int16 in query repr
        if let ir::TyKind::Enum(variants) = &ty.kind {
            let operand = Some(Box::new(sql_ast::Expr::Source(format!("{expr}::int2"))));

            let mut cases = Vec::with_capacity(variants.len());
            for (tag, v) in variants.iter().enumerate() {
                let variant_name = sql_ast::escape_string(&v.name, '\'');
                cases.push(sql_ast::CaseWhen {
                    condition: sql_ast::Expr::Source(format!("{tag}")),
                    result: sql_ast::Expr::Source(format!("'{variant_name}'")),
                });
            }

            let case = sql_ast::Expr::Case {
                operand,
                cases,
                else_result: None,
            };
            return sql_ast::Expr::Source(format!("{case}::text"));
        }

        // general case: noop
        expr
    }
}

/// Framed types for which there is a "specialized" Postgres repr.
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
