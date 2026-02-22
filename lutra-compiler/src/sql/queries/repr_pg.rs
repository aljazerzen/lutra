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
use lutra_sql as sa;

use crate::sql::queries::repr_duckdb;
use crate::sql::utils::Node;
use crate::sql::utils::{self, RelCols};
use crate::sql::{COL_ARRAY_INDEX, queries};

impl<'a> queries::Context<'a> {
    /// Returns columns of ty in the postgres repr.
    pub fn pg_cols<'t>(
        &'t self,
        ty: &'t ir::Ty,
        include_index: bool,
    ) -> Vec<(String, Cow<'t, ir::Ty>)> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) | ir::TyKind::Enum(_) => {
                self.pg_cols_nested(ty, "".into())
            }

            ir::TyKind::Array(item) => {
                // just inner item cols
                let mut r = self.pg_cols_nested(item, "".into());

                if include_index {
                    let ty_index = Cow::Owned(ir::Ty::new(ir::TyPrimitive::int64));
                    r.insert(0, (COL_ARRAY_INDEX.into(), ty_index));
                }

                r
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Returns columns of ty in the postgres repr,
    /// for a type that is nested within another type.
    fn pg_cols_nested<'t>(
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
                    let mut name = repr_duckdb::field_name(f, i);
                    if !prefix.is_empty() {
                        name = format!("{prefix}.{name}");
                    }
                    self.pg_cols_nested(&f.ty, name)
                })
                .collect(),

            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                // nullable column
                self.pg_cols_nested(&variants[1].ty, prefix)
            }

            ir::TyKind::Enum(variants) => {
                let mut r = vec![(terminal(prefix.clone()), Cow::Borrowed(ty_mat))];

                for v in variants {
                    let mut p = v.name.clone();
                    if !prefix.is_empty() {
                        p = format!("{prefix}.{p}");
                    }

                    r.extend(self.pg_cols_nested(&v.ty, p));
                }

                r
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Converts a relation from "postgres repr" into "query repr".
    pub fn pg_import(&mut self, node: Node, ty: &ir::Ty) -> Node {
        let (rel_var, rel) = self.node_into_rel_var(node, ty);

        let mut values = Vec::new();

        // index
        if self.get_ty_mat(ty).kind.is_array() {
            values.push(sa::Expr::IndexBy(vec![]));
        }

        // table columns
        values.extend(self.pg_cols(ty, false).into_iter().map(|(c_name, c_ty)| {
            let ident = utils::identifier(Some(&rel_var), c_name);
            self.pg_col_import(ident, c_ty.as_ref())
        }));

        let mut select = utils::select_empty();
        select.from.extend(rel);
        select.projection = self.projection(ty, values);
        Node::Select(select)
    }

    /// Converts a column from postgres repr into query repr.
    /// This is easy, because tuples have flattened columns in both reprs.
    pub fn pg_col_import(&self, expr_pg: sa::Expr, ty: &ir::Ty) -> sa::Expr {
        // special cases
        if is_ident(ty, &["std", "Date"]) {
            return sa::Expr::Source(format!("({expr_pg}::date - '1970-01-01'::date)"));
        }
        if is_ident(ty, &["std", "Time"]) || is_ident(ty, &["std", "Timestamp"]) {
            return sa::Expr::Source(format!("(EXTRACT(EPOCH FROM {expr_pg})*1000000)::int8"));
        }
        if is_ident(ty, &["std", "Decimal"]) {
            return sa::Expr::Source(format!("({expr_pg}*100)::int8"));
        }

        // import of an enum tag, that is text in postgres repr, but an int16 in query repr
        if let ir::TyKind::Enum(variants) = &ty.kind {
            let operand = Some(Box::new(sa::Expr::Source(format!("{expr_pg}::text"))));
            let mut cases = Vec::with_capacity(variants.len());
            for (tag, v) in variants.iter().enumerate() {
                let variant_name = sa::escape_string(&v.name, '\'');
                cases.push(sa::CaseWhen {
                    condition: sa::Expr::Source(format!("'{variant_name}'")),
                    result: sa::Expr::Source(format!("{tag}")),
                });
            }

            let case = sa::Expr::Case {
                operand,
                cases,
                else_result: None,
            };
            return sa::Expr::Source(format!("{case}::int2"));
        }

        // general case: noop
        // (but with a type cast, just to be safe)
        sa::Expr::Source(format!("{expr_pg}::{}", self.ty_name(ty)))
    }

    /// Convert a relation from "postgres repr" from "query repr".
    pub fn pg_export(&mut self, node: Node, ty: &ir::Ty, include_index: bool) -> Node {
        let mut select = self.node_into_select(node, ty);

        let mut old_projection = select.projection.into_iter();

        if ty.kind.is_array() && !include_index {
            // first column will be index, discard it
            old_projection.next();
        }

        select.projection = Iterator::zip(old_projection, self.pg_cols(ty, include_index))
            .map(|(p, (col, ty))| sa::SelectItem {
                expr: self.pg_col_export(p.expr, ty.as_ref()),
                alias: Some(utils::new_ident(col)),
            })
            .collect();

        Node::Select(select)
    }

    /// Convert a column from "postgres repr" from "query repr".
    /// For example, type std::Date is exported from pg type `int4` into `date`.
    pub fn pg_col_export(&self, expr: sa::Expr, ty: &ir::Ty) -> sa::Expr {
        // special cases
        if is_ident(ty, &["std", "Date"]) {
            return sa::Expr::Source(format!("('1970-01-01'::date + {expr})"));
        }
        if is_ident(ty, &["std", "Time"]) {
            return sa::Expr::Source(format!(
                "('00:00'::time + INTERVAL '1 microsecond' * {expr})"
            ));
        }
        if is_ident(ty, &["std", "Timestamp"]) {
            return sa::Expr::Source(format!("to_timestamp({expr}::float8/1000000.0)"));
        }
        if is_ident(ty, &["std", "Decimal"]) {
            return sa::Expr::Source(format!("({expr}::decimal(20, 0)/100)"));
        }

        // export of an enum tag, that is text in postgres repr, but an int16 in query repr
        if let ir::TyKind::Enum(variants) = &ty.kind {
            let operand = Some(Box::new(sa::Expr::Source(format!("{expr}::int2"))));

            let mut cases = Vec::with_capacity(variants.len());
            for (tag, v) in variants.iter().enumerate() {
                let variant_name = sa::escape_string(&v.name, '\'');
                cases.push(sa::CaseWhen {
                    condition: sa::Expr::Source(format!("{tag}")),
                    result: sa::Expr::Source(format!("'{variant_name}'")),
                });
            }

            let case = sa::Expr::Case {
                operand,
                cases,
                else_result: None,
            };
            return sa::Expr::Source(format!("{case}::text"));
        }

        // general case: noop
        expr
    }

    /// Convert a relation in "query repr" into a column in "postgres JSON repr"
    pub fn pg_serialize(&mut self, node: Node, ty: &ir::Ty) -> Node {
        let (input, input_rels) = self.node_into_rel_var(node, ty);

        let ty_mat = self.get_ty_mat(ty);
        let expr = match &ty_mat.kind {
            ir::TyKind::Array(ty_item) => {
                let item_cols: Vec<_> = self.rel_cols_nested(ty_item, "".into()).collect();
                let item_serialized = self.pg_serialize_nested(&input, &item_cols, ty_item);

                format!(
                    "COALESCE(jsonb_agg({item_serialized} ORDER BY {input}.{COL_ARRAY_INDEX}), '[]'::jsonb)"
                )
            }
            ir::TyKind::Tuple(_) => {
                let cols: Vec<_> = self.rel_cols_nested(ty_mat, "".into()).collect();
                self.pg_serialize_nested(&input, &cols, ty_mat)
            }
            _ => unreachable!("{:?}", ty),
        };

        Node::Column {
            expr: Box::new(sa::Expr::Source(expr)),
            rels: input_rels.into_iter().collect(),
        }
    }

    fn pg_serialize_nested(&self, input_rel: &str, input_cols: &[String], ty: &ir::Ty) -> String {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) => {
                format!("{input_rel}.{}", input_cols[0])
                // if strict { format!("to_json({r})") } else { r }
            }
            ir::TyKind::Tuple(fields) => {
                let mut input_cols = input_cols;

                let mut serialized = Vec::with_capacity(fields.len());
                for field in fields {
                    serialized.push(self.pg_serialize_nested(input_rel, input_cols, &field.ty));

                    let consumed_cols = self.rel_cols_ty_nested(&field.ty).count();
                    input_cols = &input_cols[consumed_cols..];
                }
                format!("jsonb_build_array({})", serialized.join(", "))
            }
            ir::TyKind::Array(_) => {
                // array will be serialized already
                format!("{input_rel}.{}", input_cols[0])
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                // option enum is a nullable column in SQL repr, but in json repr
                // it is an object with one of the two possible properties 0 and 1.
                let col = &input_cols[0];

                let mut r = "CASE".to_string();

                // case: none
                r += &format!(" WHEN {input_rel}.{col} IS NULL THEN jsonb '{{\"0\": []}}'");

                // case: some
                r += " ELSE jsonb_build_object('1', ";
                let is_recursive =
                    lutra_bin::layout::does_enum_variant_contain_recursive(ty_mat, 1_u16);
                if is_recursive {
                    r += &format!("{input_rel}.{col}");
                } else {
                    r += &self.pg_serialize_nested(input_rel, input_cols, &variants[1].ty);
                }

                r += ") END";
                r
            }
            ir::TyKind::Enum(variants) => {
                let tag = &input_cols[0];
                let mut input_cols = &input_cols[1..]; // remove tag

                let mut cases = String::new();
                for (tag, variant) in variants.iter().enumerate() {
                    cases += "WHEN ";
                    cases += &tag.to_string();
                    cases += " THEN jsonb_build_object('";
                    cases += &tag.to_string();
                    cases += "', ";

                    let is_recursive =
                        lutra_bin::layout::does_enum_variant_contain_recursive(ty_mat, tag as u16);
                    if is_recursive {
                        cases += &format!("{input_rel}.{}", input_cols[0]);
                        input_cols = &input_cols[1..];
                    } else {
                        cases += &self.pg_serialize_nested(input_rel, input_cols, &variant.ty);
                        let consumed_cols = self.rel_cols_ty_nested(&variant.ty).count();
                        input_cols = &input_cols[consumed_cols..];
                    }

                    cases += ")";
                }

                format!("CASE {input_rel}.{tag} {cases} END")
            }
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Convert a column in "postgres JSON repr" into a relation in "query repr"
    pub fn pg_deserialize(&mut self, input: Node, input_ty: &ir::Ty, ty: &ir::Ty) -> Node {
        let (input_expr, input_rels) = self.node_into_column_and_rels(input, input_ty);

        let mut result = utils::select_empty();
        result.from.extend(input_rels);

        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(ty_item) => {
                /*
                FROM json_array_elements(...expr...) j
                SELECT ROW_NUMBER(), j.value
                */
                result.from.push(utils::lateral(utils::rel_func(
                    utils::new_ident("jsonb_array_elements"),
                    vec![input_expr],
                    Some("j".into()),
                )));

                result.projection = vec![sa::SelectItem {
                    expr: sa::Expr::IndexBy(vec![]),
                    alias: Some(utils::new_ident(COL_ARRAY_INDEX)),
                }];

                let values = self.pg_deserialize_nested("j.value".into(), ty_item);
                let names = self.rel_cols_nested(ty_item, "".into());

                for (value, name) in std::iter::zip(values, names) {
                    result.projection.push(sa::SelectItem {
                        expr: sa::Expr::Source(value),
                        alias: Some(utils::new_ident(name)),
                    });
                }
            }
            ir::TyKind::Tuple(_) => todo!(),
            ir::TyKind::Enum(_) => {
                let json_ref = input_expr.to_string();

                let values = self.pg_deserialize_nested(json_ref, ty);

                let names = self.rel_cols_nested(ty, "".into());
                for (value, name) in std::iter::zip(values, names) {
                    result.projection.push(sa::SelectItem {
                        expr: sa::Expr::Source(value),
                        alias: Some(utils::new_ident(name)),
                    });
                }
            }
            _ => unreachable!("{:?}", ty),
        };

        Node::Select(result)
    }

    /// Given a serialized JSON value, accessible at `json_ref`,
    /// constructs a list of SQL column expressions that evaluate to the relational repr of the value.
    fn pg_deserialize_nested(&self, json_ref: String, ty: &ir::Ty) -> Vec<String> {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(prim) => {
                let r = match prim {
                    ir::TyPrimitive::text => format!("jsonb_build_array({json_ref}) ->> 0"),
                    _ => format!("{json_ref}::text::{}", self.ty_name(ty)),
                };
                vec![r]
            }

            ir::TyKind::Array(_) => {
                // arrays remain serialized in a single column
                vec![json_ref]
            }
            ir::TyKind::Tuple(fields) => {
                let mut r = Vec::new();

                for (position, f) in fields.iter().enumerate() {
                    r.extend(self.pg_deserialize_nested(format!("({json_ref}->{position})"), &f.ty))
                }

                r
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                let cols =
                    self.pg_deserialize_nested(format!("({json_ref}->'1')"), &variants[1].ty);
                assert_eq!(cols.len(), 1);
                let col = &cols[0];

                vec![format!(
                    "(CASE WHEN ({json_ref}->'1') IS NULL THEN NULL ELSE {col} END)",
                )]
            }
            ir::TyKind::Enum(variants) => {
                let mut r = Vec::new();

                r.push(format!(
                    "(SELECT jsonb_object_keys({json_ref}) LIMIT 1)::int::int2"
                ));

                for (tag, f) in variants.iter().enumerate() {
                    r.extend(self.pg_deserialize_nested(format!("({json_ref}->'{tag}')"), &f.ty))
                }

                r
            }

            _ => todo!(),
        }
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

fn is_ident(ty: &ir::Ty, name: &[&'static str]) -> bool {
    let ir::TyKind::Ident(ty_ident) = &ty.kind else {
        return false;
    };
    ty_ident.0 == name
}
