//! DuckDB representation of Lutra values.
//!
//! When using sql::from, sql::insert or sql::raw with DuckDB, this module
//! handles conversion between DuckDB's native types and the "query repr".

use std::borrow::Cow;

use lutra_bin::ir;
use lutra_sql as sa;

use crate::sql::COL_ARRAY_INDEX;
use crate::sql::COL_VALUE;
use crate::sql::queries;
use crate::sql::utils::RelCols;
use crate::sql::utils::{self, Node};

impl<'a> queries::Context<'a> {
    /// Returns columns of ty in the DuckDB repr.
    pub fn duck_cols<'t>(&'t self, ty: &'t ir::Ty) -> Vec<(String, Cow<'t, ir::Ty>)> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(item) => {
                // Top-level array has columns of it's inner type
                self.duck_cols_row(item)
            }

            _ => self.duck_cols_row(ty),
        }
    }

    fn duck_cols_row<'t>(&'t self, ty: &'t ir::Ty) -> Vec<(String, Cow<'t, ir::Ty>)> {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            // each field is one column (either primitive or STRUCT/LIST/UNION)
            ir::TyKind::Tuple(fields) => fields
                .iter()
                .enumerate()
                .map(|(position, field)| (field_name(field, position), Cow::Borrowed(&field.ty)))
                .collect(),

            // a single column
            _ => vec![("value".into(), Cow::Borrowed(ty))],
        }
    }

    /// Convert a relation in "duckdb repr" into a relation in "query repr".
    pub fn duck_import(&mut self, node: Node, ty: &ir::Ty) -> Node {
        let (rel_var, rel) = self.node_into_rel_var(node, ty);

        // construct new column expressions (in query repr)
        let mut values = Vec::new();
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Array(ty_item) => {
                values.push(sa::Expr::IndexBy(vec![])); // index
                values.extend(self.duck_col_import(rel_var, ty_item));
            }
            _ => {
                values.extend(self.duck_col_import(rel_var, ty));
            }
        }

        let mut select = utils::select_empty();
        select.from.extend(rel);
        select.projection = self.projection(ty, values);
        Node::Select(select)
    }

    /// Convert a column in "duckdb repr" into a relation in "query repr".
    pub fn duck_deserialize(&mut self, input: Node, ty: &ir::Ty) -> Node {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(ty_item) => {
                let (input_expr, input_rels) = self.node_into_column_and_rels(input, ty);

                let mut result = utils::select_empty();
                result.from.extend(input_rels);

                // Expand LIST to rows using unnest(list)
                // unnest generates a single column named 'unnest'
                result.from.push(utils::lateral(utils::rel_func(
                    utils::new_ident("unnest"),
                    vec![input_expr],
                    Some("u".into()),
                )));

                // Add index column (row_number starting from 0)
                result.projection = vec![sa::SelectItem {
                    expr: sa::Expr::IndexBy(vec![]),
                    alias: Some(utils::new_ident(COL_ARRAY_INDEX)),
                }];

                // Deserialize array items
                let value_ref = "u.unnest".to_string();
                let values = self.duck_col_import(value_ref, ty_item);
                result.projection.extend(self.projection(ty_item, values));

                Node::Select(result)
            }
            ir::TyKind::Tuple(_) => {
                let (rel_var, rel) = self.node_into_rel_var(input, ty);

                let values = self.duck_col_import(rel_var, ty);

                let mut result = utils::select_empty();
                result.from.extend(rel);
                result.projection = self.projection(ty, values);
                Node::Select(result)
            }
            ir::TyKind::Enum(_) => {
                panic!("Enum deserialization not yet supported for DuckDB native types")
            }
            _ => unreachable!("{:?}", ty),
        }
    }

    /// Convert a column in "duckdb repr" into columns in "query repr".
    /// For example, STRUCT type is converted into columns of its fields.
    fn duck_col_import(&self, ser_ref: String, ty: &ir::Ty) -> Vec<sa::Expr> {
        // special cases
        if is_ident(ty, &["std", "Date"]) {
            let r = format!("({ser_ref}::date - '1970-01-01'::date)::int4");
            return vec![sa::Expr::Source(r)];
        }
        if is_ident(ty, &["std", "Time"]) || is_ident(ty, &["std", "Timestamp"]) {
            let r = format!("epoch_us({ser_ref})");
            return vec![sa::Expr::Source(r)];
        }
        if is_ident(ty, &["std", "Decimal"]) {
            return vec![sa::Expr::Source(ser_ref)];
        }

        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                vec![sa::Expr::Source(ser_ref)]
            }
            ir::TyKind::Tuple(fields) => {
                // extract each struct field using dot notation
                let mut result = Vec::new();
                for (position, field) in fields.iter().enumerate() {
                    let name = field_name(field, position);

                    let ser_ref = format!("{ser_ref}.{name}");
                    result.extend(self.duck_col_import(ser_ref, &field.ty));
                }
                result
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                // a nullable column
                self.duck_col_import(ser_ref, &variants[1].ty)
            }
            ir::TyKind::Enum(variants) => {
                // Get tag from UNION as ENUM, then convert to int2
                let tag_enum = format!("union_tag({ser_ref})");
                let operand = Some(Box::new(sa::Expr::Source(format!("{tag_enum}::text"))));
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
                let r = format!("{case}::int2");
                vec![sa::Expr::Source(r)]
            }
            _ => unreachable!("Unexpected nested type: {:?}", ty_mat.kind),
        }
    }

    /// Converts columns "query repr" into a column in "duckdb repr".
    /// For example, two columns are converted to STRUCT type.
    pub fn duck_serialize(&mut self, node: Node, ty: &ir::Ty) -> Node {
        let (input, input_rels) = self.node_into_rel_var(node, ty);

        let ty_mat = self.get_ty_mat(ty);
        let expr = match &ty_mat.kind {
            ir::TyKind::Primitive(_) => {
                // pass through
                sa::Expr::Source(format!("{input}.value"))
            }
            ir::TyKind::Tuple(_) => {
                // pack into STRUCT
                let cols: Vec<_> = self
                    .rel_cols_nested(ty_mat, "".into())
                    .map(|c| utils::identifier(Some(&input), c))
                    .collect();
                self.duck_col_export(cols, ty_mat)
            }
            ir::TyKind::Array(ty_item) => {
                // aggregate into LIST
                let cols: Vec<_> = self
                    .rel_cols_nested(ty_item, "".into())
                    .map(|c| utils::identifier(Some(&input), c))
                    .collect();
                let item_serialized = self.duck_col_export(cols, ty_item);

                let list_agg =
                    format!("list({item_serialized} ORDER BY {input}.{COL_ARRAY_INDEX})");

                // empty array needs explicit type cast for DuckDB
                let empty_list = format!("CAST([] AS {}[])", self.duck_compile_ty_name(ty_item));

                sa::Expr::Source(format!("COALESCE({list_agg}, {empty_list})"))
            }
            ir::TyKind::Enum(_) => {
                panic!("Enum serialization not yet supported for DuckDB native types")
            }
            _ => unreachable!("{:?}", ty),
        };

        Node::Column {
            expr: Box::new(expr),
            rels: input_rels.into_iter().collect(),
        }
    }

    /// Convert columns in "query repr" to "duckdb repr".
    fn duck_col_export(&self, mut cols: Vec<sa::Expr>, ty: &ir::Ty) -> sa::Expr {
        // special cases
        if is_ident(ty, &["std", "Date"]) {
            let expr = cols.remove(0);
            return sa::Expr::Source(format!("('1970-01-01'::date + {expr})"));
        }
        if is_ident(ty, &["std", "Time"]) {
            let expr = cols.remove(0);
            return sa::Expr::Source(format!(
                "('00:00'::time + INTERVAL '1 microsecond' * {expr})"
            ));
        }
        if is_ident(ty, &["std", "Timestamp"]) {
            let expr = cols.remove(0);
            return sa::Expr::Source(format!("make_timestamp({expr})"));
        }
        if is_ident(ty, &["std", "Decimal"]) {
            return cols.remove(0);
        }

        let ty_mat = self.get_ty_mat(ty);

        // Handle unit type (empty tuple) - no columns, return NULL
        if ty_mat.is_unit() {
            // TODO: this is probably not ok, it will break `enum {none, some: ()}`
            return sa::Expr::Source("NULL".to_string());
        }

        match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                // single column value - pass through
                // nested array - already serialized as LIST in a single column
                cols.remove(0)
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                self.duck_col_export(cols, &variants[1].ty)
            }
            ir::TyKind::Tuple(fields) => {
                // construct STRUCT using struct_pack(field1 := val1, field2 := val2, ...)
                let mut parts = Vec::new();
                for (position, field) in fields.iter().enumerate() {
                    // Use field name if present, otherwise use positional name (_0, _1, etc.)
                    let name = utils::new_ident(field_name(field, position));

                    let f_col_count = self.rel_cols_ty_nested(&field.ty).count();
                    let f_cols = take_front(&mut cols, f_col_count);

                    let field_expr = self.duck_col_export(f_cols, &field.ty);

                    parts.push(format!("{name} := {field_expr}"));
                }

                sa::Expr::Source(format!("struct_pack({})", parts.join(", ")))
            }

            ir::TyKind::Enum(variants) => {
                // enum - convert {tag, ..variant columns..} into UNION
                // Columns layout: [tag, variant0_cols..., variant1_cols..., ...]

                let tag_expr = cols.remove(0);
                let operand = Some(Box::new(sa::Expr::Source(format!("{tag_expr}::int2"))));

                let union_ty = self.duck_compile_ty_name(ty);

                let mut cases = Vec::with_capacity(variants.len());
                for (position, v) in variants.iter().enumerate() {
                    let v_col_count = self.rel_cols_ty_nested(&v.ty).count();
                    let v_cols = take_front(&mut cols, v_col_count);

                    let inner = self.duck_col_export(v_cols, &v.ty);

                    let variant_name = utils::new_ident(&v.name);

                    // cast first variant to union type, all others will infer
                    let cast = if position == 0 {
                        format!("::{union_ty}")
                    } else {
                        "".into()
                    };

                    cases.push(sa::CaseWhen {
                        condition: sa::Expr::Source(format!("{position}")),
                        result: sa::Expr::Source(format!(
                            "union_value({variant_name} := {inner}){cast}"
                        )),
                    });
                }

                sa::Expr::Case {
                    operand,
                    cases,
                    else_result: None,
                }
            }
            _ => unreachable!("Unexpected nested type: {:?}", ty_mat.kind),
        }
    }

    /// Converts a relation from "query repr" to "arrow repr".
    pub fn duck_export(&mut self, node: Node, ty: &ir::Ty, include_index: bool) -> Node {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) => node,
            ir::TyKind::Tuple(f) if f.is_empty() => node,
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                // option enum is a single nullable column in both reprs
                node
            }
            ir::TyKind::Tuple(_) | ir::TyKind::Enum(_) => {
                let (cols, rels) = self.node_into_columns_and_rels(node, ty);

                let mut select = utils::select_empty();

                select.from.extend(rels);
                select.projection = self.duck_export_row(cols, ty);
                Node::Select(select)
            }
            ir::TyKind::Array(ty_item) => {
                // arrays export the inner type as a row

                let (mut cols, rels) = self.node_into_columns_and_rels(node, ty);

                let mut select = utils::select_empty();

                let index = cols.remove(0);

                select.from.extend(rels);
                select.projection = self.duck_export_row(cols, ty_item);

                if include_index {
                    // special case: retain the index column
                    select
                        .projection
                        .insert(0, sa::SelectItem::unnamed(index.clone()));
                }

                // apply order by
                let mut query = utils::query_select(select);
                query.order_by = utils::order_by_one(index);

                Node::Query(query)
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Converts a row from "query repr" to "arrow repr".
    fn duck_export_row(&mut self, mut cols: Vec<sa::Expr>, ty: &ir::Ty) -> Vec<sa::SelectItem> {
        if let ir::TyKind::Tuple(ty_fields) = &self.get_ty_mat(ty).kind {
            // convert {_0, _1_0, _1_1} into {id, address: STRUCT[street, number]}

            let mut res_projection = Vec::with_capacity(ty_fields.len());

            for (position, field) in ty_fields.iter().enumerate() {
                let f_ty = &field.ty;

                // take columns for this field
                let f_col_count = self.rel_cols_ty_nested(f_ty).count();
                let f_cols = take_front(&mut cols, f_col_count);

                let expr = self.duck_col_export(f_cols, f_ty);

                res_projection.push(sa::SelectItem {
                    expr,
                    alias: Some(utils::new_ident(field_name(field, position))),
                });
            }

            res_projection
        } else {
            // primitive -> no conversion
            // nested array -> query repr will already contain serialized items
            // enum -> convert columns to single UNION column

            let expr = self.duck_col_export(cols, ty);

            vec![sa::SelectItem {
                expr,
                alias: Some(utils::new_ident(COL_VALUE)),
            }]
        }
    }
}

fn is_ident(ty: &ir::Ty, name: &[&'static str]) -> bool {
    let ir::TyKind::Ident(ty_ident) = &ty.kind else {
        return false;
    };
    ty_ident.0 == name
}

fn take_front(cols: &mut Vec<sa::Expr>, count: usize) -> Vec<sa::Expr> {
    let rem = cols.split_off(count);
    std::mem::replace(cols, rem)
}

pub(crate) fn field_name(field: &ir::TyTupleField, position: usize) -> String {
    match &field.name {
        Some(x) => x.clone(),
        None => format!("field{position}"),
    }
}
