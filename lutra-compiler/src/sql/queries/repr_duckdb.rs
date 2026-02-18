//! DuckDB representation of Lutra values.
//!
//! When using sql::from, sql::insert or sql::raw with DuckDB, this module
//! handles conversion between DuckDB's native types and the "query repr".

use std::borrow::Cow;

use lutra_bin::ir;

use crate::sql::COL_ARRAY_INDEX;
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
                values.push(utils::new_index(None)); // index
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
                result.projection = vec![sql_ast::SelectItem {
                    expr: utils::new_index(None),
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
    fn duck_col_import(&self, ser_ref: String, ty: &ir::Ty) -> Vec<sql_ast::Expr> {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            let r = format!("({ser_ref}::date - '1970-01-01'::date)::int4");
            return vec![sql_ast::Expr::Source(r)];
        }
        // special case: std::Time & std::Timestamp
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && (ty_ident.0 == ["std", "Time"] || ty_ident.0 == ["std", "Timestamp"])
        {
            let r = format!("epoch_us({ser_ref})");
            return vec![sql_ast::Expr::Source(r)];
        }
        // special case: std::Decimal
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Decimal"]
        {
            let r = format!("({ser_ref}*100)::int8");
            return vec![sql_ast::Expr::Source(r)];
        }

        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                vec![sql_ast::Expr::Source(ser_ref)]
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
                let r = format!("{case}::int2");
                vec![sql_ast::Expr::Source(r)]
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
                sql_ast::Expr::Source(format!("{input}.value"))
            }
            ir::TyKind::Tuple(_) => {
                // pack into STRUCT
                let cols: Vec<_> = self.rel_cols_nested(ty_mat, "".into()).collect();
                self.duck_col_export(&input, &cols, ty_mat)
            }
            ir::TyKind::Array(ty_item) => {
                // aggregate into LIST
                let item_cols: Vec<_> = self.rel_cols_nested(ty_item, "".into()).collect();
                let item_serialized = self.duck_col_export(&input, &item_cols, ty_item);

                let list_agg =
                    format!("list({item_serialized} ORDER BY {input}.{COL_ARRAY_INDEX})");

                // empty array needs explicit type cast for DuckDB
                let empty_list = format!("CAST([] AS {}[])", self.duck_compile_ty_name(ty_item));

                sql_ast::Expr::Source(format!("COALESCE({list_agg}, {empty_list})"))
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
    fn duck_col_export(&self, rel_var: &str, cols: &[String], ty: &ir::Ty) -> sql_ast::Expr {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            let expr = format!("{rel_var}.{}", cols[0]);
            return sql_ast::Expr::Source(format!("('1970-01-01'::date + {expr})"));
        }
        // special case: std::Time
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Time"]
        {
            let expr = format!("{rel_var}.{}", cols[0]);
            return sql_ast::Expr::Source(format!(
                "('00:00'::time + INTERVAL '1 microsecond' * {expr})"
            ));
        }
        // special case: std::Timestamp
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Timestamp"]
        {
            let expr = format!("{rel_var}.{}", cols[0]);
            return sql_ast::Expr::Source(format!("make_timestamp({expr})"));
        }
        // special case: std::Decimal
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Decimal"]
        {
            let expr = format!("{rel_var}.{}", cols[0]);
            return sql_ast::Expr::Source(format!("({expr}::decimal(20, 0)/100)"));
        }

        let ty_mat = self.get_ty_mat(ty);

        // Handle unit type (empty tuple) - no columns, return NULL
        if ty_mat.is_unit() {
            // TODO: this is probably not ok, it will break `enum {none, some: ()}`
            return sql_ast::Expr::Source("NULL".to_string());
        }

        match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                // single column value - pass through
                // nested array - already serialized as LIST in a single column
                sql_ast::Expr::Source(format!("{rel_var}.{}", cols[0]))
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                self.duck_col_export(rel_var, cols, &variants[1].ty)
            }
            ir::TyKind::Tuple(fields) => {
                // Construct STRUCT using struct_pack(field1 := val1, field2 := val2, ...)
                let mut parts = Vec::new();
                let mut col_idx = 0;

                for (position, field) in fields.iter().enumerate() {
                    // Use field name if present, otherwise use positional name (_0, _1, etc.)
                    let name = utils::new_ident(field_name(field, position));

                    let field_col_count = self.rel_cols_ty_nested(&field.ty).count();

                    let field_expr = self.duck_col_export(
                        rel_var,
                        &cols[col_idx..col_idx + field_col_count],
                        &field.ty,
                    );

                    parts.push(format!("{name} := {field_expr}"));
                    col_idx += field_col_count;
                }

                sql_ast::Expr::Source(format!("struct_pack({})", parts.join(", ")))
            }

            ir::TyKind::Enum(variants) => {
                // enum - convert {tag, ..variant columns..} into UNION
                // Columns layout: [tag, variant0_cols..., variant1_cols..., ...]

                let tag_expr = format!("{rel_var}.{}", cols[0]);
                let operand = Some(Box::new(sql_ast::Expr::Source(format!("{tag_expr}::int2"))));

                let union_ty = self.duck_compile_ty_name(ty);

                let mut cases = Vec::with_capacity(variants.len());
                let mut col_idx = 1; // skip tag column

                for (position, v) in variants.iter().enumerate() {
                    let variant_col_count = self.rel_cols_ty_nested(&v.ty).count();
                    let variant_cols = &cols[col_idx..col_idx + variant_col_count];

                    let inner = self.duck_col_export(rel_var, variant_cols, &v.ty);

                    let variant_name = utils::new_ident(&v.name);

                    // cast first variant to union type, all others will infer
                    let cast = if position == 0 {
                        format!("::{union_ty}")
                    } else {
                        "".into()
                    };

                    cases.push(sql_ast::CaseWhen {
                        condition: sql_ast::Expr::Source(format!("{position}")),
                        result: sql_ast::Expr::Source(format!(
                            "union_value({variant_name} := {inner}){cast}"
                        )),
                    });

                    col_idx += variant_col_count;
                }

                sql_ast::Expr::Case {
                    operand,
                    cases,
                    else_result: None,
                }
            }
            _ => unreachable!("Unexpected nested type: {:?}", ty_mat.kind),
        }
    }

    /// Converts a relation from "query repr" to "arrow repr".
    pub fn duck_export(&mut self, node: Node, ty: &ir::Ty, keep_index: bool) -> Node {
        let ty_mat = self.get_ty_mat(ty);

        match &ty_mat.kind {
            ir::TyKind::Primitive(_) => node,
            ir::TyKind::Tuple(f) if f.is_empty() => node,
            ir::TyKind::Tuple(_) => self.duck_export_row(node, ty),
            ir::TyKind::Array(ty_item) => {
                // arrays export the inner type as a row
                let mut r = self.duck_export_row(node, ty_item);

                if keep_index {
                    // special case: retain the index column
                    let mut select = self.node_into_select(r, ty_item);
                    select.projection.insert(
                        0,
                        sql_ast::SelectItem::unnamed(utils::identifier(
                            None::<&str>,
                            COL_ARRAY_INDEX,
                        )),
                    );
                    r = Node::Select(select);
                }

                r
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                // Option enum is a single nullable column - no transformation needed
                node
            }
            ir::TyKind::Enum(_) => {
                // General enum: convert to single UNION column
                self.duck_export_row(node, ty)
            }
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Converts a row from "query repr" to "arrow repr".
    fn duck_export_row(&mut self, node: Node, ty: &ir::Ty) -> Node {
        let ty_mat = self.get_ty_mat(ty);

        match &ty_mat.kind {
            ir::TyKind::Primitive(_) => node,
            ir::TyKind::Tuple(ty_fields) => {
                // convert {_0, _1_0, _1_1} into {id, address: STRUCT[street, number]}

                let (rel_var, rel) = self.node_into_rel_var(node, ty);
                let mut columns: Vec<_> = self.rel_cols(ty_mat, true).collect();

                let mut res_projection = Vec::with_capacity(ty_fields.len() + 1);

                for (position, field) in ty_fields.iter().enumerate() {
                    let f_ty = &field.ty;

                    // take columns for this field
                    let col_count = self.rel_cols_ty_nested(f_ty).count();
                    let rem = columns.split_off(col_count);
                    let f_cols = std::mem::replace(&mut columns, rem);

                    let expr = self.duck_col_export(&rel_var, &f_cols, f_ty);

                    res_projection.push(sql_ast::SelectItem {
                        expr,
                        alias: Some(utils::new_ident(field_name(field, position))),
                    });
                }

                let mut select = utils::select_empty();
                select.projection = res_projection;
                select.from.extend(rel);
                Node::Select(select)
            }
            ir::TyKind::Array(_) => {
                // for nested arrays, query repr will already contain serialized items
                node
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                // Option enum is a single nullable column - no transformation needed
                node
            }
            ir::TyKind::Enum(_) => {
                // General enum: convert query repr columns to single UNION column
                let (rel_var, rel) = self.node_into_rel_var(node, ty);
                let cols: Vec<_> = self.rel_cols(ty_mat, true).collect();

                let expr = self.duck_col_export(&rel_var, &cols, ty_mat);

                let mut select = utils::select_empty();
                select.projection = vec![sql_ast::SelectItem {
                    expr,
                    alias: Some(utils::new_ident("value")),
                }];
                select.from.extend(rel);
                Node::Select(select)
            }
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }
}

pub(crate) fn field_name(field: &ir::TyTupleField, position: usize) -> String {
    match &field.name {
        Some(x) => x.clone(),
        None => format!("field{position}"),
    }
}
