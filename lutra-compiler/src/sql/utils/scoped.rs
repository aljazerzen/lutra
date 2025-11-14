use std::fmt::Write;

use lutra_bin::ir;
use sql_ast::Query;

use crate::sql::utils::{self, RelCols};

/// SQL expression and dependent relational variables
#[derive(Clone, strum::AsRefStr)]
pub enum Node {
    /// A single column.
    /// Includes things that can be used in a WHERE clause.
    /// For example: `TRUE`, `my_table.id`, `(select id from my_table)`
    /// Might need certain relations in scope.
    Column {
        expr: Box<sql_ast::Expr>,
        rels: Vec<sql_ast::RelNamed>,
    },

    /// Many columns.
    /// Includes things that can be used in GROUP BY.
    /// For example: `[TRUE, my_table.id, (select id from my_table)]`
    /// Might need certain relations in scope.
    Columns {
        exprs: Vec<sql_ast::Expr>,
        rels: Vec<sql_ast::RelNamed>,
    },

    /// A relational expression.
    /// Includes things that can be used in FROM.
    /// For example: `my_table AS r1`, `(select a, b from my_table) AS r2`
    Rel(sql_ast::RelNamed),

    /// A relation variable - a reference to a relation that affects cardinality
    /// of current evaluation context.
    /// Includes things that can be used to prefix column references.
    /// For example, `r1` (as in `r1.id`).
    RelVar(String),

    /// A select query.
    Select(sql_ast::Select),

    /// A query.
    Query(sql_ast::Query),

    /// Direct SQL source.
    // This has to be a variant, because we don't (yet) know what it
    // contains - it could be a column or a query.
    Source(String),
}

impl Node {
    pub fn dummy() -> Self {
        Self::RelVar(String::new())
    }

    pub fn as_columns(&self) -> Option<&[sql_ast::SelectItem]> {
        match self {
            Node::Select(select)
                if select.distinct.is_none()
                    && select.from.is_empty()
                    && select.having.is_none()
                    && select.selection.is_none()
                    && select.sort_by.is_empty()
                    && select.into.is_none() =>
            {
                Some(select.projection.as_slice())
            }
            _ => None,
        }
    }
}

impl From<sql_ast::Expr> for Node {
    fn from(expr: sql_ast::Expr) -> Self {
        Node::Column {
            expr: Box::new(expr),
            rels: vec![],
        }
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_ref())?;
        f.write_str("(")?;

        fn fmt_rels(
            f: &mut std::fmt::Formatter<'_>,
            rels: &Vec<sql_ast::RelNamed>,
        ) -> Result<(), std::fmt::Error> {
            for rvar in rels {
                f.write_str("WITH {")?;
                f.write_str(&rvar.to_string())?;
                f.write_str("}")?;
                if f.alternate() {
                    f.write_char('\n')?;
                } else {
                    f.write_char(' ')?;
                }
            }
            Ok(())
        }

        match self {
            Node::Column { expr, rels } => {
                fmt_rels(f, rels)?;
                f.write_str(&expr.to_string())?;
            }
            Node::Columns { exprs, rels } => {
                fmt_rels(f, rels)?;
                f.write_char('[')?;
                for e in exprs {
                    f.write_str(&e.to_string())?;
                    if f.alternate() {
                        f.write_str(",\n")?;
                    } else {
                        f.write_str(", ")?;
                    }
                }
                f.write_char(']')?;
            }
            Node::Rel(rel_var) => f.write_str(&rel_var.to_string())?,
            Node::RelVar(rel_var) => f.write_str(rel_var)?,
            Node::Select(select) => f.write_str(&select.to_string())?,
            Node::Query(query) => f.write_str(&query.to_string())?,
            Node::Source(source) => f.write_str(source)?,
        }
        f.write_char(')')
    }
}

impl<'a> crate::sql::queries::Context<'a> {
    pub fn query_into_rel(&mut self, query: sql_ast::Query) -> sql_ast::RelNamed {
        let rel_var = self.rel_name_gen.next();
        utils::sub_rel(query, rel_var)
    }

    pub fn rel_into_select(
        &mut self,
        rel: sql_ast::RelNamed,
        ty: &ir::Ty,
        include_index: bool,
    ) -> sql_ast::Select {
        let rel_var = utils::get_rel_alias(&rel);

        let mut select = utils::select_empty();
        select.projection = self.projection_noop(rel_var, ty, include_index);
        select.from = vec![rel];
        select
    }

    pub fn rel_var_into_columns(
        &mut self,
        rel_var: Option<&str>,
        ty: &ir::Ty,
        include_index: bool,
    ) -> Vec<sql_ast::Expr> {
        self.rel_cols(ty, include_index)
            .map(|c_name| utils::identifier(rel_var, c_name))
            .collect()
    }

    pub fn rel_var_into_select(
        &mut self,
        rel_var: &str,
        ty: &ir::Ty,
        include_index: bool,
    ) -> sql_ast::Select {
        let mut select = utils::select_empty();
        select.projection = self.projection_noop(Some(rel_var), ty, include_index);
        select
    }

    pub fn wrap_node(&mut self, rel: Node, rel_ty: &ir::Ty) -> Node {
        let select = self.node_into_select(rel, rel_ty);
        Node::Query(utils::query_select(select))
    }

    pub fn wrap_query(&mut self, query: Query, ty: &ir::Ty) -> Query {
        let rel = self.query_into_rel(query);
        let select = self.rel_into_select(rel, ty, true);
        utils::query_select(select)
    }

    pub fn node_into_column_and_rels(
        &mut self,
        scoped: Node,
        ty: &ir::Ty,
    ) -> (sql_ast::Expr, Vec<sql_ast::RelNamed>) {
        match scoped {
            Node::Column {
                expr,
                rels: rel_vars,
            } => (*expr, rel_vars),
            Node::Columns {
                exprs,
                rels: rel_vars,
            } if exprs.len() == 1 => (exprs.into_iter().next().unwrap(), rel_vars),
            Node::Source(source) => (sql_ast::Expr::Source(source), vec![]),
            _ => {
                let rel = self.node_into_rel(scoped, ty);

                let rel_var = utils::get_rel_alias(&rel).unwrap();
                let c_name = self.rel_cols(ty, true).next().unwrap();
                let expr = utils::identifier(Some(rel_var), c_name);

                (expr, vec![rel])
            }
        }
    }

    pub fn node_into_column(&mut self, scoped: Node, ty: &ir::Ty) -> sql_ast::Expr {
        match scoped {
            Node::Column { expr, rels } if rels.is_empty() => *expr,
            Node::Columns { exprs, rels } if exprs.len() == 1 && rels.is_empty() => {
                exprs.into_iter().next().unwrap()
            }
            Node::Source(source) => sql_ast::Expr::Source(source),
            Node::RelVar(ref rel_name) => {
                let cols = self.rel_var_into_columns(Some(rel_name), ty, true);
                if cols.len() == 1 {
                    cols.into_iter().next().unwrap()
                } else {
                    sql_ast::Expr::Subquery(Box::new(self.node_into_query(scoped, ty)))
                }
            }
            _ => sql_ast::Expr::Subquery(Box::new(self.node_into_query(scoped, ty))),
        }
    }

    pub fn node_into_columns(&mut self, scoped: Node, ty: &ir::Ty) -> Vec<sql_ast::Expr> {
        match scoped {
            Node::Column { expr, rels } if rels.is_empty() => vec![*expr],
            Node::Columns { exprs, rels } if rels.is_empty() => exprs,
            Node::Source(source) => vec![sql_ast::Expr::Source(source)],
            Node::RelVar(rel_name) => self.rel_var_into_columns(Some(&rel_name), ty, true),
            _ => {
                vec![sql_ast::Expr::Subquery(Box::new(
                    self.node_into_query(scoped, ty),
                ))]
            }
        }
    }

    pub fn node_into_columns_and_rels(
        &mut self,
        scoped: Node,
        ty: &ir::Ty,
    ) -> (Vec<sql_ast::Expr>, Vec<sql_ast::RelNamed>) {
        match scoped {
            Node::Column { expr, rels } => (vec![*expr], rels),
            Node::Columns { exprs, rels } => (exprs, rels),
            Node::Source(source) => (vec![sql_ast::Expr::Source(source)], vec![]),
            Node::RelVar(rel_name) => {
                (self.rel_var_into_columns(Some(&rel_name), ty, true), vec![])
            }
            _ => {
                let rel = self.node_into_rel(scoped, ty);
                let rel_var = utils::get_rel_alias(&rel);
                (self.rel_var_into_columns(rel_var, ty, true), vec![rel])
            }
        }
    }

    pub fn node_into_rel(&mut self, node: Node, ty: &ir::Ty) -> sql_ast::RelNamed {
        match node {
            Node::Rel(rel) => rel,
            Node::Query(query) => self.query_into_rel(query),
            Node::Source(source) => {
                self.query_into_rel(utils::query_new(sql_ast::SetExpr::Source(source)))
            }
            _ => {
                let rel_name = self.rel_name_gen.next();
                let select = self.node_into_select(node, ty);
                utils::sub_rel(utils::query_select(select), rel_name)
            }
        }
    }

    pub fn node_into_rel_var(
        &mut self,
        node: Node,
        ty: &ir::Ty,
    ) -> (String, Option<sql_ast::RelNamed>) {
        match node {
            Node::RelVar(name) => (name, None),
            _ => {
                let rel = self.node_into_rel(node, ty);
                let name = utils::get_rel_alias(&rel).unwrap().to_string();
                (name, Some(rel))
            }
        }
    }

    pub fn node_into_select(&mut self, node: Node, ty: &ir::Ty) -> sql_ast::Select {
        match node {
            Node::Column {
                expr,
                rels: rel_vars,
            } => {
                let mut select = utils::select_empty();
                select.projection = self.projection(ty, [*expr]);
                select.from = rel_vars;
                select
            }
            Node::Columns {
                exprs,
                rels: rel_vars,
            } => {
                let mut select = utils::select_empty();
                select.projection = self.projection(ty, exprs);
                select.from = rel_vars;
                select
            }
            Node::Rel(rel) => self.rel_into_select(rel, ty, true),
            Node::RelVar(rel_var) => self.rel_var_into_select(&rel_var, ty, true),
            Node::Select(select) => select,
            Node::Query(query) => {
                // try unwrapping Query
                if query.limit.is_some()
                    && query.offset.is_some()
                    && query.order_by.is_some()
                    && query.with.is_some()
                    && let sql_ast::SetExpr::Select(select) = *query.body
                {
                    return *select;
                }

                // wrap into sub rel
                let rel = self.query_into_rel(query);
                self.rel_into_select(rel, ty, true)
            }
            Node::Source(_) => {
                // assume source is a query, wrap it into sub rel
                let rel = self.node_into_rel(node, ty);
                self.rel_into_select(rel, ty, true)
            }
        }
    }

    pub fn node_without_index(&mut self, node: Node, ty: &ir::Ty) -> sql_ast::Select {
        match node {
            Node::Column { .. } => self.node_into_select(node, ty),
            Node::RelVar(rel_var) => self.rel_var_into_select(&rel_var, ty, false),

            _ => {
                let rel = self.node_into_rel(node, ty);
                self.rel_into_select(rel, ty, false)
            }
        }
    }

    pub fn node_into_query(&mut self, node: Node, ty: &ir::Ty) -> sql_ast::Query {
        match node {
            Node::Query(query) => query,
            _ => utils::query_select(self.node_into_select(node, ty)),
        }
    }
}
