use lutra_bin::ir;
use sqlparser::ast::{self as sql_ast, Query};

use crate::sql::utils;
use crate::sql::utils::ExprOrSource;

/// SQL expression and dependent relational variables
#[derive(Debug, Clone)]
pub struct Scoped {
    /// SQL expression that represents input CR node
    pub expr: ExprOrSource,

    /// Relations that need to be in scope for expr to make sense
    pub rel_vars: Vec<sql_ast::TableFactor>,
}

impl Scoped {
    pub fn new(rel_name: String, rel_vars: Vec<sql_ast::TableFactor>) -> Self {
        Self {
            expr: ExprOrSource::RelVar(rel_name),
            rel_vars,
        }
    }

    pub fn as_query(&self) -> Option<&sql_ast::Query> {
        let name = self.expr.as_rel_var()?;
        let rel_var = self
            .rel_vars
            .iter()
            .rev()
            .find(|r| utils::get_rel_alias(r) == name)?;
        utils::as_sub_rel(rel_var)
    }
    pub fn as_mut_query(&mut self) -> Option<&mut sql_ast::Query> {
        let name = self.expr.as_rel_var()?;
        let rel_var = self
            .rel_vars
            .iter_mut()
            .rev()
            .find(|r| utils::get_rel_alias(r) == name)?;
        utils::as_mut_sub_rel(rel_var)
    }

    pub fn as_row(&self) -> Option<&[sql_ast::SelectItem]> {
        if self.rel_vars.len() != 1 {
            return None;
        }
        let query = self.as_query()?;

        if query.limit.is_some() || query.order_by.is_some() || query.with.is_some() {
            return None;
        }
        let sql_ast::SetExpr::Select(select) = query.body.as_ref() else {
            return None;
        };
        if select.selection.is_some() || !select.from.is_empty() {
            return None;
        }
        Some(&select.projection)
    }
    pub fn as_simplified_expr(&self) -> Option<Scoped> {
        let row = self.as_row()?;
        if row.len() != 1 {
            return None;
        }
        Some(Scoped {
            expr: ExprOrSource::new_expr(utils::unwrap_select_item(row[0].clone())),
            rel_vars: vec![],
        })
    }

    pub fn merge_input(mut self, mut input: Scoped) -> Self {
        input
            .rel_vars
            .extend(self.rel_vars.into_iter().map(utils::lateral));
        self.rel_vars = input.rel_vars;
        self
    }
}

impl From<ExprOrSource> for Scoped {
    fn from(expr: ExprOrSource) -> Self {
        Scoped {
            expr,
            rel_vars: Vec::new(),
        }
    }
}

impl<'a> crate::sql::queries::Context<'a> {
    pub fn scoped_into_expr(&self, expr: Scoped, ty: &ir::Ty) -> ExprOrSource {
        if expr.rel_vars.is_empty()
            && matches!(expr.expr, ExprOrSource::Expr(_) | ExprOrSource::Source(_))
        {
            return expr.expr;
        }

        ExprOrSource::new_expr(sql_ast::Expr::Subquery(Box::new(
            self.scoped_into_query(expr, ty),
        )))
    }

    pub fn query_into_scoped(&mut self, query: Query) -> Scoped {
        let name = self.rel_name_gen.next();
        Scoped::new(name.clone(), vec![utils::sub_rel(query, name)])
    }

    pub fn wrap_scoped(&mut self, rel: Scoped, rel_ty: &ir::Ty) -> Scoped {
        self.query_into_scoped(self.scoped_into_query(rel, rel_ty))
    }

    pub fn scoped_into_query(&self, scope: Scoped, ty: &ir::Ty) -> sql_ast::Query {
        self.scoped_into_query_ext(scope, ty, true)
    }

    pub fn scoped_into_query_ext(
        &self,
        scoped: Scoped,
        ty: &ir::Ty,
        include_index: bool,
    ) -> sql_ast::Query {
        let mut select = utils::select_empty();

        match scoped.expr {
            ExprOrSource::Expr(_) | ExprOrSource::Source(_) => {
                select.projection = vec![sql_ast::SelectItem::ExprWithAlias {
                    expr: scoped.expr.into_expr(),
                    alias: sql_ast::Ident::new("value"),
                }];
            }
            ExprOrSource::RelVar(rvar_name) => {
                select.projection = self.projection_noop(Some(&rvar_name), ty, include_index);
            }
        }

        select
            .from
            .extend(scoped.rel_vars.into_iter().map(utils::from));
        utils::query_select(select)
    }

    pub fn scoped_as_mut_select<'q>(
        &mut self,
        rel: &'q mut Scoped,
        rel_ty: &ir::Ty,
    ) -> &'q mut sql_ast::Select {
        self.scoped_as_mut_select_that(rel, rel_ty, |_| true)
    }

    pub fn scoped_as_mut_select_that<'q>(
        &mut self,
        rel: &'q mut Scoped,
        rel_ty: &ir::Ty,
        is_valid: impl Fn(&sql_ast::Select) -> bool,
    ) -> &'q mut sql_ast::Select {
        let is_valid = rel.as_query().is_some_and(|query| {
            if let sql_ast::SetExpr::Select(select) = query.body.as_ref() {
                is_valid(select)
            } else {
                false
            }
        });

        // if query is not a valid select
        if !is_valid {
            // take the query
            let dummy = Scoped {
                expr: ExprOrSource::Source(String::new()),
                rel_vars: vec![],
            };
            let original = std::mem::replace(rel, dummy);

            // wrap it as a subquery
            let wrapped = self.wrap_scoped(original, rel_ty);

            // place it back
            let _dummy = std::mem::replace(rel, wrapped);
        }

        let query = rel.as_mut_query().unwrap();
        let select = match query.body.as_mut() {
            sql_ast::SetExpr::Select(select) => select,
            set_expr => {
                unreachable!("expected SetExpr::Select, found: {set_expr:?}");
            }
        };
        select.as_mut()
    }
}
