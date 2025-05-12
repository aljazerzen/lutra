use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::utils;
use crate::sql::utils::ExprOrSource;

/// SQL relational expression, in a scope of possibly many relational variables.
#[derive(Debug)]
pub struct RelScoped {
    pub rel_name: String,

    pub rel_vars: Vec<sql_ast::TableFactor>,
}

impl RelScoped {
    pub fn new(rel_factor: sql_ast::TableFactor) -> Self {
        Self {
            rel_name: utils::get_rel_alias(&rel_factor).to_string(),
            rel_vars: vec![rel_factor],
        }
    }
    pub fn push(&mut self, rel_factor: sql_ast::TableFactor) {
        self.rel_name = utils::get_rel_alias(&rel_factor).to_string();
        self.rel_vars.push(rel_factor);
    }
    pub fn get_name(&self) -> &str {
        &self.rel_name
    }
    pub fn into_tables_with_joins(self) -> impl Iterator<Item = sql_ast::TableWithJoins> {
        self.rel_vars.into_iter().map(utils::from)
    }

    pub fn as_sub_rel(&self) -> Option<&sql_ast::Query> {
        let last = self.rel_vars.last()?;
        if utils::get_rel_alias(last) != self.rel_name {
            return None;
        }
        utils::as_sub_rel(last)
    }
    pub fn as_mut_sub_rel(&mut self) -> Option<&mut sql_ast::Query> {
        let last = self.rel_vars.last_mut()?;
        if utils::get_rel_alias(last) != self.rel_name {
            return None;
        }
        utils::as_mut_sub_rel(last)
    }
    pub fn into_sub_rel(mut self) -> Result<sql_ast::Query, Self> {
        if self.rel_vars.is_empty() {
            return Err(self);
        }
        let last = self.rel_vars.last().unwrap();
        if utils::get_rel_alias(last) != self.rel_name {
            return Err(self);
        }
        if !matches!(last, sql_ast::TableFactor::Derived { .. }) {
            return Err(self);
        }
        let sql_ast::TableFactor::Derived { subquery, .. } = self.rel_vars.pop().unwrap() else {
            panic!()
        };
        Ok(*subquery)
    }
}

/// SQL column expression, in a scope of possibly many relational variables.
pub struct ExprScoped {
    pub expr: ExprOrSource,

    pub rel_vars: Vec<sql_ast::TableFactor>,
}

impl ExprScoped {
    pub fn new(expr: ExprOrSource) -> Self {
        Self {
            expr,
            rel_vars: vec![],
        }
    }
    pub fn into_subquery(self) -> sql_ast::Expr {
        if self.rel_vars.is_empty() {
            return self.expr.into_expr();
        }
        let mut select = utils::select_empty();
        select.projection = vec![sql_ast::SelectItem::UnnamedExpr(self.expr.into_expr())];
        select
            .from
            .extend(self.rel_vars.into_iter().map(utils::from));

        sql_ast::Expr::Subquery(Box::new(utils::query_select(select)))
    }
}

impl<'a> crate::sql::queries::Context<'a> {
    pub fn wrap_rel(&mut self, rel: RelScoped, rel_ty: &ir::Ty) -> RelScoped {
        RelScoped::new(utils::sub_rel(
            self.wrap_rel_into_query(rel, rel_ty),
            Some(self.rel_name_gen.next()),
        ))
    }

    pub fn wrap_rel_into_query(&self, scope: RelScoped, ty: &ir::Ty) -> sql_ast::Query {
        let mut select = utils::select_empty();
        select.projection = self.projection_noop(Some(scope.get_name()), ty, true);
        select.from.extend(scope.into_tables_with_joins());
        utils::query_select(select)
    }

    pub fn wrap_rel_into_query_without_index(
        &self,
        scope: RelScoped,
        ty: &ir::Ty,
    ) -> sql_ast::Query {
        let mut select = utils::select_empty();
        select.projection = self.projection_noop(Some(scope.get_name()), ty, false);
        select.from.extend(scope.into_tables_with_joins());
        utils::query_select(select)
    }

    pub fn rel_as_mut_select<'q>(
        &mut self,
        rel: &'q mut RelScoped,
        rel_ty: &ir::Ty,
    ) -> &'q mut sql_ast::Select {
        self.rel_as_mut_select_that(rel, rel_ty, |_| true)
    }

    pub fn rel_as_mut_select_that<'q>(
        &mut self,
        rel: &'q mut RelScoped,
        rel_ty: &ir::Ty,
        is_valid: impl Fn(&sql_ast::Select) -> bool,
    ) -> &'q mut sql_ast::Select {
        let is_valid = rel.as_sub_rel().is_some_and(|query| {
            if let sql_ast::SetExpr::Select(select) = query.body.as_ref() {
                is_valid(select)
            } else {
                false
            }
        });

        // if query is not a valid select
        if !is_valid {
            // take the query
            let dummy = RelScoped {
                rel_name: "".into(),
                rel_vars: vec![],
            };
            let original = std::mem::replace(rel, dummy);

            // wrap it as a subquery
            let wrapped = self.wrap_rel(original, rel_ty);

            // place it back
            let _dummy = std::mem::replace(rel, wrapped);
        }

        let query = rel.as_mut_sub_rel().unwrap();
        let select = match query.body.as_mut() {
            sql_ast::SetExpr::Select(select) => select,
            set_expr => {
                unreachable!("expected SetExpr::Select, found: {set_expr:?}");
            }
        };
        select.as_mut()
    }
}
