use std::fmt::Write;

use lutra_bin::ir;

use crate::sql::COL_VALUE;
use crate::sql::utils::ExprOrRelVar;
use crate::sql::utils::{self, RelCols};

/// SQL expression and dependent relational variables
#[derive(Clone)]
pub struct Scoped {
    /// SQL expression that represents input CR node
    pub expr: ExprOrRelVar,

    /// Relations that need to be in scope for expr to make sense
    pub rel_vars: Vec<sql_ast::RelVar>,
}

impl Scoped {
    pub fn new(rel_name: String, rel_vars: Vec<sql_ast::RelVar>) -> Self {
        Self {
            expr: ExprOrRelVar::RelVar(rel_name),
            rel_vars,
        }
    }

    pub fn remove_rvar(&mut self, name: &str) -> Option<sql_ast::RelVar> {
        let (p, _) = self
            .rel_vars
            .iter()
            .enumerate()
            .rev()
            .find(|(_, r)| utils::get_rel_alias(r) == name)?;
        Some(self.rel_vars.remove(p))
    }
    pub fn as_query(&self) -> Option<&sql_ast::Query> {
        if self.rel_vars.len() != 1 {
            return None;
        }

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
            expr: ExprOrRelVar::new_expr(row[0].expr.clone()),
            rel_vars: vec![],
        })
    }

    pub fn merge_lateral(mut self, mut before: Scoped) -> Self {
        before
            .rel_vars
            .extend(self.rel_vars.into_iter().map(utils::lateral));
        self.rel_vars = before.rel_vars;
        self
    }
}

impl From<ExprOrRelVar> for Scoped {
    fn from(expr: ExprOrRelVar) -> Self {
        Scoped {
            expr,
            rel_vars: Vec::new(),
        }
    }
}
impl From<sql_ast::Expr> for Scoped {
    fn from(expr: sql_ast::Expr) -> Self {
        Scoped {
            expr: ExprOrRelVar::Expr(Box::new(expr)),
            rel_vars: Vec::new(),
        }
    }
}

impl std::fmt::Debug for Scoped {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for rvar in &self.rel_vars {
            f.write_str("WITH {")?;
            rvar.fmt(f)?;
            f.write_str("}")?;
            if f.alternate() {
                f.write_char('\n')?;
            } else {
                f.write_char(' ')?;
            }
        }
        self.expr.fmt(f)?;
        Ok(())
    }
}

impl<'a> crate::sql::queries::Context<'a> {
    pub fn query_into_scoped(&mut self, query: sql_ast::Query) -> Scoped {
        let name = self.rel_name_gen.next();
        Scoped::new(name.clone(), vec![utils::sub_rel(query, name)])
    }

    pub fn wrap_scoped(&mut self, rel: Scoped, rel_ty: &ir::Ty) -> Scoped {
        self.query_into_scoped(self.scoped_into_query(rel, rel_ty))
    }

    pub fn scoped_into_query(&self, scoped: Scoped, ty: &ir::Ty) -> sql_ast::Query {
        self.scoped_into_query_ext(scoped, ty, true)
    }

    pub fn scoped_into_query_ext(
        &self,
        scoped: Scoped,
        ty: &ir::Ty,
        include_index: bool,
    ) -> sql_ast::Query {
        let mut select = utils::select_empty();

        self.expr_into_query(scoped.expr, &mut select, ty, include_index);

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
                expr: ExprOrRelVar::new(String::new()),
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
    pub fn scoped_as_rel_var<'s>(&mut self, scoped: &'s mut Scoped, ty: &ir::Ty) -> &'s str {
        let is_rel_var = scoped.expr.as_rel_var().is_some();
        if !is_rel_var {
            let rel_name = self.rel_name_gen.next();

            let expr = std::mem::replace(&mut scoped.expr, ExprOrRelVar::RelVar(rel_name.clone()));

            let mut select = utils::select_empty();
            self.expr_into_query(expr, &mut select, ty, true);

            let query = utils::query_select(select);

            scoped
                .rel_vars
                .push(utils::sub_rel(query, rel_name.clone()));
        }
        scoped.expr.as_rel_var().unwrap()
    }

    fn expr_into_query(
        &self,
        expr: ExprOrRelVar,
        select: &mut sql_ast::Select,
        ty: &ir::Ty,
        include_index: bool,
    ) {
        match expr {
            ExprOrRelVar::Expr(expr) => {
                let single_column = self.rel_cols(ty, true).nth(1).is_none();
                if single_column {
                    select.projection = vec![sql_ast::SelectItem {
                        expr: *expr,
                        alias: Some(utils::new_ident(COL_VALUE)),
                    }];
                } else {
                    let s = expr.to_string();
                    let s = s.strip_prefix("(").unwrap().strip_suffix(")").unwrap();
                    select.from.push(utils::from(utils::sub_rel(
                        utils::query_new(sql_ast::SetExpr::Source(s.to_string())),
                        COL_VALUE.into(),
                    )));
                    select.projection = self.projection_noop(None, ty, include_index);
                }
            }
            ExprOrRelVar::RelVar(rvar_name) => {
                select.projection = self.projection_noop(Some(&rvar_name), ty, include_index);
            }
        }
    }
}
