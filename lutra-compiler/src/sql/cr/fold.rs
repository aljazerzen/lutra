use super::*;

type Result<T, E = ()> = std::result::Result<T, E>;

pub trait CrFold {
    fn fold_expr(&mut self, expr: Expr) -> Result<Expr> {
        fold_expr(self, expr)
    }
    fn fold_expr_kind(&mut self, kind: ExprKind, ty: ir::Ty) -> Result<Expr> {
        fold_expr_kind(self, kind, ty)
    }
    fn fold_bound_expr(&mut self, bound: BoundExpr) -> Result<BoundExpr> {
        fold_bound_expr(self, bound)
    }
    fn fold_from(&mut self, from: From, ty: ir::Ty) -> Result<Expr> {
        fold_from(self, from, ty)
    }
    fn fold_transform(&mut self, transform: Transform) -> Result<Transform> {
        fold_transform(self, transform)
    }
}

pub fn fold_expr<T: ?Sized + CrFold>(fold: &mut T, expr: Expr) -> Result<Expr> {
    fold.fold_expr_kind(expr.kind, expr.ty)
}

pub fn fold_exprs<T: ?Sized + CrFold>(fold: &mut T, exprs: Vec<Expr>) -> Result<Vec<Expr>> {
    exprs.into_iter().map(|e| fold.fold_expr(e)).collect()
}

pub fn fold_option<T: ?Sized + CrFold>(
    fold: &mut T,
    opt: Option<Box<Expr>>,
) -> Result<Option<Box<Expr>>> {
    opt.map(|e| fold.fold_expr(*e).map(Box::new)).transpose()
}

pub fn fold_expr_kind<T: ?Sized + CrFold>(
    fold: &mut T,
    kind: ExprKind,
    ty: ir::Ty,
) -> Result<Expr> {
    let kind = match kind {
        ExprKind::From(from) => return fold.fold_from(from, ty),
        ExprKind::Transform(bound, transform) => ExprKind::Transform(
            Box::new(fold.fold_bound_expr(*bound)?),
            fold.fold_transform(transform)?,
        ),
        ExprKind::Join(left, right, cond) => ExprKind::Join(
            Box::new(fold.fold_bound_expr(*left)?),
            Box::new(fold.fold_bound_expr(*right)?),
            fold_option(fold, cond)?,
        ),
        ExprKind::Bind(bound, expr) => ExprKind::Bind(
            Box::new(fold.fold_bound_expr(*bound)?),
            Box::new(fold.fold_expr(*expr)?),
        ),
        ExprKind::BindCorrelated(bound, expr) => ExprKind::BindCorrelated(
            Box::new(fold.fold_bound_expr(*bound)?),
            Box::new(fold.fold_expr(*expr)?),
        ),
        ExprKind::Union(exprs) => ExprKind::Union(fold_exprs(fold, exprs)?),
        ExprKind::Iteration(initial, step) => ExprKind::Iteration(
            Box::new(fold.fold_expr(*initial)?),
            Box::new(fold.fold_bound_expr(*step)?),
        ),
    };
    Ok(Expr { kind, ty })
}

pub fn fold_bound_expr<T: ?Sized + CrFold>(fold: &mut T, bound: BoundExpr) -> Result<BoundExpr> {
    Ok(BoundExpr {
        id: bound.id,
        rel: fold.fold_expr(bound.rel)?,
    })
}

pub fn fold_from<T: ?Sized + CrFold>(fold: &mut T, from: From, ty: ir::Ty) -> Result<Expr> {
    let from = match from {
        From::Row(exprs) => From::Row(fold_exprs(fold, exprs)?),
        From::Table(name) => From::Table(name),
        From::RelRef(id) => From::RelRef(id),
        From::Null => From::Null,
        From::Literal(lit) => From::Literal(lit),
        From::Param(i) => From::Param(i),
        From::FuncCall(name, args) => From::FuncCall(name, fold_exprs(fold, args)?),
        From::Deserialize(expr) => From::Deserialize(Box::new(fold.fold_expr(*expr)?)),
        From::Serialize(expr) => From::Serialize(Box::new(fold.fold_expr(*expr)?)),
        From::Case(arms) => From::Case(
            arms.into_iter()
                .map(|(c, v)| Ok((fold.fold_expr(c)?, fold.fold_expr(v)?)))
                .collect::<Result<_>>()?,
        ),
        From::SQLSource(s) => From::SQLSource(s),
    };
    Ok(Expr {
        kind: ExprKind::From(from),
        ty,
    })
}

pub fn fold_transform<T: ?Sized + CrFold>(fold: &mut T, transform: Transform) -> Result<Transform> {
    Ok(match transform {
        Transform::ProjectPick(cols) => Transform::ProjectPick(cols),
        Transform::ProjectDiscard(cols) => Transform::ProjectDiscard(cols),
        Transform::Aggregate(exprs) => Transform::Aggregate(fold_exprs(fold, exprs)?),
        Transform::Limit(expr) => Transform::Limit(Box::new(fold.fold_expr(*expr)?)),
        Transform::Offset(expr) => Transform::Offset(Box::new(fold.fold_expr(*expr)?)),
        Transform::Where(expr) => Transform::Where(Box::new(fold.fold_expr(*expr)?)),
        Transform::IndexBy(expr) => Transform::IndexBy(fold_option(fold, expr)?),
        Transform::Order => Transform::Order,
        Transform::Group { key, values } => Transform::Group {
            key: Box::new(fold.fold_expr(*key)?),
            values: fold_exprs(fold, values)?,
        },
        Transform::Insert(name) => Transform::Insert(name),
    })
}
