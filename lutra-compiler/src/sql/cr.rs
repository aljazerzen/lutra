use lutra_bin::ir;

#[derive(Debug, Clone)]
pub struct RelExpr {
    pub kind: RelExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone)]
pub enum RelExprKind {
    /// Relation with many rows and many columns, where each cell can have
    /// a different expression.
    Constructed(Vec<Vec<Expr>>),

    /// Read from a table
    From(String),

    Limit(Box<RelExpr>, Expr),
    Offset(Box<RelExpr>, Expr),
    ProjectUnIndex(Box<RelExpr>),
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(ir::Literal),
    FuncCall(String, Vec<Expr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}
