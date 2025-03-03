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
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(ir::Literal),
    FuncCall(String, Vec<Expr>),
}
