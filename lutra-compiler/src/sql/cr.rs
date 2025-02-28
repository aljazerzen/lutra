use lutra_bin::ir;

#[derive(Debug, Clone)]
pub struct RelExpr {
    pub kind: RelExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone)]
pub enum RelExprKind {
    /// Relation with a single row, single col, representing a single value
    Literal(Expr),

    /// Relation with a single row, representing a tuple
    Tuple(Vec<Expr>),

    /// Relation with many rows and many columns, where each cell can have
    /// a different expression.
    Array(Vec<Vec<Expr>>),

    /// Read from a table
    From(String),

    Limit(Box<RelExpr>, Expr),
    Offset(Box<RelExpr>, Expr),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(ir::Literal),
    BinOp(Box<Expr>, String, Box<Expr>),
}
