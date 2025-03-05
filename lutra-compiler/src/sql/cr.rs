use lutra_bin::ir;

/// A relational expression (something that can be used in `FROM (...)`).
/// Its columns are dictated by its lutra type.
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
    FromTable(String),
    /// Read from a common table table
    FromBinding(String),
    /// Select all columns of a relational variable
    SelectRelVar,

    Limit(Box<RelExpr>, Expr),
    Offset(Box<RelExpr>, Expr),

    /// Projection that retains columns by position
    ProjectRetain(Box<RelExpr>, Vec<usize>),
    /// Projection that discards columns by position
    ProjectDrop(Box<RelExpr>, Vec<usize>),

    /// Projection that replaces all columns (but not the index)
    ProjectReplace(Box<RelExpr>, Vec<Expr>),

    Aggregate(Box<RelExpr>, Vec<Expr>),

    /// Filtering (also known as selection)
    Where(Box<RelExpr>, Expr),

    /// Sorting
    OrderBy(Box<RelExpr>, Expr),

    /// Bind a common table expression to a name
    With(String, Box<RelExpr>, Box<RelExpr>),
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
    Subquery(Box<RelExpr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}
