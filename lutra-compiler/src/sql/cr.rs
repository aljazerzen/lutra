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
    FromTable(String),
    /// Read from a common table table
    FromBinding(String),
    /// Select all columns of a relational variable
    SelectRelVar,

    Limit(Box<RelExpr>, Expr),
    Offset(Box<RelExpr>, Expr),

    /// Projection that just removes array index
    ProjectUnIndex(Box<RelExpr>),

    /// Projection that picks a column by position
    ProjectColumn(Box<RelExpr>, u16),

    /// Projection that replaces all columns
    ProjectReplace(Box<RelExpr>, Vec<Expr>),

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

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(ir::Literal),
    FuncCall(String, Vec<Expr>),
    Subquery(Box<RelExpr>),
    Ident(Vec<String>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}
