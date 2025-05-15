use lutra_bin::ir;

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Expression without relational inputs.
    From(From),

    /// Applies a relational transform.
    Transform(Box<BoundExpr>, Transform),

    Join(Box<BoundExpr>, Box<BoundExpr>, Option<Box<Expr>>),

    /// Bind a relation and evaluate an unrelated expression
    Bind(Box<BoundExpr>, Box<Expr>),

    /// Bind a relation and evaluate an correlated expression
    BindCorrelated(Box<BoundExpr>, Box<Expr>),

    /// Computes relations separately and concatenates them together
    #[allow(dead_code)]
    Union(Vec<Expr>),
}

/// An expression, bound to an identifier.
/// Such relations can be referred to by From::RelRef.
#[derive(Clone)]
pub struct BoundExpr {
    pub id: usize,
    pub rel: Expr,
}

#[derive(Debug, Clone)]
pub enum From {
    /// Relation with one row and many columns, where each cell can have
    /// a different expression.
    Row(Vec<Expr>),

    /// Read from a table (in Table representation)
    Table(String),

    /// Reference to a relation variable in scope.
    RelRef(usize),

    /// NULL
    Null,

    // A literal value
    Literal(ir::Literal),

    /// SQL query parameter. Contains 0-based index
    Param(u8),

    /// Call a function by its lutra name
    FuncCall(String, Vec<Expr>),

    /// Converts a JSON-encoded value into a relation
    JsonUnpack(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Transform {
    /// Projection that retains columns by position
    ProjectRetain(Vec<usize>),

    /// Projection that discards columns by position
    ProjectDiscard(Vec<usize>),

    Aggregate(Vec<Expr>),

    Window(Vec<Expr>),

    /// Filters by retaining only first N rows
    Limit(Box<Expr>),

    /// Filters by discarding first N rows
    Offset(Box<Expr>),

    /// Filtering (also known as selection)
    Where(Box<Expr>),

    /// Sorting of rows
    OrderBy(Box<Expr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)?;
        // f.write_str(": ")?;
        // self.ty.fmt(f)
        Ok(())
    }
}

impl std::fmt::Debug for BoundExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id.fmt(f)?;
        f.write_str(": ")?;
        self.rel.fmt(f)
    }
}

impl Expr {
    pub fn new_transform_preserve_ty(input: Expr, transform: Transform, id: usize) -> Self {
        Expr {
            ty: input.ty.clone(),
            kind: ExprKind::Transform(Box::new(BoundExpr { rel: input, id }), transform),
        }
    }
}
