use lutra_bin::ir;

#[derive(Clone)]
pub struct RelExpr {
    pub kind: RelExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone)]
pub enum RelExprKind {
    /// Expression that produces a relation, without relational inputs.
    From(From),

    Transform(Box<RelExpr>, Transform),

    #[allow(dead_code)]
    Join(Box<RelExpr>, Box<RelExpr>),

    /// Evaluate body for each row in iterator. Return union of rows of each iteration.
    ForEach(Box<RelExpr>, Box<RelExpr>),

    /// Bind a common table expression to a name
    // TODO: this should probably be RelExprKind
    Bind(String, Box<RelExpr>, Box<RelExpr>),
}

#[derive(Debug, Clone)]
pub enum From {
    /// Relation with many rows and many columns, where each cell can have
    /// a different expression.
    Construction(Vec<Vec<ColExpr>>),

    /// Read from a table (in Table representation)
    Table(String),

    /// Read from a CTE (in RelExpr representation)
    Binding(String),

    /// Reference to iterator of [RelExprKind::ForEach]
    ForIterator,

    /// Reference to the input of the enclosing transform
    TransformIterator,
}

#[derive(Debug, Clone)]
pub enum Transform {
    /// Projection that replaces all columns (but not the index)
    /// Each column expression is evaluated for each row of the input.
    Project(Vec<ColExpr>),

    /// Projection that retains columns by position
    ProjectRetain(Vec<usize>),

    /// Projection that discards columns by position
    ProjectDiscard(Vec<usize>),

    Aggregate(Vec<ColExpr>),

    /// Filters by retaining only first N rows
    Limit(ColExpr),

    /// Filters by discarding first N rows
    Offset(ColExpr),

    /// Filtering (also known as selection)
    Where(ColExpr),

    /// Sorting or rows
    OrderBy(ColExpr),

    /// Converts a JSON-encoded value into a relation
    JsonUnpack(Box<ColExpr>),
}

#[derive(Clone)]
pub struct ColExpr {
    pub kind: ColExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone)]
pub enum ColExprKind {
    Null,

    // A literal value
    Literal(ir::Literal),

    /// SQL query parameter. Contains 0-based index
    Param(u8),

    /// A column of the input of the enclosing transform
    InputRelCol(usize),

    /// Call a function by its lutra name
    FuncCall(String, Vec<ColExpr>),

    /// A relational subquery
    Subquery(Box<RelExpr>),
}

impl std::fmt::Debug for RelExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::fmt::Debug for ColExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl RelExpr {
    pub fn new_transform_preserve_ty(input: RelExpr, transform: Transform) -> Self {
        RelExpr {
            ty: input.ty.clone(),
            kind: RelExprKind::Transform(Box::new(input), transform),
        }
    }
}

impl ColExpr {
    pub fn new_subquery(subquery: RelExpr) -> Self {
        ColExpr {
            ty: subquery.ty.clone(),
            kind: ColExprKind::Subquery(Box::new(subquery)),
        }
    }
    pub fn new_rel_col(col_position: usize, col_ty: ir::Ty) -> ColExpr {
        ColExpr {
            kind: ColExprKind::InputRelCol(col_position),
            ty: col_ty,
        }
    }
}
