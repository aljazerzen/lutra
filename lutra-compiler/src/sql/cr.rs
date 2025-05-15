use lutra_bin::ir;

use crate::utils::IdGenerator;

#[derive(Clone)]
pub struct RelExpr {
    pub id: usize,
    pub kind: RelExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone)]
pub enum RelExprKind {
    /// Expression that produces a relation, without relational inputs.
    From(From),

    /// Applies a relational transform.
    /// Introduces iterator over input relation for the scope of this transform.
    Transform(Box<RelExpr>, Transform),

    Join(Box<RelExpr>, Box<RelExpr>, Option<Box<ColExpr>>),

    /// Bind a relation and evaluate an unrelated expression
    Bind(Box<RelExpr>, Box<RelExpr>),

    /// Bind a relation and evaluate an correlated expression
    BindCorrelated(Box<RelExpr>, Box<RelExpr>),

    /// Computes relations separately and concatenates them together
    #[allow(dead_code)]
    Union(Vec<RelExpr>),
}

#[derive(Debug, Clone)]
pub enum From {
    /// Relation with many rows and many columns, where each cell can have
    /// a different expression.
    Construction(Vec<Vec<ColExpr>>),

    /// Read from a table (in Table representation)
    Table(String),

    /// Read from a CTE (in RelExpr representation)
    Binding(usize),

    /// Reference to a relation variable in scope.
    RelRef(usize),
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
    JsonUnpack,
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
    InputRelCol(usize, usize),

    /// Call a function by its lutra name
    FuncCall(String, Vec<ColExpr>),

    /// A relational subquery
    Subquery(Box<RelExpr>),
}

impl std::fmt::Debug for RelExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id.fmt(f)?;
        f.write_str(": ")?;
        self.kind.fmt(f)
    }
}

impl std::fmt::Debug for ColExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl RelExpr {
    pub fn new_transform_preserve_ty(input: RelExpr, transform: Transform, id: usize) -> Self {
        RelExpr {
            ty: input.ty.clone(),
            id,
            kind: RelExprKind::Transform(Box::new(input), transform),
        }
    }

    pub fn new_rel_col(
        rel_id: usize,
        rel_ty: ir::Ty,
        col_position: usize,
        col_ty: ir::Ty,
        id_gen: &mut IdGenerator<usize>,
    ) -> Self {
        RelExpr {
            kind: RelExprKind::Transform(
                Box::new(RelExpr {
                    kind: RelExprKind::From(From::RelRef(rel_id)),
                    ty: rel_ty,
                    id: id_gen.gen(),
                }),
                Transform::ProjectRetain(vec![col_position]),
            ),
            ty: col_ty,
            id: id_gen.gen(),
        }
    }
}

impl RelExprKind {
    pub fn new_transform(
        input: RelExpr,
        get_transform: impl FnOnce(usize) -> Transform,
    ) -> RelExprKind {
        let transform = get_transform(input.id);
        RelExprKind::Transform(Box::new(input), transform)
    }
}

impl ColExpr {
    pub fn new_subquery(subquery: RelExpr) -> Self {
        ColExpr {
            ty: subquery.ty.clone(),
            kind: ColExprKind::Subquery(Box::new(subquery)),
        }
    }
    pub fn new_rel_col(rel_id: usize, col_position: usize, col_ty: ir::Ty) -> ColExpr {
        ColExpr {
            kind: ColExprKind::InputRelCol(rel_id, col_position),
            ty: col_ty,
        }
    }
}
