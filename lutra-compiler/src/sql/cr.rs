mod fold;

pub use fold::*;

use lutra_bin::ir;

#[derive(Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: ir::Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// Expression without relational inputs.
    From(From),

    /// Applies a relational transform.
    // TODO: combine this into BindCorrelated
    Transform(Box<BoundExpr>, Transform),

    /// Computes cartesian product of two relations, filtered by an expression.
    /// (translates to two FROM and WHERE)
    Join(Box<BoundExpr>, Box<BoundExpr>, Option<Box<Expr>>),

    /// Bind a relation and evaluate an unrelated expression
    /// (translates to CTE)
    Bind(Box<BoundExpr>, Box<Expr>),

    /// Bind a relation and evaluate an correlated expression
    /// (translates to either SELECT or FROM LATERAL)
    BindCorrelated(Box<BoundExpr>, Box<Expr>),

    /// Computes relations separately and concatenates them together
    Union(Vec<Expr>),

    /// Fixed-point iteration. Has initial value and an update step.
    /// Update step expression can refer to its own id.
    Iteration(Box<Expr>, Box<BoundExpr>),
}

/// An expression, bound to an identifier.
/// Such relations can be referred to by From::RelRef.
#[derive(Clone, PartialEq)]
pub struct BoundExpr {
    pub id: usize,
    pub rel: Expr,
}

#[derive(Debug, Clone, strum::AsRefStr, PartialEq)]
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
    Deserialize(Box<Expr>),

    /// Converts a relation into a JSON-encoded value
    Serialize(Box<Expr>),

    /// List of conditions and values.
    /// Evaluates to first value whose condition is true.
    /// Does not evaluate any following conditions or values.
    /// Does not evaluate values whose conditions are false.
    Case(Vec<(Expr, Expr)>),

    /// Direct SQL source. Should be used for std::sql::raw only.
    SQLSource(String),
}

#[derive(Debug, Clone, strum::AsRefStr, PartialEq)]
pub enum Transform {
    /// Projection that picks columns by position
    ProjectPick(Vec<usize>),

    /// Projection that discards columns by position
    ProjectDiscard(Vec<usize>),

    Aggregate(Vec<Expr>),

    /// Filters by retaining only first N rows
    Limit(usize),

    /// Filtering (also known as selection)
    Where(Box<Expr>),

    /// Replaces first column (which is index for array ty).
    /// None implies to use the current order of rows in relation
    /// (which is implemented by ROW_NUMBER())
    IndexBy(Option<Box<Expr>>),

    /// Applies the order from index column to rows of relation.
    Order,

    /// Groups rows by a given key and computes a value for each partition.
    /// Returns a row for each key.
    /// Columns include the index, followed by columns of the value expr.
    Group {
        key: Box<Expr>,
        values: Vec<Expr>,
    },

    /// Inserts rows into a table
    Insert(String),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExprKind::From(f) => f.fmt(fmt)?,
            k => k.fmt(fmt)?,
        }
        fmt.write_str(": ")?;
        fmt.write_str(&ir::print_ty(&self.ty))?;
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
    /// Helper for creating transforms that do not modify type of the expr
    pub fn new_iso_transform(rel: Box<BoundExpr>, transform: Transform) -> Self {
        Expr {
            ty: rel.rel.ty.clone(),
            kind: ExprKind::Transform(rel, transform),
        }
    }

    pub fn new_rel_ref(rel: &BoundExpr) -> Self {
        Expr {
            ty: rel.rel.ty.clone(),
            kind: ExprKind::From(From::RelRef(rel.id)),
        }
    }

    #[allow(dead_code)]
    pub fn new_deserialize(rel: Expr) -> Self {
        Expr {
            ty: rel.ty.clone(),
            kind: ExprKind::From(From::Deserialize(Box::new(rel))),
        }
    }
    pub fn new_serialize(rel: Expr) -> Self {
        Expr {
            ty: ir::Ty::new(ir::TyPrimitive::text),
            kind: ExprKind::From(From::Serialize(Box::new(rel))),
        }
    }

    pub fn null(ty: ir::Ty) -> Expr {
        Expr {
            kind: ExprKind::From(From::Null),
            ty,
        }
    }

    /// Helper for creating transforms that do not modify type of the expr
    pub fn new_bind_correlated(bound: Box<BoundExpr>, correlated: Expr) -> Self {
        Expr {
            ty: correlated.ty.clone(),
            kind: ExprKind::BindCorrelated(bound, Box::new(correlated)),
        }
    }
}
