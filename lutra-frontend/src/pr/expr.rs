use enum_as_inner::EnumAsInner;

use crate::pr::{BinOp, Literal, Ty, UnOp};
use crate::span::Span;

use super::Path;

impl Expr {
    pub fn new<K: Into<ExprKind>>(kind: K) -> Self {
        Expr {
            kind: kind.into(),
            span: None,
            alias: None,
            ty: None,
            id: None,
        }
    }
}

// The following code is tested by the tests_misc crate to match expr.rs in prqlc.

/// Expr is anything that has a value and thus a type.
/// Most of these can contain other [Expr] themselves; literals should be [ExprKind::Literal].
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,

    pub span: Option<Span>,

    pub alias: Option<String>,

    /// Type of expression this node represents.
    /// [None] means that type should be inferred.
    pub ty: Option<Ty>,

    pub id: Option<usize>,
}

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum ExprKind {
    Ident(Path),

    /// A lookup into an object by name or position.
    /// Currently, this includes only tuple field lookups, primarily by name.
    Indirection {
        base: Box<Expr>,
        field: IndirectionKind,
    },
    Literal(Literal),
    Pipeline(Pipeline),

    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    Range(Range),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    FuncCall(FuncCall),
    Func(Box<Func>),
    FString(Vec<InterpolateItem>),
    Case(Vec<SwitchCase>),

    Internal,
}

impl ExprKind {
    pub fn into_expr(self, span: Span) -> Expr {
        Expr {
            span: Some(span),
            kind: self,
            alias: None,
            ty: None,
            id: None,
        }
    }
}

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum IndirectionKind {
    Name(String),
    Position(i64),
    Star,
}

/// Expression with two operands and an operator, such as `1 + 2`.
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinOp,
    pub right: Box<Expr>,
}

/// Expression with one operand and an operator, such as `-1`.
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

/// Function call.
#[derive(Debug, PartialEq, Clone)]
pub struct FuncCall {
    pub name: Box<Expr>,
    pub args: Vec<Expr>,
}

/// Function called with possibly missing positional arguments.
/// May also contain environment that is needed to evaluate the body.
#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    /// Type requirement for the function body expression.
    pub return_ty: Option<Ty>,

    /// Expression containing parameter (and environment) references.
    pub body: Box<Expr>,

    /// Function parameters.
    pub params: Vec<FuncParam>,

    /// Generic type arguments within this function.
    pub generic_type_params: Vec<GenericTypeParam>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncParam {
    pub name: String,

    pub ty: Option<Ty>,

    pub default_value: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct GenericTypeParam {
    /// Assigned name of this generic type argument.
    pub name: String,

    pub domain: Vec<Ty>,

    pub span: Option<Span>,
}

/// A value and a series of functions that are to be applied to that value one after another.
#[derive(Debug, PartialEq, Clone)]
pub struct Pipeline {
    pub exprs: Vec<Expr>,
}

/// Inclusive-inclusive range.
/// Missing bound means unbounded range.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Range {
    pub start: Option<Box<Expr>>,
    pub end: Option<Box<Expr>>,
}

impl Range {
    pub const fn unbounded() -> Self {
        Range {
            start: None,
            end: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpolateItem {
    String(String),
    Expr {
        expr: Box<Expr>,
        format: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCase {
    pub condition: Box<Expr>,
    pub value: Box<Expr>,
}

impl From<Literal> for ExprKind {
    fn from(value: Literal) -> Self {
        ExprKind::Literal(value)
    }
}

impl From<Func> for ExprKind {
    fn from(value: Func) -> Self {
        ExprKind::Func(Box::new(value))
    }
}

impl From<Path> for ExprKind {
    fn from(value: Path) -> Self {
        ExprKind::Ident(value)
    }
}

impl From<Range> for ExprKind {
    fn from(value: Range) -> Self {
        ExprKind::Range(value)
    }
}

impl PartialEq for GenericTypeParam {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.domain == other.domain
    }
}

impl Eq for GenericTypeParam {}

impl std::hash::Hash for GenericTypeParam {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.domain.hash(state);
    }
}
