use enum_as_inner::EnumAsInner;

use crate::pr::{BinOp, Literal, Ty, UnOp};
use crate::span::Span;

use super::{Path, TyParam};

impl Expr {
    pub fn new<K: Into<ExprKind>>(kind: K) -> Self {
        Expr {
            kind: kind.into(),
            span: None,
            ty: None,
            ty_args: Vec::new(),
            target: None,
            scope_id: None,
        }
    }

    pub fn new_with_span<K: Into<ExprKind>>(kind: K, span: Span) -> Expr {
        Expr {
            kind: kind.into(),
            span: Some(span),
            ty: None,
            ty_args: Vec::new(),
            target: None,
            scope_id: None,
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

    /// Type of expression this node represents.
    /// [None] means that type should be inferred.
    pub ty: Option<Ty>,

    /// When this expression refers to a function with type parameters,
    /// these params are instantiated into these args and finalized when
    /// scope closes.
    pub ty_args: Vec<Ty>,

    /// When this expr is the root of a new scope, this holds the id of
    /// that scope. This will always be set for [ExprKind::Func], but
    /// might be set for other nodes too.
    pub scope_id: Option<usize>,

    /// When this expression is an identifer, this holds information about
    /// what is being referenced.
    pub target: Option<Ref>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ref {
    FullyQualified(Path),
    Local {
        /// scope id
        scope: usize,

        /// position of the name within the scope
        offset: usize,
    },
}

#[derive(Debug, EnumAsInner, PartialEq, Clone, strum::AsRefStr)]
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
    TypeAnnotation(TypeAnnotation),

    Tuple(Vec<TupleField>),
    Array(Vec<Expr>),
    EnumVariant(EnumVariant),

    Range(Range),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    FuncCall(FuncCall),
    Func(Box<Func>),
    FString(Vec<InterpolateItem>),
    Case(Vec<SwitchCase>),

    Internal,
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
    pub func: Box<Expr>,
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

    pub ty_params: Vec<TyParam>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncParam {
    pub name: String,

    pub ty: Option<Ty>,

    pub span: Span,
}

/// A value and a series of functions that are to be applied to that value one after another.
#[derive(Debug, PartialEq, Clone)]
pub struct Pipeline {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeAnnotation {
    pub expr: Box<Expr>,
    pub ty: Box<Ty>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct TupleField {
    pub name: Option<String>,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariant {
    pub tag: usize,
    pub inner: Option<Box<Expr>>,
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
