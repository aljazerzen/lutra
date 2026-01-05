use enum_as_inner::EnumAsInner;

use crate::Span;
use crate::pr::{BinOp, Literal, Ty, UnOp};

use super::{Path, TyParam};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ref {
    Global(Path),
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
    Lookup {
        base: Box<Expr>,
        lookup: Lookup,
    },
    Literal(Literal),
    Nested(Box<Expr>),
    TypeAnnotation(TypeAnnotation),

    Tuple(Vec<TupleField>),
    Array(Vec<Expr>),
    Variant(Variant),

    Range(Range),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(Call),
    Func(Box<Func>),
    FuncShort(Box<FuncShort>),
    FString(Vec<InterpolateItem>),
    Match(Match),
    If(If),

    VarBinding(VarBinding),
}

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum Lookup {
    Name(String),
    Position(i64),
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

/// Call of either a function, enum variant, or framed type constructor.
#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub subject: Box<Expr>,
    pub args: Vec<CallArg>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallArg {
    pub label: Option<String>,
    pub expr: Expr,
    pub span: Option<Span>,
}

impl CallArg {
    pub fn simple(expr: Expr) -> Self {
        CallArg {
            label: None,
            span: expr.span,
            expr,
        }
    }
}

/// Function. `func (p1: P1, p2: P2): B where T -> b`
#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    /// Function parameters.
    pub params: Vec<FuncParam>,

    /// Type requirement for the function body expression.
    pub return_ty: Option<Ty>,

    /// Expression containing parameter references.
    pub body: Option<Box<Expr>>,

    /// Parameters of the types within this function (where clause).
    pub ty_params: Vec<TyParam>,
}

/// Function, short notation. `x -> b`
#[derive(Debug, PartialEq, Clone)]
pub struct FuncShort {
    /// Function parameter.
    pub param: FuncParam,

    /// Expression containing parameter (and environment) references.
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncParam {
    /// When true, arg used for this param must be a const value.
    pub constant: bool,

    /// Public name of the paramater, that can be specified in function calls.
    pub label: Option<String>,

    /// Parameter name that can be referenced in the function.
    pub name: String,

    /// Type requirement for this param.
    pub ty: Option<Ty>,

    pub span: Span,
}

/// A value and a series of functions that are to be applied to that value one after another.
#[derive(Debug, PartialEq, Clone)]
pub struct Pipeline {
    /// Items of the pipeline. Must contain at least one element.
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeAnnotation {
    pub expr: Box<Expr>,
    pub ty: Box<Ty>,
}

/// Tuple field. `name = expr` or `..expr`
#[derive(Debug, PartialEq, Clone)]
pub struct TupleField {
    pub name: Option<String>,
    pub unpack: bool,
    pub expr: Expr,
}

/// Enum variant. `.name(inner)`
#[derive(Debug, PartialEq, Clone)]
pub struct Variant {
    pub name: String,
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
pub struct Match {
    pub subject: Box<Expr>,

    // contract: there will be at least one branch
    pub branches: Vec<MatchBranch>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchBranch {
    pub pattern: Pattern,
    pub value: Box<Expr>,
}

/// A pattern that can be matched against an expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
    pub variant_tag: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, strum::AsRefStr)]
pub enum PatternKind {
    /// Match an enum variant, recurse into matching inner
    Enum(String, Option<Box<Pattern>>),

    /// Match value of a primitive type
    Literal(Literal),

    /// Match any of the following
    AnyOf(Vec<Pattern>),

    /// Match anything, bind it to a name
    Bind(String),
}

impl Pattern {
    pub fn new_with_span(kind: PatternKind, span: Span) -> Self {
        Self {
            kind,
            span,
            variant_tag: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Box<Expr>,
    pub then: Box<Expr>,
    pub els: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarBinding {
    pub name: String,
    pub bound: Box<Expr>,
    pub main: Box<Expr>,
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

impl From<Variant> for ExprKind {
    fn from(value: Variant) -> Self {
        ExprKind::Variant(value)
    }
}

impl From<TypeAnnotation> for ExprKind {
    fn from(value: TypeAnnotation) -> Self {
        ExprKind::TypeAnnotation(value)
    }
}
