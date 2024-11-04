use enum_as_inner::EnumAsInner;
use lutra_parser::generic;
use std::collections::HashMap;

use crate::pr::{GenericTypeParam, Ident, Literal, Span, Ty};

/// Expr is anything that has a value and thus a type.
/// Most of these can contain other [Expr] themselves; literals should be [ExprKind::Literal].
#[derive(Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,

    pub span: Option<Span>,

    pub alias: Option<String>,

    /// Unique identificator of the node. Set exactly once during semantic::resolve.
    pub id: Option<usize>,

    /// For [Ident]s, this is id of node referenced by the ident
    pub target_id: Option<usize>,

    /// Type of expression this node represents.
    /// [None] means that type should be inferred.
    pub ty: Option<Ty>,

    pub needs_window: bool,

    /// When true on [ExprKind::Tuple], this list will be flattened when placed
    /// in some other list.
    // TODO: maybe we should have a special ExprKind instead of this flag?
    pub flatten: bool,
}

#[derive(Debug, EnumAsInner, PartialEq, Clone, strum::AsRefStr)]
pub enum ExprKind {
    Ident(Ident),
    All {
        within: Box<Expr>,
        except: Box<Expr>,
    },
    Indirection {
        base: Box<Expr>,
        field: IndirectionKind,
    },
    Literal(Literal),

    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    FuncCall(FuncCall),
    Func(Box<Func>),
    FuncApplication(FuncApplication),

    SString(Vec<InterpolateItem>),
    FString(Vec<InterpolateItem>),
    Case(Vec<SwitchCase>),
    RqOperator {
        name: String,
        args: Vec<Expr>,
    },

    /// placeholder for values provided after query is compiled
    Param(String),

    /// When used instead of function body, the function will be translated to a RQ operator.
    /// Contains ident of the RQ operator.
    Internal(String),
}

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum IndirectionKind {
    Name(String),
    Position(i64),
}

/// Function call.
#[derive(Debug, PartialEq, Clone)]
pub struct FuncCall {
    pub name: Box<Expr>,
    pub args: Vec<Expr>,
    pub named_args: HashMap<String, Expr>,
}

/// Function called with possibly missing positional arguments.
/// May also contain environment that is needed to evaluate the body.
#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    /// Type requirement for the function body expression.
    pub return_ty: Option<Ty>,

    /// Expression containing parameter (and environment) references.
    pub body: Box<Expr>,

    /// Positional function parameters.
    pub params: Vec<FuncParam>,

    /// Named function parameters.
    pub named_params: Vec<FuncParam>,

    /// Generic type arguments within this function.
    pub generic_type_params: Vec<GenericTypeParam>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncParam {
    pub name: String,

    pub ty: Option<Ty>,

    pub default_value: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncApplication {
    pub func: Box<Expr>, // TODO: change this to Expr

    pub args: Vec<Expr>,
}

pub type Range = generic::Range<Box<Expr>>;
pub type InterpolateItem = generic::InterpolateItem<Expr>;
pub type SwitchCase = generic::SwitchCase<Box<Expr>>;

impl From<Literal> for ExprKind {
    fn from(value: Literal) -> Self {
        ExprKind::Literal(value)
    }
}

impl From<Ident> for ExprKind {
    fn from(value: Ident) -> Self {
        ExprKind::Ident(value)
    }
}

impl From<Func> for ExprKind {
    fn from(value: Func) -> Self {
        ExprKind::Func(Box::new(value))
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = f.debug_struct("Expr");

        if let Some(x) = &self.span {
            ds.field("span", x);
        }
        ds.field("kind", &self.kind);
        if let Some(x) = &self.alias {
            ds.field("alias", x);
        }
        if let Some(x) = &self.id {
            ds.field("id", x);
        }
        if let Some(x) = &self.target_id {
            ds.field("target_id", x);
        }
        if self.needs_window {
            ds.field("needs_window", &self.needs_window);
        }
        if self.flatten {
            ds.field("flatten", &self.flatten);
        }
        if let Some(x) = &self.ty {
            ds.field("ty", &x);
        }
        ds.finish()
    }
}
