#![allow(dead_code)]

/// Symbol id.
/// - 0b00xxxxxx - external
/// - 0b01xxxxxx - local bindings
/// - 0b10xxxxxx - function scopes
/// - 0b11xxxxxx - unused
pub type Sid = u32;

pub struct Program {
    pub externals: Vec<ExternalSymbol>,
    pub main: Expr,
}

#[derive(Clone)]
pub struct ExternalSymbol {
    pub id: String,
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Clone)]
pub enum ExprKind {
    Pointer(Sid),
    Literal(Literal),
    Call(Call),
    Function(Function),
    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    TupleLookup(TupleLookup),
    ArrayLookup(ArrayLookup),
    Binding(Binding),
}

#[derive(Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(String),
}

#[derive(Clone)]
pub struct Call {
    pub function: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Clone)]
pub struct Function {
    pub symbol_ns: Sid,

    // can contain function parameters prefixed with symbol_ns
    pub body: Box<Expr>,
}

#[derive(Clone)]
pub struct TupleLookup {
    pub base: Box<Expr>,
    pub offset: u32,
}

#[derive(Clone)]
pub struct ArrayLookup {
    pub base: Box<Expr>,
    pub offset: u32,
}

#[derive(Clone)]
pub struct Binding {
    pub symbol: Sid,
    pub expr: Box<Expr>,
    pub main: Box<Expr>,
}
