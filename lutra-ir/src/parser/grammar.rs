use ::std::ops::{Shl, ShlAssign};

use chumsky::prelude::*;
use lutra_frontend::pr;

use super::perror::PError;
use crate::ir::*;
use lutra_frontend::_lexer::TokenKind;

pub fn program() -> impl Parser<TokenKind, Program, Error = PError> {
    let externals = keyword("let")
        .then(ident_keyword("externals"))
        .then(ctrl('='))
        .ignore_then(
            ident_part()
                .map(|id| ExternalSymbol { id })
                .separated_by(ctrl(','))
                .allow_trailing()
                .delimited_by(ctrl('['), ctrl(']')),
        )
        .then_ignore(ctrl(';'))
        .or_not()
        .map(Option::unwrap_or_default);

    let main = keyword("let")
        .then(ident_keyword("main"))
        .then(ctrl('='))
        .ignore_then(expr());

    externals
        .then(main)
        .map(|(externals, main)| Program { externals, main })
}

fn expr() -> impl Parser<TokenKind, Expr, Error = PError> + Clone {
    recursive(|expr| {
        let literal = select! {
            TokenKind::Literal(pr::Literal::Integer(i)) => ExprKind::Literal(Literal::Int(i)),
            TokenKind::Literal(pr::Literal::Float(i)) => ExprKind::Literal(Literal::Float(i)),
            TokenKind::Literal(pr::Literal::Boolean(i)) => ExprKind::Literal(Literal::Bool(i)),
            TokenKind::Literal(pr::Literal::String(i)) => ExprKind::Literal(Literal::Text(i)),
        };

        let pointer_external = ident_keyword("external")
            .ignore_then(ctrl('.'))
            .ignore_then(sid());
        let pointer_var = ident_keyword("var")
            .ignore_then(ctrl('.'))
            .ignore_then(sid().map(|mut sid| {
                sid.0 += 0x40000000;
                sid
            }));
        let pointer_param = ident_keyword("fn")
            .ignore_then(ctrl('.'))
            .ignore_then(sid())
            .then_ignore(ctrl('+'))
            .then(sid())
            .map(|(f, p)| Sid(0x80000000_i64 + f.0.shl(8) + p.0));
        let pointer = choice((pointer_external, pointer_var, pointer_param)).map(ExprKind::Pointer);

        let binding = binding(expr.clone());
        let tuple = tuple(expr.clone());
        let array = array(expr.clone());
        let function = function(expr.clone());
        let call = func_call(expr);

        let term = choice((pointer, literal, binding, tuple, array, function, call))
            .map(|kind| Expr { kind })
            .boxed();

        lookups(term)
    })
}

fn sid() -> impl Parser<TokenKind, Sid, Error = PError> {
    select! { TokenKind::Literal(pr::Literal::Integer(sid)) => Sid(sid) }
}

fn tuple<'a>(
    nested_expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
    nested_expr
        .separated_by(ctrl(','))
        .allow_trailing()
        .delimited_by(ctrl('{'), ctrl('}'))
        .recover_with(nested_delimiters(
            TokenKind::Control('{'),
            TokenKind::Control('}'),
            [
                (TokenKind::Control('{'), TokenKind::Control('}')),
                (TokenKind::Control('('), TokenKind::Control(')')),
                (TokenKind::Control('['), TokenKind::Control(']')),
            ],
            |_| vec![],
        ))
        .map(ExprKind::Tuple)
        .labelled("tuple")
}

fn array<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
    expr.separated_by(ctrl(','))
        .allow_trailing()
        .delimited_by(ctrl('['), ctrl(']'))
        .recover_with(nested_delimiters(
            TokenKind::Control('['),
            TokenKind::Control(']'),
            [
                (TokenKind::Control('{'), TokenKind::Control('}')),
                (TokenKind::Control('('), TokenKind::Control(')')),
                (TokenKind::Control('['), TokenKind::Control(']')),
            ],
            |_| vec![],
        ))
        .map(ExprKind::Array)
        .labelled("array")
}

enum LookupKind {
    Tuple,
    Array,
}

fn lookups<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    expr.then(
        choice((
            ctrl('.').ignore_then(select! {
                TokenKind::Literal(pr::Literal::Integer(i)) => (LookupKind::Tuple, i)
            }),
            select! {
                TokenKind::Literal(pr::Literal::Integer(i)) => (LookupKind::Array, i)
            }
            .delimited_by(ctrl('['), ctrl(']')),
        ))
        .repeated(),
    )
    .foldl(|base, (kind, offset)| Expr {
        kind: match kind {
            LookupKind::Tuple => ExprKind::TupleLookup(Box::new(TupleLookup { base, offset })),
            LookupKind::Array => ExprKind::ArrayLookup(Box::new(ArrayLookup { base, offset })),
        },
    })
}

fn func_call<'a, E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    expr.clone()
        .separated_by(ctrl(','))
        .allow_trailing()
        .delimited_by(ctrl('('), ctrl(')'))
        .recover_with(nested_delimiters(
            TokenKind::Control('('),
            TokenKind::Control(')'),
            [
                (TokenKind::Control('{'), TokenKind::Control('}')),
                (TokenKind::Control('('), TokenKind::Control(')')),
                (TokenKind::Control('['), TokenKind::Control(']')),
            ],
            |_| vec![],
        ))
        .then_ignore(ctrl('|'))
        .then(expr)
        .map(|(args, function)| ExprKind::Call(Box::new(Call { args, function })))
        .labelled("function call")
}

fn function<E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError>
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone,
{
    // func
    keyword("func")
        // scope id
        .ignore_then(sid().map(|mut sid| {
            sid.0.shl_assign(8);
            sid.0 += 0x80000000;
            sid
        }))
        .then_ignore(just(TokenKind::ArrowThin))
        // body
        .then(expr)
        .map(|(symbol_ns, body)| ExprKind::Function(Box::new(Function { symbol_ns, body })))
        .labelled("function")
}

fn binding<E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError>
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone,
{
    // func
    keyword("let")
        // symbol id
        .ignore_then(sid().map(|mut sid| {
            sid.0 += 0x40000000;
            sid
        }))
        .then_ignore(ctrl('='))
        // expr
        .then(expr.clone())
        .then_ignore(ctrl(';'))
        .repeated()
        .at_least(1)
        // main
        .then(expr)
        .foldr(|(symbol, expr), main| Expr {
            kind: ExprKind::Binding(Box::new(Binding { symbol, expr, main })),
        })
        .map(|e| e.kind)
        .labelled("binding")
}

fn ident_part() -> impl Parser<TokenKind, String, Error = PError> + Clone {
    select! {
        TokenKind::Ident(ident) => ident,
    }
}

fn ident_keyword(kw: &'static str) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    select! {
        TokenKind::Ident(ident) if ident == kw => (),
    }
}

fn keyword(kw: &'static str) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::Keyword(kw.to_string())).ignored()
}

fn ctrl(char: char) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::Control(char)).ignored()
}
