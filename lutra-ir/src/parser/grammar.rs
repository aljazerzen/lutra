use chumsky::prelude::*;
use lutra_compiler::pr;

use super::perror::PError;
use lutra_bin::ir::*;
use lutra_compiler::_lexer::TokenKind;

pub fn program() -> impl Parser<TokenKind, Program, Error = PError> {
    let types = keyword("type")
        .ignore_then(path())
        .then_ignore(ctrl('='))
        .then(ty())
        .then_ignore(ctrl(';'))
        .map(|(name, ty)| TyDef { name, ty })
        .repeated()
        .labelled("type defs");

    let main = keyword("let")
        .then(ident_keyword("main"))
        .then(ctrl('='))
        .ignore_then(expr());

    types
        .then(main)
        .map(|(types, main)| Program { main, types })
}

fn expr() -> impl Parser<TokenKind, Expr, Error = PError> + Clone {
    recursive(|expr| {
        let literal = select! {
            TokenKind::Literal(pr::Literal::Integer(i)) => ExprKind::Literal(Literal::Int(i)),
            TokenKind::Literal(pr::Literal::Float(i)) => ExprKind::Literal(Literal::Float(i)),
            TokenKind::Literal(pr::Literal::Boolean(i)) => ExprKind::Literal(Literal::Bool(i)),
            TokenKind::Literal(pr::Literal::Text(i)) => ExprKind::Literal(Literal::Text(i)),
        };

        let pointer_external = ident_keyword("external")
            .ignore_then(ctrl('.'))
            .ignore_then(external_ptr())
            .map(Pointer::External);
        let pointer_var = ident_keyword("var")
            .ignore_then(ctrl('.'))
            .ignore_then(uint32())
            .map(Pointer::Binding);
        let pointer_param = ident_keyword("fn")
            .ignore_then(ctrl('.'))
            .ignore_then(uint32())
            .then_ignore(ctrl('+'))
            .then(uint32())
            .map(|(function_id, p)| ParameterPtr {
                function_id,
                param_position: p as u8,
            })
            .map(Pointer::Parameter);
        let pointer = choice((pointer_external, pointer_var, pointer_param)).map(ExprKind::Pointer);

        // let binding = binding(expr.clone());
        let tuple = tuple(expr.clone());
        let array = array(expr.clone());
        let function = function(expr.clone());
        let call = func_call(expr.clone());

        let term = choice((pointer, literal, tuple, array, function, call))
            .then(ctrl(':').ignore_then(ty()))
            .map(|(kind, ty)| Expr { kind, ty })
            .boxed();

        binding(tuple_lookup(term).boxed())
    })
}

fn external_ptr() -> impl Parser<TokenKind, ExternalPtr, Error = PError> {
    let id = ident_part()
        .separated_by(just(TokenKind::PathSep))
        .at_least(1)
        .map(|id| id.join("::"));

    id.map(|id| ExternalPtr { id })
}

fn ty() -> impl Parser<TokenKind, Ty, Error = PError> {
    recursive(|ty_inner| {
        let primitive = choice((
            ident_keyword("bool").to(TyPrimitive::bool),
            ident_keyword("int8").to(TyPrimitive::int8),
            ident_keyword("int16").to(TyPrimitive::int16),
            ident_keyword("int32").to(TyPrimitive::int32),
            ident_keyword("int64").to(TyPrimitive::int64),
            ident_keyword("uint8").to(TyPrimitive::uint8),
            ident_keyword("uint16").to(TyPrimitive::uint16),
            ident_keyword("uint32").to(TyPrimitive::uint32),
            ident_keyword("uint64").to(TyPrimitive::uint64),
            ident_keyword("float32").to(TyPrimitive::float32),
            ident_keyword("float64").to(TyPrimitive::float64),
            ident_keyword("text").to(TyPrimitive::text),
        ))
        .map(TyKind::Primitive);

        let array = ty_inner
            .clone()
            .delimited_by(ctrl('['), ctrl(']'))
            .map(Box::new)
            .map(TyKind::Array);

        let tuple = ident_part()
            .then_ignore(ctrl('='))
            .or_not()
            .then(ty_inner.clone())
            .map(|(name, ty)| TyTupleField { name, ty })
            .separated_by(ctrl(','))
            .allow_trailing()
            .delimited_by(ctrl('{'), ctrl('}'))
            .map(TyKind::Tuple);

        let enum_ = ident_keyword("enum")
            .ignore_then(
                ident_part()
                    .then_ignore(ctrl('='))
                    .then(ty_inner.clone())
                    .map(|(name, ty)| TyEnumVariant { name, ty })
                    .separated_by(ctrl(','))
                    .allow_trailing()
                    .delimited_by(ctrl('{'), ctrl('}')),
            )
            .map(TyKind::Enum);

        let func = keyword("func")
            .ignore_then(
                ty_inner
                    .clone()
                    .separated_by(ctrl(','))
                    .allow_trailing()
                    .delimited_by(ctrl('('), ctrl(')')),
            )
            .then_ignore(just(TokenKind::ArrowThin))
            .then(ty_inner)
            .map(|(params, body)| TyFunction { params, body })
            .map(Box::new)
            .map(TyKind::Function);

        let ident = path().map(TyKind::Ident);

        choice((primitive, array, tuple, enum_, func, ident)).map(|kind| {
            let mut ty = Ty::new(kind);
            ty.layout = lutra_bin::layout::get_layout_simple(&ty);
            ty
        })
    })
    .labelled("a type")
}

fn uint32() -> impl Parser<TokenKind, u32, Error = PError> {
    select! { TokenKind::Literal(pr::Literal::Integer(i)) => i as u32 }
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

fn tuple_lookup<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + 'a,
{
    expr.then(
        ctrl('.')
            .ignore_then(select! {
                TokenKind::Literal(pr::Literal::Integer(i)) => i as u16
            })
            .then_ignore(ctrl(':'))
            .then(ty())
            .repeated(),
    )
    .foldl(|base, (position, ty)| Expr {
        kind: ExprKind::TupleLookup(Box::new(TupleLookup { base, position })),
        ty,
    })
}

fn func_call<'a, E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    ident_keyword("call")
        .ignore_then(expr.clone())
        .then(ctrl(',').ignore_then(expr).repeated())
        .then_ignore(ctrl(',').or_not())
        .map(|(function, args)| ExprKind::Call(Box::new(Call { function, args })))
        .delimited_by(ctrl('('), ctrl(')'))
        .recover_with(nested_delimiters(
            TokenKind::Control('('),
            TokenKind::Control(')'),
            [
                (TokenKind::Control('{'), TokenKind::Control('}')),
                (TokenKind::Control('('), TokenKind::Control(')')),
                (TokenKind::Control('['), TokenKind::Control(']')),
            ],
            |_| ExprKind::Tuple(vec![]),
        ))
        .labelled("function call")
}

fn function<E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError>
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone,
{
    // func
    keyword("func")
        // scope id
        .ignore_then(uint32())
        .then_ignore(just(TokenKind::ArrowThin))
        // body
        .then(expr)
        .delimited_by(ctrl('('), ctrl(')'))
        .map(|(id, body)| ExprKind::Function(Box::new(Function { id, body })))
        .labelled("function")
}

fn binding<E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError>
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone,
{
    // func
    keyword("let")
        // symbol id
        .ignore_then(uint32())
        .then_ignore(ctrl('='))
        // expr
        .then(expr.clone())
        .then_ignore(ctrl(';'))
        .repeated()
        // main
        .then(expr)
        .foldr(|(id, expr), main| Expr {
            ty: main.ty.clone(),
            kind: ExprKind::Binding(Box::new(Binding { id, expr, main })),
        })
        .labelled("binding")
}

fn path() -> impl Parser<TokenKind, Path, Error = PError> + Clone {
    ident_part()
        .separated_by(just(TokenKind::PathSep))
        .at_least(1)
        .map(Path)
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
