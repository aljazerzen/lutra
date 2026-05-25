use chumsky::input::MappedInput;
use chumsky::prelude::*;
use lutra_compiler::pr;

use super::perror::PError;
use super::perror::Span;
use lutra_bin::ir::*;
use lutra_compiler::_lexer::TokenKind;

pub fn program<'src>() -> impl Parser<'src, I<'src>, Program, extra::Err<PError>> {
    let defs = keyword("type")
        .ignore_then(path())
        .then_ignore(ctrl('='))
        .then(ty())
        .then_ignore(ctrl(';'))
        .map(|(name, ty)| TyDef { name, ty })
        .repeated()
        .collect::<Vec<_>>()
        .labelled("type defs");

    let main = keyword("let")
        .then(ident_keyword("main"))
        .then(ctrl('='))
        .ignore_then(expr());

    defs.then(main).map(|(defs, main)| Program { main, defs })
}

type I<'src> = MappedInput<'src, TokenKind, Span, &'src [(TokenKind, Span)]>;

fn expr<'src>() -> impl Parser<'src, I<'src>, Expr, extra::Err<PError>> + Clone {
    recursive(|expr| {
        let literal = select! {
            TokenKind::Literal(pr::Literal::Number(n)) => {
                let lit = pr::Literal::Number(n);
                if let Some(i) = lit.as_integer() {
                    ExprKind::Literal(Literal::Prim64(i.to_le()))
                } else if let Some(f) = lit.as_float() {
                    ExprKind::Literal(Literal::Prim64(u64::from_le_bytes(f.to_le_bytes())))
                } else {
                    todo!()
                }
            },
            TokenKind::Literal(pr::Literal::Boolean(i)) => ExprKind::Literal(Literal::Prim8(i as u8)),
            TokenKind::Literal(pr::Literal::Text(i)) => ExprKind::Literal(Literal::text(i)),
        };

        let pointer_external = keyword("external")
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

        let tuple = tuple(expr.clone());
        let array = array(expr.clone());
        let tuple_lookup = tuple_lookup(expr.clone());
        let function = function(expr.clone());
        let call = func_call(expr.clone());
        let wrapped = choice((tuple_lookup, function, call, tuple, array))
            .delimited_by(ctrl('('), ctrl(')'))
            .recover_with(via_parser(nested_delimiters(
                TokenKind::Control('('),
                TokenKind::Control(')'),
                [
                    (TokenKind::Control('{'), TokenKind::Control('}')),
                    (TokenKind::Control('('), TokenKind::Control(')')),
                    (TokenKind::Control('['), TokenKind::Control(']')),
                ],
                |_| ExprKind::Tuple(vec![]),
            )));

        let simple = choice((pointer, literal, wrapped))
            .then(ctrl(':').ignore_then(ty()))
            .map(|(kind, ty)| Expr { kind, ty });

        binding(expr).or(simple).boxed()
    })
}

fn external_ptr<'src>() -> impl Parser<'src, I<'src>, ExternalPtr, extra::Err<PError>> {
    ident_part()
        .separated_by(just(TokenKind::PathSep))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|id| id.join("::"))
        .map(|id| ExternalPtr { id })
}

fn ty<'src>() -> impl Parser<'src, I<'src>, Ty, extra::Err<PError>> {
    recursive(|ty_inner| {
        let primitive = select! {
            TokenKind::Ident(i) if i == "Prim8" => TyPrimitive::prim8,
            TokenKind::Ident(i) if i == "Prim16" => TyPrimitive::prim16,
            TokenKind::Ident(i) if i == "Prim32" => TyPrimitive::prim32,
            TokenKind::Ident(i) if i == "Prim64" => TyPrimitive::prim64,
        }
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
            .collect::<Vec<_>>()
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
                    .collect::<Vec<_>>()
                    .delimited_by(ctrl('{'), ctrl('}')),
            )
            .map(TyKind::Enum);

        let func = keyword("func")
            .ignore_then(
                ty_inner
                    .clone()
                    .separated_by(ctrl(','))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(ctrl('('), ctrl(')')),
            )
            .then_ignore(just(TokenKind::ArrowThin))
            .then(ty_inner)
            .map(|(params, body)| TyFunction { params, body })
            .map(Box::new)
            .map(TyKind::Function);

        let ident = path().map(TyKind::Ident);

        choice((primitive, array, tuple, enum_, func, ident)).map(Ty::new)
    })
    .labelled("a type")
}

fn uint32<'src>() -> impl Parser<'src, I<'src>, u32, extra::Err<PError>> {
    select! { TokenKind::Literal(pr::Literal::Number(i)) => i.parse::<u32>().unwrap() }
}

fn tuple<'src, 'a>(
    nested_expr: impl Parser<'src, I<'src>, Expr, extra::Err<PError>> + Clone + 'a,
) -> impl Parser<'src, I<'src>, ExprKind, extra::Err<PError>> + Clone + 'a
where
    'src: 'a,
{
    ident_keyword("tuple")
        .ignore_then(
            nested_expr
                .map(|expr| TupleField {
                    expr,
                    unpack: false,
                })
                .separated_by(ctrl(','))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .map(ExprKind::Tuple)
        .labelled("tuple")
}

fn array<'src, 'a>(
    expr: impl Parser<'src, I<'src>, Expr, extra::Err<PError>> + Clone + 'a,
) -> impl Parser<'src, I<'src>, ExprKind, extra::Err<PError>> + Clone + 'a
where
    'src: 'a,
{
    ident_keyword("array")
        .ignore_then(
            expr.separated_by(ctrl(','))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .map(ExprKind::Array)
        .labelled("array")
}

fn tuple_lookup<'src, 'a, E>(
    expr: E,
) -> impl Parser<'src, I<'src>, ExprKind, extra::Err<PError>> + 'a
where
    'src: 'a,
    E: Parser<'src, I<'src>, Expr, extra::Err<PError>> + 'a,
{
    ident_keyword("tuple_lookup")
        .ignore_then(expr)
        .then(select! {
            TokenKind::Literal(pr::Literal::Number(i)) => i.parse().unwrap()
        })
        .map(|(base, position)| ExprKind::TupleLookup(Box::new(TupleLookup { base, position })))
        .labelled("tuple lookup")
}

fn func_call<'src, 'a, E>(
    expr: E,
) -> impl Parser<'src, I<'src>, ExprKind, extra::Err<PError>> + Clone + 'a
where
    'src: 'a,
    E: Parser<'src, I<'src>, Expr, extra::Err<PError>> + Clone + 'a,
{
    ident_keyword("call")
        .ignore_then(expr.clone())
        .then(ctrl(',').ignore_then(expr).repeated().collect::<Vec<_>>())
        .then_ignore(ctrl(',').or_not())
        .map(|(function, args)| ExprKind::Call(Box::new(Call { function, args })))
        .labelled("function call")
}

fn function<'src, E>(expr: E) -> impl Parser<'src, I<'src>, ExprKind, extra::Err<PError>>
where
    E: Parser<'src, I<'src>, Expr, extra::Err<PError>> + Clone,
{
    keyword("func")
        .ignore_then(uint32())
        .then_ignore(just(TokenKind::ArrowThin))
        .then(expr)
        .map(|(id, body)| ExprKind::Function(Box::new(Function { id, body })))
        .labelled("function")
}

fn binding<'src, E>(expr: E) -> impl Parser<'src, I<'src>, Expr, extra::Err<PError>>
where
    E: Parser<'src, I<'src>, Expr, extra::Err<PError>> + Clone,
{
    keyword("let")
        .ignore_then(uint32())
        .then_ignore(ctrl('='))
        .then(expr.clone().labelled("bound"))
        .then_ignore(ctrl(';'))
        .then(expr.labelled("main"))
        .map(|((id, expr), main)| Expr {
            ty: main.ty.clone(),
            kind: ExprKind::Binding(Box::new(Binding { id, expr, main })),
        })
        .labelled("binding")
}

fn path<'src>() -> impl Parser<'src, I<'src>, Path, extra::Err<PError>> + Clone {
    ident_part()
        .separated_by(just(TokenKind::PathSep))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(Path)
}

fn ident_part<'src>() -> impl Parser<'src, I<'src>, String, extra::Err<PError>> + Clone {
    select! {
        TokenKind::Ident(ident) => ident,
    }
}

fn ident_keyword<'src>(
    kw: &'static str,
) -> impl Parser<'src, I<'src>, (), extra::Err<PError>> + Clone {
    select! {
        TokenKind::Ident(ident) if ident == kw => (),
    }
}

fn keyword<'src>(kw: &'static str) -> impl Parser<'src, I<'src>, (), extra::Err<PError>> + Clone {
    just(TokenKind::Keyword(kw)).ignored()
}

fn ctrl<'src>(char: char) -> impl Parser<'src, I<'src>, (), extra::Err<PError>> + Clone {
    just(TokenKind::Control(char)).ignored()
}
