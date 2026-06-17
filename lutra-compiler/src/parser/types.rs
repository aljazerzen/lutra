use chumsky::input::ValueInput;
use chumsky::prelude::*;

use super::expr;
use super::helpers::*;
use super::{PExtra, TokenKind};

use crate::Span;
use crate::pr::*;

pub(crate) fn type_expr<'src, I>(
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let primitive = ty_primitive().map(TyKind::Primitive);

    let ident = expr::path().map(TyKind::Ident);

    let func_params = func_params(ty.clone(), expr);

    let func = keyword("func")
        .ignore_then(
            func_params
                .then_ignore(ctrl(':'))
                .then(ty.clone().map(Box::new).map(Some))
                .map(|(params, body)| TyFunc {
                    params,
                    body,
                    ty_params: Vec::new(),
                }),
        )
        .map(TyKind::Func);

    let comprehension_body = keyword("for")
        .ignore_then(ident_part())
        .then_ignore(ctrl(':'))
        .then(ident_part())
        .then_ignore(keyword("in"))
        .then(ty.clone().map(Box::new))
        .then_ignore(keyword("do"))
        .then(ident_part().then_ignore(ctrl(':')).or_not())
        .then(ty.clone().map(Box::new))
        .map(|((((v_n, v_t), tuple), b_n), b_t)| TyTupleComprehension {
            tuple,
            variable_name: v_n,
            variable_ty: v_t,
            body_name: b_n,
            body_ty: b_t,
        })
        .map(TyKind::TupleComprehension);

    let tuple_body = sequence(
        ident_part()
            .then_ignore(ctrl(':'))
            .or_not()
            .then(ty.clone())
            .map(|(name, ty)| TyTupleField {
                name,
                ty,
                unpack: false,
            }),
    )
    .map(TyKind::Tuple);

    let tuple = delimited_by_braces(comprehension_body.or(tuple_body), |_| TyKind::Tuple(vec![]))
        .labelled("tuple");

    let enum_ = keyword("enum")
        .ignore_then(delimited_by_braces(
            ident_part()
                .then(
                    ctrl(':')
                        .ignore_then(ty.clone())
                        .or_not()
                        .map_with(|ty, e| {
                            ty.unwrap_or_else(|| Ty::new_with_span(TyKind::Tuple(vec![]), e.span()))
                        }),
                )
                .map(|(name, ty)| TyEnumVariant { name, ty })
                .separated_by(ctrl(','))
                .allow_trailing()
                .collect()
                .map(TyKind::Enum),
            |_| TyKind::Enum(vec![]),
        ))
        .labelled("enum");

    let array =
        delimited_by_brackets(ty.map(Box::new).map(TyKind::Array), empty_array).labelled("array");

    let base = choice((
        primitive.boxed(),
        ident.boxed(),
        func.boxed(),
        tuple.boxed(),
        array.boxed(),
        enum_.boxed(),
    ))
    .map_with(|kind, e| Ty::new_with_span(kind, e.span()));

    optional_type(base).labelled("type")
}

pub(super) fn func_params<'src, I>(
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Vec<TyFuncParam>, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let param = (keyword("const").or_not())
        .then(ident_part().then_ignore(ctrl(':')).or_not())
        .then(ty.map(Some))
        .then(ctrl('=').ignore_then(expr).map(Box::new).or_not())
        .map_with(|(((constant, label), ty), default), e| TyFuncParam {
            constant: constant.is_some(),
            label,
            ty,
            default,
            span: Some(e.span()),
        });

    delimited_by_parenthesis(
        param.separated_by(ctrl(',')).allow_trailing().collect(),
        |_| vec![],
    )
}

fn optional_type<'src, I>(
    base: impl Parser<'src, I, Ty, PExtra<'src>> + 'src,
) -> impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    base.then(ctrl('?').map_with(|_, e| e.span()).or_not())
        .map(|(inner, question_span)| {
            if let Some(q_span) = question_span {
                let mut span = inner.span.unwrap();
                span.set_end_of(&q_span);
                Ty::new_with_span(TyKind::Option(Box::new(inner)), span)
            } else {
                inner
            }
        })
        .labelled("optional type")
        .boxed()
}

fn empty_array(s: Span) -> TyKind {
    TyKind::Array(Box::new(Ty::new_with_span(TyKind::Tuple(vec![]), s)))
}

fn ty_primitive<'src, I>() -> impl Parser<'src, I, TyPrimitive, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    select! {
        TokenKind::Ident(i) if i == "Prim8" => TyPrimitive::prim8,
        TokenKind::Ident(i) if i == "Prim16" => TyPrimitive::prim16,
        TokenKind::Ident(i) if i == "Prim32" => TyPrimitive::prim32,
        TokenKind::Ident(i) if i == "Prim64" => TyPrimitive::prim64,
    }
}

pub fn type_params<'src, I>(
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Vec<TyParam>, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let tuple = choice((
        ident_part()
            .then_ignore(ctrl(':'))
            .or_not()
            .then(ty.clone())
            .map_with(|t, e| (t, e.span()))
            .separated_by(ctrl(','))
            .at_least(1)
            .collect::<Vec<_>>()
            .then_ignore(ctrl(',').then(just(TokenKind::Range))),
        just(TokenKind::Range).to(vec![]),
    ))
    .delimited_by(ctrl('{'), ctrl('}'))
    .try_map(|mut positional, _span| {
        let first_named = positional
            .iter()
            .position(|((n, _), _)| n.is_some())
            .unwrap_or(positional.len());

        let named = positional.split_off(first_named);

        let mut fields = Vec::new();

        // positional
        for (i, ((_, ty), span)) in positional.into_iter().enumerate() {
            fields.push(TyDomainTupleField {
                location: Lookup::Position(i as i64),
                span,
                ty,
            });
        }

        // named
        for ((name, ty), span) in named {
            let Some(name) = name else {
                return Err(Rich::custom(
                    span,
                    "named field cannot be followed by a positional field",
                ));
            };
            fields.push(TyDomainTupleField {
                location: Lookup::Name(name),
                span,
                ty,
            });
        }
        Ok(TyDomain::TupleHasFields(fields))
    })
    .labelled("tuple domain");

    let domain = ctrl(':')
        .ignore_then(choice((
            tuple,
            ty.separated_by(ctrl('|'))
                .at_least(1)
                .collect::<Vec<_>>()
                .map(TyDomain::OneOf),
        )))
        .or_not()
        .map(|x| x.unwrap_or(TyDomain::Open))
        .labelled("type parameter domain");

    let param = ident_part()
        .then(domain)
        .map_with(|(name, domain), e| TyParam {
            name,
            domain,
            span: Some(e.span()),
        });

    param
        .separated_by(ctrl(','))
        .allow_trailing()
        .at_least(1)
        .collect()
}
