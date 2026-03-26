use chumsky::input::ValueInput;
use chumsky::prelude::*;

use super::expr;
use super::helpers::*;
use super::{PExtra, TokenKind};

use crate::Span;
use crate::pr::*;

pub(crate) fn type_expr<'src, I>() -> impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    recursive(|nested_type_expr| {
        let primitive = primitive_set().map(TyKind::Primitive);

        let ident = expr::ident().map(TyKind::Ident);

        let func_params = delimited_by_parenthesis(
            (ident_part().then_ignore(ctrl(':')).or_not())
                .ignore_then(nested_type_expr.clone().map(Some))
                .map(|ty| TyFuncParam {
                    constant: false,
                    label: None,
                    ty,
                })
                .separated_by(ctrl(','))
                .allow_trailing()
                .collect(),
            |_| vec![],
        );

        let func = keyword("func")
            .ignore_then(
                func_params
                    .then_ignore(ctrl(':'))
                    .then(nested_type_expr.clone().map(Box::new).map(Some))
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
            .then(nested_type_expr.clone().map(Box::new))
            .then_ignore(keyword("do"))
            .then(ident_part().then_ignore(ctrl(':')).or_not())
            .then(nested_type_expr.clone().map(Box::new))
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
                .then(nested_type_expr.clone())
                .map(|(name, ty)| TyTupleField {
                    name,
                    ty,
                    unpack: false,
                }),
        )
        .map(TyKind::Tuple);

        let tuple =
            delimited_by_braces(comprehension_body.or(tuple_body), |_| TyKind::Tuple(vec![]))
                .labelled("tuple");

        let enum_ = keyword("enum")
            .ignore_then(delimited_by_braces(
                ident_part()
                    .then(
                        ctrl(':')
                            .ignore_then(nested_type_expr.clone())
                            .or_not()
                            .map_with(|ty, e| {
                                ty.unwrap_or_else(|| {
                                    Ty::new_with_span(TyKind::Tuple(vec![]), e.span())
                                })
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

        let array = delimited_by_brackets(
            nested_type_expr.map(Box::new).map(TyKind::Array),
            empty_array,
        )
        .labelled("array");

        // Box each alternative so Choice<6-tuple> sees uniform Boxed<TyKind>
        // elements, collapsing the 20K-line Choice<6-tuple>::go monomorphization.
        choice((
            primitive.boxed(),
            ident.boxed(),
            func.boxed(),
            tuple.boxed(),
            array.boxed(),
            enum_.boxed(),
        ))
        .map_with(|kind, e| Ty::new_with_span(kind, e.span()))
        .boxed() // prevent MapWith<Choice<6-tuple>> propagating out of recursive
    })
    .labelled("type")
}

fn empty_array(s: Span) -> TyKind {
    TyKind::Array(Box::new(Ty::new_with_span(TyKind::Tuple(vec![]), s)))
}

fn primitive_set<'src, I>() -> impl Parser<'src, I, TyPrimitive, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    select! {
        TokenKind::Ident(i) if i == "int8" => TyPrimitive::int8,
        TokenKind::Ident(i) if i == "int16" => TyPrimitive::int16,
        TokenKind::Ident(i) if i == "int32" => TyPrimitive::int32,
        TokenKind::Ident(i) if i == "int64" => TyPrimitive::int64,
        TokenKind::Ident(i) if i == "uint8" => TyPrimitive::uint8,
        TokenKind::Ident(i) if i == "uint16" => TyPrimitive::uint16,
        TokenKind::Ident(i) if i == "uint32" => TyPrimitive::uint32,
        TokenKind::Ident(i) if i == "uint64" => TyPrimitive::uint64,
        TokenKind::Ident(i) if i == "float32" => TyPrimitive::float32,
        TokenKind::Ident(i) if i == "float64" => TyPrimitive::float64,
        TokenKind::Ident(i) if i == "bool"=> TyPrimitive::bool,
        TokenKind::Ident(i) if i == "text"=> TyPrimitive::text,
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

    let one_of_numbers = ident_keyword("number").map_with(|_, e| {
        let s = e.span();
        TyDomain::OneOf(vec![
            Ty::new_with_span(TyPrimitive::int8, s),
            Ty::new_with_span(TyPrimitive::int16, s),
            Ty::new_with_span(TyPrimitive::int32, s),
            Ty::new_with_span(TyPrimitive::int64, s),
            Ty::new_with_span(TyPrimitive::uint8, s),
            Ty::new_with_span(TyPrimitive::uint16, s),
            Ty::new_with_span(TyPrimitive::uint32, s),
            Ty::new_with_span(TyPrimitive::uint64, s),
            Ty::new_with_span(TyPrimitive::float32, s),
            Ty::new_with_span(TyPrimitive::float64, s),
        ])
    });

    let one_of_primitives = ident_keyword("primitive").map_with(|_, e| {
        let s = e.span();
        TyDomain::OneOf(vec![
            Ty::new_with_span(TyPrimitive::bool, s),
            Ty::new_with_span(TyPrimitive::int8, s),
            Ty::new_with_span(TyPrimitive::int16, s),
            Ty::new_with_span(TyPrimitive::int32, s),
            Ty::new_with_span(TyPrimitive::int64, s),
            Ty::new_with_span(TyPrimitive::uint8, s),
            Ty::new_with_span(TyPrimitive::uint16, s),
            Ty::new_with_span(TyPrimitive::uint32, s),
            Ty::new_with_span(TyPrimitive::uint64, s),
            Ty::new_with_span(TyPrimitive::float32, s),
            Ty::new_with_span(TyPrimitive::float64, s),
            Ty::new_with_span(TyPrimitive::text, s),
        ])
    });

    let domain = ctrl(':')
        .ignore_then(choice((
            tuple,
            one_of_primitives,
            one_of_numbers,
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
