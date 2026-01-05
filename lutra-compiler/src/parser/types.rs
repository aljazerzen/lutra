use chumsky::prelude::*;

use super::expr::ident;
use super::perror::PError;
use super::*;

use crate::parser::lexer::TokenKind;
use crate::pr::*;

pub(crate) fn type_expr() -> impl Parser<TokenKind, Ty, Error = PError> + Clone {
    recursive(|nested_type_expr| {
        let primitive = primitive_set().map(TyKind::Primitive);

        let ident = ident().map(TyKind::Ident);

        let func_params = delimited_by_parenthesis(
            (ident_part().then_ignore(ctrl(':')).or_not())
                .ignore_then(nested_type_expr.clone().map(Some))
                .map(|ty| TyFuncParam {
                    constant: false,
                    label: None,
                    ty,
                })
                .separated_by(ctrl(','))
                .allow_trailing(),
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

        let tuple = delimited_by_braces(
            sequence(
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
            .map(TyKind::Tuple),
        );

        let tuple_comprehension = delimited_by_braces(
            keyword("for")
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
                .map(TyKind::TupleComprehension),
        );
        let tuple = tuple_comprehension.or(tuple);

        let enum_ = keyword("enum")
            .ignore_then(
                // variant name
                ident_part()
                    .then(
                        // inner type
                        ctrl(':')
                            .ignore_then(nested_type_expr.clone())
                            .or_not()
                            .map_with_span(|ty, span| {
                                ty.unwrap_or_else(|| Ty::new_with_span(TyKind::Tuple(vec![]), span))
                            }),
                    )
                    .map(|(name, ty)| TyEnumVariant { name, ty })
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
                    )),
            )
            .map(TyKind::Enum)
            .labelled("enum");

        let array = nested_type_expr
            .map(Box::new)
            .delimited_by(ctrl('['), ctrl(']'))
            .recover_with(nested_delimiters(
                TokenKind::Control('['),
                TokenKind::Control(']'),
                [
                    (TokenKind::Control('{'), TokenKind::Control('}')),
                    (TokenKind::Control('('), TokenKind::Control(')')),
                    (TokenKind::Control('['), TokenKind::Control(']')),
                ],
                |_| Box::new(Ty::new(TyKind::Tuple(vec![]))),
            ))
            .map(TyKind::Array)
            .labelled("array");

        // exclude
        // term.clone()
        //     .then(ctrl('-').ignore_then(term).repeated())
        //     .foldl(|left, right| {
        //         let left_span = left.span.as_ref().unwrap();
        //         let right_span = right.span.as_ref().unwrap();
        //         let span = Span {
        //             start: left_span.start,
        //             end: right_span.end,
        //             source_id: left_span.source_id,
        //         };

        //         let kind = TyKind::Exclude {
        //             base: Box::new(left),
        //             except: Box::new(right),
        //         };
        //         TyKind::into_ty(kind, span)
        //     })

        choice((primitive, ident, func, tuple, array, enum_))
            .map_with_span(Ty::new_with_span)
            .boxed()
    })
    .labelled("type")
}

fn delimited_by_braces(
    tuple_contents: impl Parser<TokenKind, TyKind, Error = PError>,
) -> impl Parser<TokenKind, TyKind, Error = PError> {
    tuple_contents
        .delimited_by(ctrl('{'), ctrl('}'))
        .recover_with(nested_delimiters(
            TokenKind::Control('{'),
            TokenKind::Control('}'),
            [
                (TokenKind::Control('{'), TokenKind::Control('}')),
                (TokenKind::Control('('), TokenKind::Control(')')),
                (TokenKind::Control('['), TokenKind::Control(']')),
            ],
            |_| TyKind::Tuple(vec![]),
        ))
        .labelled("tuple")
}

fn primitive_set() -> impl Parser<TokenKind, TyPrimitive, Error = PError> {
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

pub fn type_params<'a>(
    ty: impl Parser<TokenKind, Ty, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, Vec<TyParam>, Error = PError> + 'a {
    let tuple = choice((
        ident_part()
            .then_ignore(ctrl(':'))
            .or_not()
            .then(ty.clone())
            .map_with_span(|t, s| (t, s))
            .separated_by(ctrl(','))
            .at_least(1)
            .then_ignore(ctrl(',').then(just(TokenKind::Range))),
        just(TokenKind::Range).to(vec![]),
    ))
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
    .try_map(|mut positional, _| {
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
                return Err(PError::custom(
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

    let one_of_numbers = ident_keyword("number").map_with_span(|_, s| {
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

    let one_of_primitives = ident_keyword("primitive").map_with_span(|_, s| {
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

    // domain
    let domain = ctrl(':')
        .ignore_then(choice((
            // ctrl('*').to(TyParamDomain::Open),
            tuple,
            one_of_primitives,
            one_of_numbers,
            ty.separated_by(ctrl('|')).at_least(1).map(TyDomain::OneOf),
        )))
        .or_not()
        .map(|x| x.unwrap_or(TyDomain::Open))
        .labelled("type parameter domain");

    // param name
    let param = ident_part()
        .then(domain)
        .map_with_span(|(name, domain), span| TyParam {
            name,
            domain,
            span: Some(span),
        });

    param.separated_by(ctrl(',')).allow_trailing().at_least(1)
}
