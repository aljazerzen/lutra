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

        let func_params = (ident_part().then_ignore(ctrl(':')).or_not())
            .ignore_then(nested_type_expr.clone().map(Some))
            .separated_by(ctrl(','))
            .allow_trailing()
            .delimited_by(ctrl('('), ctrl(')'));

        let func = keyword("func")
            .ignore_then(
                type_params(nested_type_expr.clone())
                    .then(func_params)
                    .then_ignore(ctrl(':'))
                    .then(nested_type_expr.clone().map(Box::new).map(Some))
                    .map(|((type_params, params), body)| TyFunc {
                        params,
                        body,
                        ty_params: type_params,
                    }),
            )
            .map(TyKind::Func);

        let tuple = sequence(choice((ident_part()
            .then_ignore(ctrl('='))
            .or_not()
            .then(nested_type_expr.clone())
            .map(|(name, ty)| TyTupleField { name, ty }),)))
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
        .map(TyKind::Tuple)
        .labelled("tuple");

        let enum_ = keyword("enum")
            .ignore_then(
                sequence(
                    ident_part()
                        .then(
                            ctrl('=')
                                .ignore_then(nested_type_expr.clone())
                                .or_not()
                                .map(|ty| ty.unwrap_or_else(|| Ty::new(TyKind::Tuple(vec![])))),
                        )
                        .map(|(name, ty)| TyEnumVariant { name, ty }),
                )
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

        let term = choice((primitive, ident, func, tuple, array, enum_))
            .map_with_span(Ty::new_with_span)
            .boxed();

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
        term
    })
    .labelled("type")
}

fn primitive_set() -> impl Parser<TokenKind, TyPrimitive, Error = PError> {
    select! {
        TokenKind::Ident(i) if i == "int8" => TyPrimitive::int8,
        TokenKind::Ident(i) if i == "int16" => TyPrimitive::int16,
        TokenKind::Ident(i) if i == "int32" => TyPrimitive::int32,
        TokenKind::Ident(i) if i == "int64" => TyPrimitive::int64,
        TokenKind::Ident(i) if i == "int" => TyPrimitive::int64,
        TokenKind::Ident(i) if i == "uint8" => TyPrimitive::uint8,
        TokenKind::Ident(i) if i == "uint16" => TyPrimitive::uint16,
        TokenKind::Ident(i) if i == "uint32" => TyPrimitive::uint32,
        TokenKind::Ident(i) if i == "uint64" => TyPrimitive::uint64,
        TokenKind::Ident(i) if i == "float32" => TyPrimitive::float32,
        TokenKind::Ident(i) if i == "float64" => TyPrimitive::float64,
        TokenKind::Ident(i) if i == "float" => TyPrimitive::float64,
        TokenKind::Ident(i) if i == "bool"=> TyPrimitive::bool,
        TokenKind::Ident(i) if i == "text"=> TyPrimitive::text,
    }
}

pub fn type_params<'a>(
    ty: impl Parser<TokenKind, Ty, Error = PError> + 'a,
) -> impl Parser<TokenKind, Vec<TyParam>, Error = PError> + Clone + 'a {
    let tuple = ident_part()
        .then_ignore(ctrl('='))
        .or_not()
        .then(ty)
        .map(|(name, ty)| TyDomainTupleField { name, ty })
        .separated_by(ctrl(','))
        .then_ignore(ctrl(',').then(just(TokenKind::Range)))
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
        .map(TyParamDomain::TupleFields)
        .labelled("tuple domain");

    // domain
    let domain = ctrl(':')
        .ignore_then(choice((
            tuple,
            primitive_set()
                .separated_by(ctrl('|'))
                .at_least(1)
                .map(TyParamDomain::OneOf),
        )))
        .or_not()
        .map(|x| x.unwrap_or(TyParamDomain::Open))
        .labelled("type parameter domain");

    // param name
    let param = ident_part()
        .then(domain)
        .map_with_span(|(name, domain), span| TyParam {
            name,
            domain,
            span: Some(span),
        });

    param
        .separated_by(ctrl(','))
        .allow_trailing()
        .at_least(1)
        .delimited_by(ctrl('<'), ctrl('>'))
        .or_not()
        .map(|x| x.unwrap_or_default())
        .boxed()
}
