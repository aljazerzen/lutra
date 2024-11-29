use chumsky::prelude::*;

use super::expr::ident;
use super::perror::PError;
use super::*;

use crate::parser::lexer::TokenKind;
use crate::pr::*;

pub(crate) fn type_expr() -> impl Parser<TokenKind, Ty, Error = PError> + Clone {
    recursive(|nested_type_expr| {
        let basic = select! {
            TokenKind::Ident(i) if i == "int8" => PrimitiveSet::int8,
            TokenKind::Ident(i) if i == "int16" => PrimitiveSet::int16,
            TokenKind::Ident(i) if i == "int32" => PrimitiveSet::int32,
            TokenKind::Ident(i) if i == "int64" => PrimitiveSet::int64,
            TokenKind::Ident(i) if i == "uint8" => PrimitiveSet::uint8,
            TokenKind::Ident(i) if i == "uint16" => PrimitiveSet::uint16,
            TokenKind::Ident(i) if i == "uint32" => PrimitiveSet::uint32,
            TokenKind::Ident(i) if i == "uint64" => PrimitiveSet::uint64,
            TokenKind::Ident(i) if i == "float32" => PrimitiveSet::float32,
            TokenKind::Ident(i) if i == "float64" => PrimitiveSet::float64,
            TokenKind::Ident(i) if i == "bool"=> PrimitiveSet::bool,
            TokenKind::Ident(i) if i == "text"=> PrimitiveSet::text,
        }
        .map(TyKind::Primitive);

        let ident = ident().map(TyKind::Ident);

        let func = keyword("func")
            .ignore_then(
                nested_type_expr
                    .clone()
                    .map(Some)
                    .repeated()
                    .then_ignore(just(TokenKind::ArrowThin))
                    .then(nested_type_expr.clone().map(Box::new).map(Some))
                    .map(|(params, return_ty)| TyFunc {
                        params,
                        body: return_ty,
                    })
                    .or_not(),
            )
            .map(TyKind::Function);

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

        let term = choice((basic, ident, func, tuple, array, enum_))
            .map_with_span(TyKind::into_ty)
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
    .labelled("type expression")
}
