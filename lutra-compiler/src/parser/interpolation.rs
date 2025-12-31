use chumsky::prelude::*;
use itertools::Itertools;

use crate::codespan::Span;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::{ChumError, PError};
use crate::pr::*;

/// Parses interpolated strings
pub(crate) fn parse(string: String, span_base: Span) -> Result<Vec<InterpolateItem>, Vec<PError>> {
    let prepped_stream = prepare_stream(&string, span_base);

    let res = interpolated_parser().parse(prepped_stream);

    match res {
        Ok(items) => {
            tracing::trace!("interpolated string ok: {:?}", items);
            Ok(items)
        }
        Err(errors) => Err(errors
            .into_iter()
            .map(|err| {
                tracing::debug!("interpolated string error (lex inside parse): {:?}", err);
                err.map(|c| TokenKind::Literal(Literal::Text(c.to_string())))
            })
            .collect_vec()),
    }
}

pub(crate) fn prepare_stream<'a>(
    source: &'a str,
    span_base: Span,
) -> chumsky::Stream<'a, char, Span, Box<dyn Iterator<Item = (char, Span)> + 'a>> {
    let iter = super::lexer::CharIterator::new(source);

    chumsky::Stream::from_iter(
        Span {
            start: span_base.start + source.len() as u32,
            len: 0,
            source_id: span_base.source_id,
        },
        Box::new(iter.map(move |(c, mut span)| {
            span.start += span_base.start;
            (c, span.with_source_id(span_base.source_id))
        })),
    )
}

fn interpolated_parser() -> impl Parser<char, Vec<InterpolateItem>, Error = ChumError<char>> {
    let expr = interpolate_ident_part()
        .map_with_span(move |name, s| (name, s))
        .separated_by(just('.'))
        .at_least(1)
        .map(|ident_parts| {
            let mut parts = ident_parts.into_iter();

            let (first, first_span) = parts.next().unwrap();
            let mut base = Box::new(Expr::new_with_span(Path::from_name(first), first_span));

            for (part, span) in parts {
                let lookup = Lookup::Name(part);
                let kind = ExprKind::Lookup { base, lookup };
                base = Box::new(Expr::new_with_span(kind, span));
            }
            base
        })
        .labelled("interpolated string variable")
        .then(
            just(':')
                .ignore_then(filter(|c| *c != '}').repeated().collect::<String>())
                .or_not(),
        )
        .delimited_by(just('{'), just('}'))
        .map(|(expr, format)| InterpolateItem::Expr { expr, format });

    // Convert double braces to single braces, and fail on any single braces.
    let string = just("{{")
        .to('{')
        .or(just("}}").to('}'))
        .or(none_of("{}"))
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(InterpolateItem::String);

    expr.or(string).repeated().then_ignore(end())
}

pub(crate) fn interpolate_ident_part() -> impl Parser<char, String, Error = ChumError<char>> + Clone
{
    let plain = filter(|c: &char| c.is_alphabetic() || *c == '_')
        .chain(filter(|c: &char| c.is_alphanumeric() || *c == '_').repeated())
        .labelled("interpolated string");

    let backticks = none_of('`').repeated().delimited_by(just('`'), just('`'));

    plain.or(backticks.labelled("interp:backticks")).collect()
}

#[test]
fn parse_interpolate() {
    use insta::assert_debug_snapshot;
    let span_base = Span::new(0, 0..0);

    assert_debug_snapshot!(
        parse("concat({a})".to_string(), span_base).unwrap(),
    @r#"
    [
        String(
            "concat(",
        ),
        Expr {
            expr: Expr {
                kind: Ident(
                    a,
                ),
                span: Some(
                    0:8-9,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
            format: None,
        },
        String(
            ")",
        ),
    ]
    "#);

    assert_debug_snapshot!(
        parse("print('{{hello}}')".to_string(), span_base).unwrap(),
    @r###"
    [
        String(
            "print('{hello}')",
        ),
    ]
    "###);

    assert_debug_snapshot!(
        parse("concat('{{', a, '}}')".to_string(), span_base).unwrap(),
    @r###"
    [
        String(
            "concat('{', a, '}')",
        ),
    ]
    "###);

    assert_debug_snapshot!(
        parse("concat('{{', {a}, '}}')".to_string(), span_base).unwrap(),
    @r#"
    [
        String(
            "concat('{', ",
        ),
        Expr {
            expr: Expr {
                kind: Ident(
                    a,
                ),
                span: Some(
                    0:14-15,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
            format: None,
        },
        String(
            ", '}')",
        ),
    ]
    "#);
}
