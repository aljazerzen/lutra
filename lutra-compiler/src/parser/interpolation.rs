use chumsky::prelude::*;
use itertools::Itertools;

use crate::Span;
use crate::pr::*;

use super::PError;

/// Parses interpolated strings.
pub(crate) fn parse(
    string: String,
    span_base: Span,
) -> Result<Vec<InterpolateItem>, Vec<PError<'static>>> {
    let (result, errors) = interpolated_parser(span_base)
        .parse(string.as_str())
        .into_output_errors();

    if errors.is_empty() {
        tracing::trace!("interpolated string ok: {:?}", result);
        Ok(result.unwrap_or_default())
    } else {
        let perrors = errors
            .into_iter()
            .map(|err| {
                tracing::debug!("interpolated string error (lex inside parse): {:?}", err);
                let span = absolute_span(*err.span(), span_base);
                Rich::custom(span, err.to_string())
            })
            .collect_vec();
        Err(perrors)
    }
}

fn interpolated_parser<'src>(
    span_base: Span,
) -> impl Parser<'src, &'src str, Vec<InterpolateItem>, extra::Err<Rich<'src, char, SimpleSpan>>> {
    let expr = interpolate_ident_part()
        .map_with(move |name, e| (name, e.span()))
        .separated_by(just('.'))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(move |ident_parts| {
            let mut parts = ident_parts.into_iter();

            let (first, first_span) = parts.next().unwrap();
            let first_span = absolute_span(first_span, span_base);
            let mut base = Box::new(Expr::new_with_span(Path::from_name(first), first_span));

            for (part, span) in parts {
                let span = absolute_span(span, span_base);
                let lookup = Lookup::Name(part);
                let kind = ExprKind::Lookup { base, lookup };
                base = Box::new(Expr::new_with_span(kind, span));
            }
            base
        })
        .labelled("interpolated string variable")
        .then(
            just(':')
                .ignore_then(none_of('}').repeated().collect::<String>())
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

    expr.or(string).repeated().collect().then_ignore(end())
}

fn absolute_span(span: SimpleSpan, base: Span) -> Span {
    Span {
        start: span.start as u32 + base.start,
        len: span.end.saturating_sub(span.start) as u16,
        source_id: base.source_id,
    }
}

pub(crate) fn interpolate_ident_part<'src>()
-> impl Parser<'src, &'src str, String, extra::Err<Rich<'src, char, SimpleSpan>>> + Clone {
    let plain = any()
        .filter(|c: &char| c.is_alphabetic() || *c == '_')
        .then(
            any()
                .filter(|c: &char| c.is_alphanumeric() || *c == '_')
                .repeated()
                .collect::<String>(),
        )
        .map(|(first, rest): (char, String)| {
            let mut s = first.to_string();
            s.push_str(&rest);
            s
        })
        .labelled("interpolated string");

    let backticks = none_of('`')
        .repeated()
        .collect::<String>()
        .delimited_by(just('`'), just('`'))
        .labelled("interp:backticks");

    plain.or(backticks)
}

#[test]
fn parse_interpolate() {
    use insta::assert_debug_snapshot;
    let span_base = crate::Span::new(0, 0..0);

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
