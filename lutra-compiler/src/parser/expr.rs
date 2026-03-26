use chumsky::input::ValueInput;
use chumsky::pratt::*;
use chumsky::prelude::*;

use crate::Span;
use crate::pr::*;

use super::helpers::*;
use super::interpolation;
use super::{PExtra, TokenKind};

pub(crate) fn expr<'src, I>(
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    recursive(|expr| {
        // Box each complex alternative so the 10-way choice sees a uniform
        // Boxed<ExprKind> element type, collapsing Choice<10-tuple>::go into
        // one small monomorphization instead of a 10K-line function.
        let literal = literal().map(ExprKind::Literal).boxed();
        let identified = identified(expr.clone()).boxed();
        let variant = variant(expr.clone()).map(ExprKind::Variant).boxed();
        let func = func(expr.clone(), ty.clone()).boxed();
        let tuple = tuple(expr.clone()).boxed();
        let array = array(expr.clone()).boxed();
        let nested = nested(expr.clone()).boxed();
        let interpolation = interpolation().boxed();
        let match_ = match_(expr.clone()).boxed();
        let if_else = if_else(expr.clone()).boxed();

        let term = choice((
            identified,
            literal,
            tuple,
            nested,
            func,
            match_,
            if_else,
            array,
            interpolation,
            variant,
        ))
        .map_with(|kind, e| Expr::new_with_span(kind, e.span()))
        .boxed(); // prevent MapWith<Choice<10-tuple>> from propagating downstream

        let atom = field_lookup(term).boxed();
        let atom = type_annotation(atom, ty).boxed();

        let expr_ops = unary_and_binary_expr(atom).boxed();
        let expr_with_range = range(expr_ops).boxed();
        binary_op_parser(expr_with_range, ctrl('|').to(BinOp::Pipe).boxed())
    })
}

fn unary_and_binary_expr<'src, I>(
    term: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    // Pratt parser handles all unary and binary operators in a single
    // efficient loop, avoiding the per-level Expr moves of a layered chain.
    //
    // Binding powers (higher = tighter):
    //   prefix 9: unary -/+/!
    //   right   8: **  (pow, right-associative)
    //   left    7: * / // %
    //   left    6: + -
    //   left    5: == != < > <= >= =~
    //   left    4: ??  (coalesce)
    //   left    3: &&  (and)
    //   left    2: ||  (or)
    let expr_ops = term.pratt((
        prefix(9, operator_unary(), |op, rhs: Expr, e| {
            let kind = ExprKind::Unary(UnaryExpr {
                op,
                expr: Box::new(rhs),
            });
            Expr::new_with_span(kind, e.span())
        }),
        infix(right(8), just(TokenKind::Pow).to(BinOp::Pow), new_bin_op),
        infix(left(7), operator_mul(), new_bin_op),
        infix(left(6), operator_add(), new_bin_op),
        infix(left(5), operator_compare(), new_bin_op),
        infix(
            left(4),
            just(TokenKind::Coalesce).to(BinOp::Coalesce),
            new_bin_op,
        ),
        infix(left(3), just(TokenKind::And).to(BinOp::And), new_bin_op),
        infix(left(2), just(TokenKind::Or).to(BinOp::Or), new_bin_op),
    ));
    expr_ops
}

fn literal<'src, I>() -> impl Parser<'src, I, Literal, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    select! { TokenKind::Literal(lit) => lit }
}

fn tuple<'src, I>(
    nested_expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    delimited_by_braces(
        choice((
            // ..expr
            just(TokenKind::Range)
                .ignore_then(nested_expr.clone())
                .map(|expr| TupleField {
                    name: None,
                    unpack: true,
                    expr,
                }),
            // name = expr
            ident_part()
                .then_ignore(ctrl('='))
                .or_not()
                .then(nested_expr)
                .map(|(name, expr)| TupleField {
                    name,
                    unpack: false,
                    expr,
                }),
        ))
        .separated_by(ctrl(','))
        .allow_trailing()
        .collect::<Vec<_>>(),
        |_| vec![],
    )
    .map(ExprKind::Tuple)
    .labelled("tuple")
}

fn array<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    delimited_by_brackets(sequence(expr), |_| vec![])
        .map(ExprKind::Array)
        .labelled("array")
}

fn interpolation<'src, I>() -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    select! {
        TokenKind::Interpolation('f', string) => (ExprKind::FString as fn(_) -> _, string),
    }
    .validate(|(finish, string), e, emitter| {
        let mut span: Span = e.span();
        span.start += 2;
        match interpolation::parse(string, span) {
            Ok(items) => finish(items),
            Err(errors) => {
                for err in errors {
                    // Convert sub-parser errors to the generic error type.
                    // The full error message is only available when E::Error = Rich,
                    // so we use a static fallback that works for EmptyErr too.
                    emitter.emit(Rich::custom(*err.span(), "invalid interpolated string"));
                }
                finish(vec![])
            }
        }
    })
    .labelled("interpolated string")
}

fn match_<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let pattern = recursive(|pattern| {
        let variant = ctrl('.')
            .ignore_then(ident_part())
            .then(
                delimited_by_parenthesis(pattern, |s| {
                    Pattern::new_with_span(PatternKind::AnyOf(vec![]), s)
                })
                .map(Box::new)
                .or_not(),
            )
            .map(|(path, inner)| PatternKind::Enum(path, inner));

        let term = choice((
            // enum variant
            variant,
            // bind
            ident_part().map(PatternKind::Bind),
            // literal
            literal().map(PatternKind::Literal),
        ))
        .map_with(|kind, e| Pattern::new_with_span(kind, e.span()));

        // any of
        term.separated_by(ctrl('|'))
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|terms, e| {
                if terms.len() > 1 {
                    Pattern::new_with_span(PatternKind::AnyOf(terms), e.span())
                } else {
                    terms.into_iter().next().unwrap()
                }
            })
    })
    .labelled("pattern");

    let branch = pattern
        .then_ignore(just(TokenKind::ArrowFat))
        .then(expr.clone().map(Box::new))
        .map(|(pattern, value)| MatchBranch { pattern, value });

    keyword("match")
        .ignore_then(expr.map(Box::new))
        .then(
            branch
                .separated_by(ctrl(','))
                .allow_trailing()
                .at_least(1)
                .collect::<Vec<_>>()
                .delimited_by(ctrl('{'), ctrl('}')),
        )
        .map(|(subject, branches)| ExprKind::Match(Match { subject, branches }))
}

fn field_lookup<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let lookup_step = ctrl('.')
        .ignore_then(choice((
            ident_part().map(Lookup::Name),
            select! {
                TokenKind::Literal(Literal::Number(i)) if i.parse::<u32>().is_ok()
                    => Lookup::Position(i.parse::<i64>().unwrap())
            },
        )))
        .map_with(|f, e| (f, e.span()));

    expr.foldl(lookup_step.repeated(), |base, (lookup, span)| {
        let base = Box::new(base);
        let kind = ExprKind::Lookup { base, lookup };
        Expr::new_with_span(kind, span)
    })
}

fn range<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let end_only = just(TokenKind::Range)
        .ignore_then(expr.clone())
        .map(|end| Range {
            start: None,
            end: Some(Box::new(end)),
        })
        .map(ExprKind::Range)
        .map_with(|kind, e| Expr::new_with_span(kind, e.span()));

    end_only.or(expr
        .clone()
        .then(just(TokenKind::Range).ignore_then(expr.or_not()).or_not())
        .map_with(|(start, range), e| {
            if let Some(end) = range {
                Expr::new_with_span(
                    ExprKind::Range(Range {
                        start: Some(Box::new(start)),
                        end: end.map(Box::new),
                    }),
                    e.span(),
                )
            } else {
                start
            }
        }))
}

fn nested<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let var_bindings = keyword("let")
        .ignore_then(ident_part().map_with(|name, e| (name, e.span())))
        .then_ignore(ctrl('='))
        .then(expr.clone().map(Box::new))
        .then_ignore(ctrl(';'))
        .map_with(|stmt, e| (stmt, e.span()));

    let main = expr.map(Box::new);

    let nested =
        var_bindings
            .repeated()
            .foldr(main, |(((name, name_span), bound), binding_span), main| {
                let mut span = binding_span;
                span.set_end_of(main.span.as_ref().unwrap());

                let kind = ExprKind::VarBinding(VarBinding {
                    name,
                    name_span,
                    bound,
                    main,
                });
                Box::new(Expr::new_with_span(kind, span))
            });

    delimited_by_parenthesis(nested.map(ExprKind::Nested), |_| ExprKind::Tuple(vec![]))
}

fn binary_op_parser<'src, I>(
    term: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
    op: impl Parser<'src, I, BinOp, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let term = term.map_with(|e, ex| (e, ex.span()));
    let rest = op.then(term.clone()).repeated();
    term.foldl(rest, |left, (op, right)| {
        let mut span = left.1;
        span.set_end_of(&right.1);
        let kind = ExprKind::Binary(BinaryExpr {
            left: Box::new(left.0),
            op,
            right: Box::new(right.0),
        });
        (Expr::new_with_span(kind, span), span)
    })
    .map(|(e, _)| e)
}

/// Parses three ident-starting expression forms in a single pass,
/// consuming the leading `ident_part` only once:
///
/// - `path(args)`:    `ExprKind::Call`      (single- or multi-segment)
/// - `name -> body`:  `ExprKind::FuncShort` (single-segment only)
/// - `path`:          `ExprKind::Ident`     (single- or multi-segment)
fn identified<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let call_arg = ident_part()
        .then_ignore(ctrl('='))
        .or_not()
        .then(expr.clone())
        .map_with(|(label, expr), e| CallArg {
            label,
            expr,
            span: Some(e.span()),
        });

    let call_args = delimited_by_parenthesis(
        call_arg.separated_by(ctrl(',')).allow_trailing().collect(),
        |_| vec![],
    );

    // Suffix variants after the first ident_part:
    //   Multi  – one or more `:: segment` continuations, then optional call args
    //   Call   – immediate `( args )` (single-segment call)
    //   Arrow  – `->` then body expression (func_short)
    //   Bare   – nothing more (plain ident)
    enum Suffix {
        Multi {
            rest: Vec<String>,
            rest_span: Span,
            args: Option<Vec<CallArg>>,
        },
        Call(Vec<CallArg>),
        Arrow(Box<Expr>),
        Bare,
    }

    ident_part()
        .map_with(|name, e| (name, e.span()))
        .then(
            // Box each alternative so Choice<4-tuple> sees uniform Boxed<Suffix>
            // elements, eliminating the 13K-line Choice<4-tuple>::go function.
            choice((
                // Multi-segment path: first :: rest... [( args )]
                just(TokenKind::PathSep)
                    .ignore_then(
                        ident_part()
                            .separated_by(just(TokenKind::PathSep))
                            .at_least(1)
                            .collect::<Vec<_>>(),
                    )
                    .map_with(|rest, e| (rest, e.span()))
                    .then(call_args.clone().or_not())
                    .map(|((rest, rest_span), args)| Suffix::Multi {
                        rest,
                        rest_span,
                        args,
                    })
                    .boxed(),
                // Single-segment call: first( args )
                call_args.map(Suffix::Call).boxed(),
                // Short function literal: first -> body
                just(TokenKind::ArrowThin)
                    .ignore_then(expr.map(Box::new))
                    .map(Suffix::Arrow)
                    .boxed(),
                // Bare ident (single segment)
                empty().map(|_| Suffix::Bare).boxed(),
            )),
        )
        .map_with(|((first_name, first_span), suffix), _| match suffix {
            Suffix::Multi {
                rest,
                rest_span,
                args,
            } => {
                let mut path = Path::from_name(first_name);
                for seg in rest {
                    path.push(seg);
                }
                let mut path_span = first_span;
                path_span.set_end(rest_span.end());
                if let Some(args) = args {
                    let subject = Box::new(Expr::new_with_span(ExprKind::Ident(path), path_span));
                    ExprKind::Call(Call { subject, args })
                } else {
                    ExprKind::Ident(path)
                }
            }
            Suffix::Call(args) => {
                let subject = Box::new(Expr::new_with_span(
                    ExprKind::Ident(Path::from_name(first_name)),
                    first_span,
                ));
                ExprKind::Call(Call { subject, args })
            }
            Suffix::Arrow(body) => {
                let param = FuncParam {
                    constant: false,
                    label: None,
                    name: first_name,
                    ty: None,
                    span: first_span,
                };
                ExprKind::FuncShort(Box::new(FuncShort { param, body }))
            }
            Suffix::Bare => ExprKind::Ident(Path::from_name(first_name)),
        })
}

fn func<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    keyword("func")
        .ignore_then(delimited_by_parenthesis(
            func_param(ty.clone())
                .separated_by(ctrl(','))
                .allow_trailing()
                .collect(),
            |_| vec![],
        ))
        .then(ctrl(':').ignore_then(ty.clone()).or_not())
        .then_ignore(just(TokenKind::ArrowThin))
        .then(expr.map(Box::new).map(Some))
        .map(|((params, return_ty), body)| {
            Box::new(Func {
                params,
                return_ty,
                body,
                ty_params: Vec::new(),
            })
        })
        .map(ExprKind::Func)
        .labelled("function")
}

pub fn func_param<'src, I>(
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, FuncParam, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    (keyword("const").or_not().map(|x| x.is_some()))
        .then(
            ident_part()
                .then(ident_part().or_not())
                .map(|(a, b)| match b {
                    Some(b) => (Some(a), b),
                    None => (None, a),
                }),
        )
        .then(ctrl(':').ignore_then(ty.clone()).or_not())
        .map_with(|((constant, (label, name)), ty), e| FuncParam {
            constant,
            label,
            name,
            ty,
            span: e.span(),
        })
}

pub(crate) fn ident<'src, I>() -> impl Parser<'src, I, Path, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    ident_part()
        .separated_by(just(TokenKind::PathSep))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(Path::new::<String, Vec<String>>)
}

pub fn variant<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Variant, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    ctrl('.')
        .ignore_then(ident_part())
        .then(
            delimited_by_parenthesis(expr, |s| Expr::new_with_span(ExprKind::Tuple(vec![]), s))
                .map(Box::new)
                .or_not(),
        )
        .map(|(name, inner)| Variant { name, inner })
}

fn operator_unary<'src, I>() -> impl Parser<'src, I, UnOp, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    (ctrl('+').to(UnOp::Pos))
        .or(ctrl('-').to(UnOp::Neg))
        .or(ctrl('!').to(UnOp::Not))
}
fn operator_mul<'src, I>() -> impl Parser<'src, I, BinOp, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    (just(TokenKind::DivInt).to(BinOp::DivInt))
        .or(ctrl('*').to(BinOp::Mul))
        .or(ctrl('/').to(BinOp::DivFloat))
        .or(ctrl('%').to(BinOp::Mod))
}
fn operator_add<'src, I>() -> impl Parser<'src, I, BinOp, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    (ctrl('+').to(BinOp::Add)).or(ctrl('-').to(BinOp::Sub))
}
fn operator_compare<'src, I>() -> impl Parser<'src, I, BinOp, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    choice((
        just(TokenKind::Eq).to(BinOp::Eq),
        just(TokenKind::Ne).to(BinOp::Ne),
        just(TokenKind::Lte).to(BinOp::Lte),
        just(TokenKind::Gte).to(BinOp::Gte),
        just(TokenKind::RegexSearch).to(BinOp::RegexSearch),
        ctrl('<').to(BinOp::Lt),
        ctrl('>').to(BinOp::Gt),
    ))
}
fn type_annotation<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    expr.then(ctrl(':').ignore_then(ty).or_not())
        .map_with(|(expr, ty), e| {
            if let Some(ty) = ty {
                let expr = Box::new(expr);
                let ty = Box::new(ty);
                let kind = ExprKind::TypeAnnotation(TypeAnnotation { expr, ty });
                Expr::new_with_span(kind, e.span())
            } else {
                expr
            }
        })
}
fn if_else<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, ExprKind, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let expr = expr.map(Box::new);

    keyword("if")
        .ignore_then(expr.clone())
        .then_ignore(keyword("then"))
        .then(expr.clone())
        .then_ignore(keyword("else"))
        .then(expr)
        .map(|((condition, then), els)| {
            ExprKind::If(If {
                condition,
                then,
                els,
            })
        })
}

fn new_bin_op<'src, 't, I>(
    l: Expr,
    op: BinOp,
    r: Expr,
    e: &mut chumsky::input::MapExtra<'src, 't, I, PExtra<'src>>,
) -> Expr
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    Expr::new_with_span(
        ExprKind::Binary(BinaryExpr {
            left: Box::new(l),
            op,
            right: Box::new(r),
        }),
        e.span(),
    )
}
