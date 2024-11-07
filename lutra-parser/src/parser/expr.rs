use std::collections::{hash_map::Entry, HashMap};

use chumsky::prelude::*;
use itertools::Itertools;

use crate::parser::interpolation;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::types::type_expr;
use crate::parser::{ctrl, ident_part, keyword, new_line, sequence};
use crate::pr::*;
use crate::span::Span;

use super::pipe;

pub(crate) fn expr_call() -> impl Parser<TokenKind, Expr, Error = PError> + Clone {
    let expr = expr();

    choice((
        lambda_func(expr.clone()),
        func_call(expr.clone()),
        pipeline(expr),
    ))
}

pub(crate) fn expr() -> impl Parser<TokenKind, Expr, Error = PError> + Clone {
    recursive(|expr| {
        let literal = select! { TokenKind::Literal(lit) => ExprKind::Literal(lit) };

        let ident_kind = ident().map(ExprKind::Ident);

        let internal = keyword("internal")
            .ignore_then(ident())
            .map(|x| x.to_string())
            .map(ExprKind::Internal);

        let nested_expr = lambda_func(expr.clone())
            .or(func_call(expr.clone()))
            .boxed();

        let tuple = tuple(nested_expr.clone());
        let array = array(nested_expr.clone());
        let pipeline_expr = pipeline(nested_expr.clone())
            .padded_by(new_line().repeated())
            .delimited_by(ctrl('('), ctrl(')'));
        let interpolation = interpolation();
        let case = case(expr.clone());

        let param = select! { TokenKind::Param(id) => ExprKind::Param(id) };

        let term = choice((
            literal,
            internal,
            tuple,
            array,
            interpolation,
            ident_kind,
            case,
            param,
        ))
        .map_with_span(ExprKind::into_expr)
        // No longer used given the TODO in `pipeline`; can remove if we
        // don't resolve.
        // .or(aliased(expr.clone()))
        .or(pipeline_expr)
        .boxed();

        let term = field_lookup(term);
        let term = unary(term);
        let term = range(term);

        // Binary operators
        let expr = term;
        let expr = binary_op_parser_right(expr, operator_pow());
        let expr = binary_op_parser(expr, operator_mul());
        let expr = binary_op_parser(expr, operator_add());
        let expr = binary_op_parser(expr, operator_compare());
        let expr = binary_op_parser(expr, operator_coalesce());
        let expr = binary_op_parser(expr, operator_and());

        binary_op_parser(expr, operator_or())
    })
}

fn tuple<'a>(
    nested_expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
    sequence(maybe_aliased(nested_expr))
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
    sequence(expr)
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

fn interpolation() -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone {
    select! {
        TokenKind::Interpolation('f', string) => (ExprKind::FString as fn(_) -> _, string),
    }
    .validate(
        |(finish, string), span: Span, emit| match interpolation::parse(string, span + 2) {
            Ok(items) => finish(items),
            Err(errors) => {
                for err in errors {
                    emit(err)
                }
                finish(vec![])
            }
        },
    )
    .labelled("interpolated string")
}

fn case<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
    // The `nickname != null => nickname,` part
    let mapping = func_call(expr.clone())
        .map(Box::new)
        .then_ignore(just(TokenKind::ArrowFat))
        .then(func_call(expr).map(Box::new))
        .map(|(condition, value)| SwitchCase { condition, value });

    keyword("case")
        .ignore_then(sequence(mapping).delimited_by(ctrl('['), ctrl(']')))
        .map(ExprKind::Case)
}

fn unary<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    expr.clone()
        .or(operator_unary()
            .then(expr.map(Box::new))
            .map(|(op, expr)| ExprKind::Unary(UnaryExpr { op, expr }))
            .map_with_span(ExprKind::into_expr))
        .boxed()
}

fn field_lookup<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    expr.then(
        ctrl('.')
            .ignore_then(choice((
                ident_part().map(IndirectionKind::Name),
                ctrl('*').to(IndirectionKind::Star),
                select! {
                    TokenKind::Literal(Literal::Integer(i)) => IndirectionKind::Position(i)
                },
            )))
            .map_with_span(|f, s| (f, s))
            .repeated(),
    )
    .foldl(|base, (field, span)| {
        let base = Box::new(base);
        ExprKind::Indirection { base, field }.into_expr(span)
    })
}

fn range<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    // Ranges have five cases we need to parse:
    // x..y (bounded)
    // x..  (only start bound)
    // x    (no-op)
    //  ..y (only end bound)
    //  ..  (unbounded)
    #[derive(Clone)]
    enum RangeCase {
        NoOp(Expr),
        Range(Option<Expr>, Option<Expr>),
    }
    choice((
        // with start bound (first 3 cases)
        expr.clone()
            .then(choice((
                // range and end bound
                just(TokenKind::range(true, true))
                    .ignore_then(expr.clone())
                    .map(|x| Some(Some(x))),
                // range and no end bound
                select! { TokenKind::Range { bind_left: true, .. } => Some(None) },
                // no range
                empty().to(None),
            )))
            .map(|(start, range)| {
                if let Some(end) = range {
                    RangeCase::Range(Some(start), end)
                } else {
                    RangeCase::NoOp(start)
                }
            }),
        // only end bound
        select! { TokenKind::Range { bind_right: true, .. } => () }
            .ignore_then(expr)
            .map(|range| RangeCase::Range(None, Some(range))),
        // unbounded
        select! { TokenKind::Range { .. } => RangeCase::Range(None, None) },
    ))
    .map_with_span(|case, span| match case {
        RangeCase::NoOp(x) => x,
        RangeCase::Range(start, end) => {
            let kind = ExprKind::Range(Range {
                start: start.map(Box::new),
                end: end.map(Box::new),
            });
            kind.into_expr(span)
        }
    })
}

/// A pipeline of `expr`, separated by pipes. Doesn't require parentheses.
pub(crate) fn pipeline<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    // expr has to be a param, because it can be either a normal expr() or a
    // recursive expr called from within expr(), which causes a stack overflow

    // TODO: do we need the `maybe_aliased` here rather than in `expr`? We had
    // tried `with_doc_comment(expr)` in #4775 (and push an aliased expr into
    // `expr`) but couldn't get it work.
    maybe_aliased(expr)
        .separated_by(pipe())
        .at_least(1)
        .map_with_span(|exprs, span| {
            // If there's only one expr, then we don't need to wrap it
            // in a pipeline — just return the lone expr. Otherwise,
            // wrap them in a pipeline.
            exprs.into_iter().exactly_one().unwrap_or_else(|exprs| {
                ExprKind::Pipeline(Pipeline {
                    exprs: exprs.collect(),
                })
                .into_expr(span)
            })
        })
        .recover_with(nested_delimiters(
            TokenKind::Control('('),
            TokenKind::Control(')'),
            [
                (TokenKind::Control('['), TokenKind::Control(']')),
                (TokenKind::Control('('), TokenKind::Control(')')),
            ],
            |_| Expr::new(ExprKind::Tuple(vec![])),
        ))
        .labelled("pipeline")
}

fn binary_op_parser<'a, Term, Op>(
    term: Term,
    op: Op,
) -> impl Parser<TokenKind, Expr, Error = PError> + 'a + Clone
where
    Term: Parser<TokenKind, Expr, Error = PError> + 'a + Clone,
    Op: Parser<TokenKind, BinOp, Error = PError> + 'a + Clone,
{
    let term = term.map_with_span(|e, s| (e, s)).boxed();

    term.clone()
        .then(op.then(term).repeated())
        .foldl(|left, (op, right)| {
            let span = Span {
                start: left.1.start,
                end: right.1.end,
                source_id: left.1.source_id,
            };
            let kind = ExprKind::Binary(BinaryExpr {
                left: Box::new(left.0),
                op,
                right: Box::new(right.0),
            });
            (ExprKind::into_expr(kind, span), span)
        })
        .map(|(e, _)| e)
        .boxed()
}

pub(crate) fn binary_op_parser_right<'a, Term, Op>(
    term: Term,
    op: Op,
) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    Term: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
    Op: Parser<TokenKind, BinOp, Error = PError> + Clone + 'a,
{
    let term = term.map_with_span(|e, s| (e, s)).boxed();

    (term.clone())
        .then(op.then(term).repeated())
        .map(|(first, others)| {
            // A transformation from this:
            // ```
            // first: e1
            // others: [(op1 e2) (op2 e3)]
            // ```
            // ... into:
            // ```
            // r: [(e1 op1) (e2 op2)]
            // e3
            // ```
            // .. so we can use foldr for right associativity.
            // We could use `(term.then(op)).repeated().then(term)` instead,
            // and have the correct structure from the get-go, but that would
            // perform miserably with simple expressions without operators, because
            // it would re-parse the term twice for each level of precedence we have.

            let mut free = first;
            let mut r = Vec::new();
            for (op, expr) in others {
                r.push((free, op));
                free = expr;
            }
            (r, free)
        })
        .foldr(|(left, op), right| {
            let span = Span {
                start: left.1.start,
                end: right.1.end,
                source_id: left.1.source_id,
            };
            let kind = ExprKind::Binary(BinaryExpr {
                left: Box::new(left.0),
                op,
                right: Box::new(right.0),
            });
            (kind.into_expr(span), span)
        })
        .map(|(e, _)| e)
        .boxed()
}

// Can remove if we don't end up using this
#[allow(dead_code)]
fn aliased<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    let aliased = ident_part()
        .then_ignore(ctrl('='))
        .then(expr)
        .map(|(alias, mut expr)| {
            expr.alias = Some(alias);
            expr
        });
    // Because `expr` accounts for parentheses, and aliased is `x=$expr`, we
    // need to allow another layer of parentheses here.
    aliased
        .clone()
        .or(aliased.delimited_by(ctrl('('), ctrl(')')))
}

fn maybe_aliased<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    let aliased = ident_part()
        .then_ignore(ctrl('='))
        // This is added for `maybe_aliased`; possibly we should integrate
        // the funcs
        .or_not()
        .then(expr)
        .map(|(alias, mut expr)| {
            expr.alias = alias.or(expr.alias);
            expr
        });
    // Because `expr` accounts for parentheses, and aliased is `x=$expr`, we
    // need to allow another layer of parentheses here.
    aliased
        .clone()
        .or(aliased.delimited_by(ctrl('('), ctrl(')')))
}

fn func_call<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    let func_name = expr.clone();

    let named_arg = ident_part()
        .map(Some)
        .then_ignore(ctrl(':'))
        .then(expr.clone());

    // TODO: I think this possibly should be restructured. Currently in the case
    // of `derive x = 5`, the `x` is an alias of a single positional argument.
    // That then means we incorrectly allow something like `derive x = 5 y = 6`,
    // since there are two positional arguments each with an alias. This then
    // leads to quite confusing error messages.
    //
    // Instead, we could only allow a single alias per function call as the
    // first positional argument? (I worry that not simple though...).
    // Alternatively we could change the language to enforce tuples, so `derive
    // {x = 5}` were required. But we still need to account for the `join`
    // example below, which doesn't work so well in a tuple; so I'm not sure
    // this helps much.
    //
    // As a reminder, we need to account for `derive x = 5` and `join a=artists
    // (id==album_id)`.
    let positional_arg = maybe_aliased(expr.clone()).map(|e| (None, e));

    func_name
        .then(named_arg.or(positional_arg).repeated())
        .validate(|(name, args), span, emit| {
            if args.is_empty() {
                return name.kind;
            }

            let mut named_args = HashMap::new();
            let mut positional = Vec::new();

            for (name, arg) in args {
                if let Some(name) = name {
                    match named_args.entry(name) {
                        Entry::Occupied(entry) => emit(PError::custom(
                            span,
                            format!("argument '{}' is used multiple times", entry.key()),
                        )),
                        Entry::Vacant(entry) => {
                            entry.insert(arg);
                        }
                    }
                } else {
                    positional.push(arg);
                }
            }

            ExprKind::FuncCall(FuncCall {
                name: Box::new(name),
                args: positional,
                named_args,
            })
        })
        .map_with_span(ExprKind::into_expr)
        .labelled("function call")
}

fn lambda_func<'a, E>(expr: E) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    let param = ident_part()
        .then(type_expr().delimited_by(ctrl('<'), ctrl('>')).or_not())
        .then(ctrl(':').ignore_then(expr.clone().map(Box::new)).or_not());

    let generic_args = ident_part()
        .then(
            ctrl(':')
                .ignore_then(type_expr().separated_by(ctrl('|')).at_least(1))
                .or_not()
                .map(|x| x.unwrap_or_default()),
        )
        .map_with_span(|(name, domain), span| GenericTypeParam {
            name,
            domain,
            span: Some(span),
        })
        .separated_by(ctrl(','))
        .at_least(1)
        .delimited_by(ctrl('<'), ctrl('>'))
        .or_not()
        .map(|x| x.unwrap_or_default());

    choice((
        // func
        keyword("func").ignore_then(generic_args).then(
            param
                .clone()
                .separated_by(new_line().repeated())
                .allow_leading()
                .allow_trailing(),
        ),
        // plain
        param
            .repeated()
            .at_least(1)
            .map(|params| (Vec::new(), params)),
    ))
    .then_ignore(just(TokenKind::ArrowThin))
    // return type
    .then(type_expr().delimited_by(ctrl('<'), ctrl('>')).or_not())
    // body
    .then(func_call(expr))
    .map(|(((generic_type_params, params), return_ty), body)| {
        let (pos, name) = params
            .into_iter()
            .map(|((name, ty), default_value)| FuncParam {
                name,
                ty,
                default_value,
            })
            .partition(|p| p.default_value.is_none());

        Box::new(Func {
            params: pos,
            named_params: name,

            body: Box::new(body),
            return_ty,
            generic_type_params,
        })
    })
    .map(ExprKind::Func)
    .map_with_span(ExprKind::into_expr)
    .labelled("function definition")
}

pub(crate) fn ident() -> impl Parser<TokenKind, Path, Error = PError> + Clone {
    ident_part()
        .separated_by(just(TokenKind::PathSep))
        .at_least(1)
        .map(Path::new::<String, Vec<String>>)
}

fn operator_unary() -> impl Parser<TokenKind, UnOp, Error = PError> + Clone {
    (ctrl('+').to(UnOp::Pos))
        .or(ctrl('-').to(UnOp::Neg))
        .or(ctrl('!').to(UnOp::Not))
}
fn operator_pow() -> impl Parser<TokenKind, BinOp, Error = PError> + Clone {
    just(TokenKind::Pow).to(BinOp::Pow)
}
fn operator_mul() -> impl Parser<TokenKind, BinOp, Error = PError> + Clone {
    (just(TokenKind::DivInt).to(BinOp::DivInt))
        .or(ctrl('*').to(BinOp::Mul))
        .or(ctrl('/').to(BinOp::DivFloat))
        .or(ctrl('%').to(BinOp::Mod))
}
fn operator_add() -> impl Parser<TokenKind, BinOp, Error = PError> + Clone {
    (ctrl('+').to(BinOp::Add)).or(ctrl('-').to(BinOp::Sub))
}
fn operator_compare() -> impl Parser<TokenKind, BinOp, Error = PError> + Clone {
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
fn operator_and() -> impl Parser<TokenKind, BinOp, Error = PError> + Clone {
    just(TokenKind::And).to(BinOp::And)
}
fn operator_or() -> impl Parser<TokenKind, BinOp, Error = PError> + Clone {
    just(TokenKind::Or).to(BinOp::Or)
}
fn operator_coalesce() -> impl Parser<TokenKind, BinOp, Error = PError> + Clone {
    just(TokenKind::Coalesce).to(BinOp::Coalesce)
}
