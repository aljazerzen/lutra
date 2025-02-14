use chumsky::prelude::*;
use itertools::Itertools;

use crate::parser::interpolation;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::types::type_expr;
use crate::parser::{ctrl, ident_part, keyword, sequence};
use crate::pr::*;
use crate::span::Span;

use super::pipe;

pub(crate) fn expr_call() -> impl Parser<TokenKind, Expr, Error = PError> + Clone {
    expr()
}

pub(crate) fn expr() -> impl Parser<TokenKind, Expr, Error = PError> + Clone {
    recursive(|expr| {
        let literal = select! { TokenKind::Literal(lit) => ExprKind::Literal(lit) };

        let ident_kind = ident().map(ExprKind::Ident);

        let func = lambda_func(expr.clone());
        let call = func_call(expr.clone());

        let tuple = tuple(expr.clone());
        let array = array(expr.clone());
        let pipeline_expr = pipeline(expr.clone()).delimited_by(ctrl('('), ctrl(')'));
        let interpolation = interpolation();
        let case = case(expr.clone());

        let term = choice((
            literal,
            func,
            tuple,
            array,
            interpolation,
            call,
            ident_kind,
            case,
        ))
        .map_with_span(ExprKind::into_expr)
        // No longer used given the TODO in `pipeline`; can remove if we
        // don't resolve.
        // .or(aliased(expr.clone()))
        .or(pipeline_expr)
        .boxed();

        let term = field_lookup(term);
        let term = unary(term);

        // Binary operators
        let expr = term;
        let expr = binary_op_parser_right(expr, operator_pow());
        let expr = binary_op_parser(expr, operator_mul());
        let expr = binary_op_parser(expr, operator_add());
        let expr = binary_op_parser(expr, operator_compare());
        let expr = binary_op_parser(expr, operator_coalesce());
        let expr = binary_op_parser(expr, operator_and());
        let expr = binary_op_parser(expr, operator_or());
        let expr = range(expr);

        expr.labelled("expression")
    })
}

fn tuple<'a>(
    nested_expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
    sequence(
        ident_part()
            .then_ignore(ctrl('='))
            .or_not()
            .then(nested_expr)
            .map(|(name, expr)| TupleField { name, expr }),
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
    let mapping = (expr.clone().map(Box::new))
        .then_ignore(just(TokenKind::ArrowFat))
        .then(expr.map(Box::new))
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
    let end_only = just(TokenKind::Range)
        .ignore_then(expr.clone())
        .map(|end| Range {
            start: None,
            end: Some(Box::new(end)),
        })
        .map(ExprKind::Range)
        .map_with_span(ExprKind::into_expr);

    end_only.or(expr
        .clone()
        .then(just(TokenKind::Range).ignore_then(expr.or_not()).or_not())
        .map_with_span(|(start, range), span| {
            if let Some(end) = range {
                ExprKind::Range(Range {
                    start: Some(Box::new(start)),
                    end: end.map(Box::new),
                })
                .into_expr(span)
            } else {
                start
            }
        }))
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
    expr.separated_by(pipe())
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

fn func_call<'a, E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    let func_name = ident()
        .map(ExprKind::from)
        .map_with_span(ExprKind::into_expr)
        .map(Box::new);

    func_name
        .then(
            expr.separated_by(ctrl(','))
                .allow_trailing()
                .delimited_by(ctrl('('), ctrl(')'))
                .recover_with(nested_delimiters(
                    TokenKind::Control('('),
                    TokenKind::Control(')'),
                    [
                        (TokenKind::Control('{'), TokenKind::Control('}')),
                        (TokenKind::Control('('), TokenKind::Control(')')),
                        (TokenKind::Control('['), TokenKind::Control(']')),
                    ],
                    |_| vec![],
                )),
        )
        .map(|(name, args)| ExprKind::FuncCall(FuncCall { func: name, args }))
        .labelled("function call")
}

fn lambda_func<'a, E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    let param = ident_part()
        .then(ctrl(':').ignore_then(type_expr()).or_not())
        .map_with_span(|(name, ty), span| FuncParam {
            name,
            ty,
            default_value: None,
            span,
        });

    let generic_params = ident_part()
        .then(
            ctrl(':')
                .ignore_then(type_expr().separated_by(ctrl('|')).at_least(1))
                .or_not()
                .map(|x| x.unwrap_or_default()),
        )
        .map_with_span(|(name, _domain), span| GenericTypeParam {
            name,
            // domain,
            span: Some(span),
        })
        .separated_by(ctrl(','))
        .at_least(1)
        .delimited_by(ctrl('<'), ctrl('>'))
        .or_not()
        .map(|x| x.unwrap_or_default());

    // func
    keyword("func")
        .ignore_then(generic_params)
        .then(
            param
                .clone()
                .separated_by(ctrl(','))
                .allow_trailing()
                .delimited_by(ctrl('('), ctrl(')')),
        )
        // return type
        .then(ctrl(':').ignore_then(type_expr()).or_not())
        // arrow
        .then_ignore(just(TokenKind::ArrowThin))
        // body
        .then(expr.map(Box::new))
        .map(|(((generic_type_params, params), return_ty), body)| {
            Box::new(Func {
                generic_type_params,
                params,
                return_ty,
                body,
            })
        })
        .map(ExprKind::Func)
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
