use chumsky::prelude::*;

use crate::codespan::Span;
use crate::parser::interpolation;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::{ctrl, ident_part, keyword, sequence};
use crate::pr::*;

pub(crate) fn expr<'a>(
    ty: impl Parser<TokenKind, Ty, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a {
    recursive(|expr| {
        let literal = literal().map(ExprKind::Literal);

        let ident_kind = ident().map(ExprKind::Ident);
        let variant = variant(expr.clone()).map(ExprKind::Variant);

        let func = func(expr.clone(), ty.clone());
        let func_short = func_short(expr.clone());
        let call = func_call(expr.clone());

        let tuple = tuple(expr.clone());
        let array = array(expr.clone());
        let nested = nested(expr.clone());
        let interpolation = interpolation();
        let match_ = match_(expr.clone());
        let if_else = if_else(expr.clone());

        let term = choice((
            literal,
            func,
            tuple,
            array,
            interpolation,
            call,
            func_short,
            ident_kind,
            match_,
            if_else,
            nested,
            variant,
        ))
        .map_with_span(Expr::new_with_span)
        .boxed();

        let term = field_lookup(term);
        let term = type_annotation(term, ty);
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
        binary_op_parser(expr, ctrl('|').to(BinOp::Pipe))
    })
}

fn literal() -> impl Parser<TokenKind, Literal, Error = PError> {
    select! { TokenKind::Literal(lit) => lit }
}

fn tuple<'a>(
    nested_expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
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
    .validate(|(finish, string), mut span: Span, emit| {
        span.start += 2;
        match interpolation::parse(string, span) {
            Ok(items) => finish(items),
            Err(errors) => {
                for err in errors {
                    emit(err)
                }
                finish(vec![])
            }
        }
    })
    .labelled("interpolated string")
}

fn match_(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone {
    let pattern = recursive(|pattern| {
        let variant = ctrl('.')
            .ignore_then(ident_part())
            .then(
                pattern
                    .delimited_by(ctrl('('), ctrl(')'))
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
        .map_with_span(Pattern::new_with_span);

        // any of
        term.separated_by(ctrl('|'))
            .at_least(1)
            .map_with_span(|terms, span| {
                if terms.len() > 1 {
                    Pattern::new_with_span(PatternKind::AnyOf(terms), span)
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
                .delimited_by(ctrl('{'), ctrl('}')),
        )
        .map(|(subject, branches)| ExprKind::Match(Match { subject, branches }))
}

fn unary<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + 'a,
) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a {
    operator_unary()
        .map_with_span(|o, s| (o, s))
        .repeated()
        .then(expr)
        .foldr(|(op, mut op_span), expr| {
            op_span.set_end_of(expr.span.as_ref().unwrap());

            let expr = Box::new(expr);
            let kind = ExprKind::Unary(UnaryExpr { op, expr });

            Expr::new_with_span(kind, op_span)
        })
        .boxed()
}

fn field_lookup<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a {
    expr.then(
        ctrl('.')
            .ignore_then(choice((
                ident_part().map(Lookup::Name),
                select! {
                    TokenKind::Literal(Literal::Integer(i)) => Lookup::Position(i)
                },
            )))
            .map_with_span(|f, s| (f, s))
            .repeated(),
    )
    .foldl(|base, (lookup, span)| {
        let base = Box::new(base);
        let kind = ExprKind::Lookup { base, lookup };
        Expr::new_with_span(kind, span)
    })
}

fn range<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a {
    let end_only = just(TokenKind::Range)
        .ignore_then(expr.clone())
        .map(|end| Range {
            start: None,
            end: Some(Box::new(end)),
        })
        .map(ExprKind::Range)
        .map_with_span(Expr::new_with_span);

    end_only.or(expr
        .clone()
        .then(just(TokenKind::Range).ignore_then(expr.or_not()).or_not())
        .map_with_span(|(start, range), span| {
            if let Some(end) = range {
                Expr::new_with_span(
                    ExprKind::Range(Range {
                        start: Some(Box::new(start)),
                        end: end.map(Box::new),
                    }),
                    span,
                )
            } else {
                start
            }
        }))
}

fn nested<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
    let var_bindings = keyword("let")
        .ignore_then(ident_part())
        .then_ignore(ctrl('='))
        .then(expr.clone().map(Box::new))
        .then_ignore(ctrl(';'))
        .map_with_span(|stmt, span| (stmt, span));

    let main = expr.map(Box::new);

    let nested = var_bindings
        .repeated()
        .then(main)
        .foldr(|((name, bound), binding_span), main| {
            let mut span = binding_span;
            span.set_end_of(main.span.as_ref().unwrap());

            let kind = ExprKind::VarBinding(VarBinding { name, bound, main });
            Box::new(Expr::new_with_span(kind, span))
        });

    nested
        .map(ExprKind::Nested)
        .delimited_by(ctrl('('), ctrl(')'))
        .recover_with(nested_delimiters(
            TokenKind::Control('('),
            TokenKind::Control(')'),
            [
                (TokenKind::Control('['), TokenKind::Control(']')),
                (TokenKind::Control('('), TokenKind::Control(')')),
            ],
            |_| ExprKind::Tuple(vec![]),
        ))
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
        .boxed()
}

fn func_call<'a, E>(expr: E) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a
where
    E: Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
{
    let func_name = ident()
        .map(ExprKind::from)
        .map_with_span(Expr::new_with_span)
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
        .map(|(name, args)| {
            ExprKind::Call(Call {
                subject: name,
                args,
            })
        })
        .labelled("function call")
}

fn func<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
    ty: impl Parser<TokenKind, Ty, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
    let param = ident_part()
        .then(ctrl(':').ignore_then(ty.clone()).or_not())
        .map_with_span(|(name, ty), span| FuncParam {
            constant: false,
            name,
            ty,
            span,
        });

    // func
    keyword("func")
        .ignore_then(
            param
                .clone()
                .separated_by(ctrl(','))
                .allow_trailing()
                .delimited_by(ctrl('('), ctrl(')')),
        )
        // return type
        .then(ctrl(':').ignore_then(ty.clone()).or_not())
        // arrow
        .then_ignore(just(TokenKind::ArrowThin))
        // body
        .then(expr.map(Box::new))
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

fn func_short<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + 'a {
    ident_part()
        .then_ignore(just(TokenKind::ArrowThin))
        .map_with_span(|name, span| FuncParam {
            constant: false,
            name,
            ty: None,
            span,
        })
        .then(expr.map(Box::new))
        .map(|(param, body)| FuncShort { param, body })
        .map(Box::new)
        .map(ExprKind::FuncShort)
        .labelled("function")
}

pub(crate) fn ident() -> impl Parser<TokenKind, Path, Error = PError> + Clone {
    ident_part()
        .separated_by(just(TokenKind::PathSep))
        .at_least(1)
        .map(Path::new::<String, Vec<String>>)
}

pub fn variant(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone,
) -> impl Parser<TokenKind, Variant, Error = PError> + Clone {
    ctrl('.')
        .ignore_then(ident_part())
        .then(
            expr.delimited_by(ctrl('('), ctrl(')'))
                .map(Box::new)
                .or_not(),
        )
        .map(|(name, inner)| Variant { name, inner })
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
fn type_annotation<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + 'a,
    ty: impl Parser<TokenKind, Ty, Error = PError> + 'a,
) -> impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a {
    expr.then(ctrl(':').ignore_then(ty).or_not())
        .map_with_span(|(expr, ty), span| {
            if let Some(ty) = ty {
                let expr = Box::new(expr);
                let ty = Box::new(ty);
                let kind = ExprKind::TypeAnnotation(TypeAnnotation { expr, ty });
                Expr::new_with_span(kind, span)
            } else {
                expr
            }
        })
        .boxed()
}
fn if_else<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, ExprKind, Error = PError> + Clone + 'a {
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
