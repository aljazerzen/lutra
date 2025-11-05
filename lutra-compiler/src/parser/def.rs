use chumsky::prelude::*;
use indexmap::IndexMap;

use super::expr::{expr, ident};
use super::{ctrl, ident_part, keyword};
use crate::Span;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::types;
use crate::pr::*;

/// The top-level parser
pub fn source() -> impl Parser<TokenKind, Source, Error = PError> {
    let is_submodule = keyword("submodule").or_not().map(|x| x.is_some());

    let ty = types::type_expr();
    let expr = expr(ty.clone());

    let definitions = recursive(|definitions| {
        let module_def = keyword("module")
            .ignore_then(ident_part())
            .then(definitions.delimited_by(ctrl('{'), ctrl('}')))
            .map(|(name, module_def)| (name, DefKind::Module(module_def)))
            .labelled("module definition");

        let annotation = ctrl('@')
            .ignore_then(expr.clone())
            .map(|expr| Annotation {
                expr: Box::new(expr),
            })
            .labelled("annotation");

        let def_kind = choice((
            module_def,
            type_def(ty.clone()),
            import_def(),
            func_def(expr.clone(), ty.clone()),
            const_def(expr, ty),
        ));

        // Currently doc comments need to be before the annotation; probably
        // should relax this?
        let def = (doc_comment().or_not())
            .then(annotation.repeated())
            .then(def_kind.map_with_span(|(n, def), span| (n, into_def(def, span))))
            .map(|((doc_comment, annotations), (name, mut def))| {
                def.doc_comment = doc_comment;
                def.annotations = annotations;
                (name, def)
            });

        def.repeated()
            .validate(|defs, _span, emit| into_module(defs, emit))
    });

    is_submodule
        .then(definitions)
        .map_with_span(|(is_submodule, root), span| Source {
            is_submodule,
            root,
            span,
        })
        .then_ignore(end())
}

fn into_module(def_vec: Vec<(String, Def)>, emit: &mut dyn FnMut(PError)) -> ModuleDef {
    let mut defs = IndexMap::with_capacity(def_vec.len());
    for (name, def) in def_vec {
        let span = def.span.unwrap();
        let conflict = defs.insert(name, def);
        if let Some(conflict) = conflict {
            emit(PError::custom(span, "duplicate name"));
            emit(PError::custom(conflict.span.unwrap(), "duplicate name"));
        }
    }
    ModuleDef { defs }
}

fn into_def(kind: DefKind, span: Span) -> Def {
    Def {
        kind,
        span: Some(span),
        annotations: vec![],
        doc_comment: None,
    }
}

fn doc_comment() -> impl Parser<TokenKind, DocComment, Error = PError> + Clone {
    select! {
        TokenKind::DocComment(text) => text,
    }
    .repeated()
    .at_least(1)
    .collect()
    .map_with_span(|lines: Vec<String>, span| DocComment {
        content: lines.join("\n"),
        span,
    })
    .labelled("doc comment")
}

fn const_def(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone,
    ty: impl Parser<TokenKind, Ty, Error = PError> + Clone,
) -> impl Parser<TokenKind, (String, DefKind), Error = PError> + Clone {
    keyword("const")
        .ignore_then(ident_part())
        .then(ctrl(':').ignore_then(ty).or_not())
        .then(ctrl('=').ignore_then(expr.clone()).map(Box::new).map(Some))
        .map(|((name, ty), value)| {
            (
                name,
                DefKind::Expr(ExprDef {
                    value,
                    ty,
                    constant: true,
                }),
            )
        })
        .labelled("constant definition")
}

fn func_def<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
    ty: impl Parser<TokenKind, Ty, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, (String, DefKind), Error = PError> + Clone + 'a {
    let head = keyword("func").ignore_then(ident_part());

    let params = (keyword("const").or_not().map(|x| x.is_some()))
        .then(ident_part())
        .then(ctrl(':').ignore_then(ty.clone()).or_not())
        .map_with_span(|((constant, name), ty), span| FuncParam {
            constant,
            name,
            ty,
            span,
        })
        .separated_by(ctrl(','))
        .allow_trailing()
        .delimited_by(ctrl('('), ctrl(')'));

    let ty_params = keyword("where")
        .ignore_then(types::type_params(ty.clone()).boxed())
        .or_not()
        .map(|x| x.unwrap_or_default());

    head.then(params)
        .then(ctrl(':').ignore_then(ty.clone()).or_not())
        .then(ty_params)
        .then(just(TokenKind::ArrowThin).ignore_then(expr).or_not())
        .map_with_span(|((((name, params), return_ty), ty_params), body), span| {
            let def = DefKind::Expr(if let Some(body) = body {
                let func = Func {
                    return_ty,
                    body: Box::new(body),
                    params,
                    ty_params,
                };
                let value = Expr::new_with_span(ExprKind::Func(Box::new(func)), span);
                ExprDef {
                    ty: None,
                    value: Some(Box::new(value)),
                    constant: false,
                }
            } else {
                let ty_func = TyFunc {
                    params: params.into_iter().map(|p| (p.ty, p.constant)).collect(),
                    body: return_ty.map(Box::new),
                    ty_params,
                };
                let ty = Some(Ty::new_with_span(TyKind::Func(ty_func), span));
                ExprDef {
                    ty,
                    value: None,
                    constant: false,
                }
            });
            (name, def)
        })
        .labelled("function definition")
        .boxed()
}

fn type_def(
    ty: impl Parser<TokenKind, Ty, Error = PError> + Clone,
) -> impl Parser<TokenKind, (String, DefKind), Error = PError> + Clone {
    keyword("type")
        .ignore_then(ident_part())
        .then(ctrl(':').ignore_then(ty))
        .map(|(name, ty)| (name, DefKind::Ty(TyDef { ty })))
        .labelled("type definition")
}

fn import_def() -> impl Parser<TokenKind, (String, DefKind), Error = PError> + Clone {
    keyword("import")
        .ignore_then(ident())
        .then(keyword("as").ignore_then(ident_part()).or_not())
        .map(|(target, alias)| {
            (
                alias.unwrap_or_else(|| target.last().to_string()),
                DefKind::Import(ImportDef { target }),
            )
        })
        .labelled("import statement")
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use super::*;
    use crate::parser::test::parse_with_parser;

    #[test]
    fn test_doc_comment() {
        assert_debug_snapshot!(parse_with_parser(r#"
        ## doc comment
        ## another line

        "#, doc_comment()), @r#"
        Ok(
            DocComment {
                content: "doc comment\nanother line",
                span: 0:9-47,
            },
        )
        "#);
    }

    #[test]
    fn test_doc_comment_or_not() {
        assert_debug_snapshot!(parse_with_parser(r#"hello"#, doc_comment().or_not()).unwrap(), @"None");
        assert_debug_snapshot!(parse_with_parser(r#"hello"#, doc_comment().or_not().then(ident_part())).unwrap(), @r###"
        (
            None,
            "hello",
        )
        "###);
    }
}
