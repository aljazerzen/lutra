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
pub fn source() -> impl Parser<TokenKind, ModuleDef, Error = PError> {
    definitions()
        .try_map(|defs, _span| into_module(defs))
        .then_ignore(end())
}

fn definitions() -> impl Parser<TokenKind, Vec<(String, Def)>, Error = PError> {
    let ty = types::type_expr();
    let expr = expr(ty.clone());

    recursive(|definitions| {
        let module_def = keyword("module")
            .ignore_then(ident_part())
            .then(
                definitions
                    .delimited_by(ctrl('{'), ctrl('}'))
                    .try_map(|defs, _span| into_module(defs)),
            )
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
        (doc_comment().or_not())
            .then(annotation.repeated())
            .then(def_kind.map_with_span(|(n, def), span| (n, into_def(def, span))))
            .map(|((doc_comment, annotations), (name, mut def))| {
                def.doc_comment = doc_comment;
                def.annotations = annotations;
                (name, def)
            })
            .repeated()
    })
}

#[allow(clippy::result_large_err)]
fn into_module(defs: Vec<(String, Def)>) -> Result<ModuleDef, PError> {
    let mut result = IndexMap::with_capacity(defs.len());
    for (name, def) in defs {
        let span = def.span.unwrap();
        let conflict = result.insert(name, def);
        if conflict.is_some() {
            return Err(PError::custom(span, "duplicate name"));
        }
    }
    Ok(ModuleDef { defs: result })
}

fn into_def(kind: DefKind, span: Span) -> Def {
    Def {
        kind,
        span: Some(span),
        annotations: vec![],
        doc_comment: None,
    }
}

fn doc_comment() -> impl Parser<TokenKind, String, Error = PError> + Clone {
    select! {
        TokenKind::DocComment(text) => text,
    }
    .repeated()
    .at_least(1)
    .collect()
    .map(|lines: Vec<String>| lines.join("\n"))
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
        .map(|((name, ty), value)| (name, DefKind::Expr(ExprDef { value, ty })))
        .labelled("constant definition")
}

fn func_def<'a>(
    expr: impl Parser<TokenKind, Expr, Error = PError> + Clone + 'a,
    ty: impl Parser<TokenKind, Ty, Error = PError> + Clone + 'a,
) -> impl Parser<TokenKind, (String, DefKind), Error = PError> + Clone + 'a {
    let head = keyword("func").ignore_then(ident_part());

    let params = ident_part()
        .then(ctrl(':').ignore_then(ty.clone()).or_not())
        .map_with_span(|(name, ty), span| FuncParam { name, ty, span })
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
                    ty: value.ty.clone(),
                    value: Some(Box::new(value)),
                }
            } else {
                let ty_func = TyFunc {
                    params: params.into_iter().map(|p| p.ty).collect(),
                    body: return_ty.map(Box::new),
                    ty_params,
                };
                let ty = Some(Ty::new_with_span(TyKind::Func(ty_func), span));
                ExprDef { ty, value: None }
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
        .ignore_then(ident_part().then_ignore(ctrl('=')).or_not())
        .then(ident())
        .map(|(alias, target)| {
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

        "#, doc_comment()), @r###"
        Ok(
            " doc comment\n another line",
        )
        "###);
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
