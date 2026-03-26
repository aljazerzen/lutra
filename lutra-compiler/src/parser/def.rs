use chumsky::input::ValueInput;
use chumsky::prelude::*;
use indexmap::IndexMap;

use crate::Span;
use crate::pr::*;

use super::helpers::*;
use super::{PExtra, PError, TokenKind};
use super::{expr, types};

/// The top-level parser
pub fn source<'src, I>() -> impl Parser<'src, I, Source, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let is_submodule = keyword("submodule").or_not().map(|x| x.is_some());

    let ty = types::type_expr();
    let expr = expr::expr(ty.clone());

    let mod_content = recursive(|mod_content| {
        let module_def = keyword("module")
            .ignore_then(def_name())
            .then(
                mod_content
                    .delimited_by(ctrl('{'), ctrl('}'))
                    .map_with(|m, e| set_content_span(m, e.span())),
            )
            .map(|(name, module_def)| (name, DefKind::Module(module_def)))
            .labelled("module definition");

        let annotations = ctrl('@')
            .ignore_then(expr.clone().map(Box::new))
            .map(|expr| Annotation { expr })
            .labelled("annotation")
            .repeated()
            .collect::<Vec<_>>();

        let def_kind = choice((
            module_def.boxed(),
            type_def(ty.clone()).boxed(),
            import_def().boxed(),
            func_def(expr.clone(), ty.clone()).boxed(),
            const_def(expr.clone(), ty).boxed(),
        ))
        .labelled("definition");

        let def = (doc_comment().or_not())
            .then(annotations.clone())
            .then(def_kind)
            .map_with(|((doc_comment, annotations), (name, def_kind)), e| {
                let span = e.span();
                let def = Def {
                    kind: def_kind,
                    span: Some(span),
                    span_name: Some(name.1),
                    annotations,
                    doc_comment,
                };
                (name.0, def)
            });

        let defs = def
            .repeated()
            .collect::<Vec<_>>()
            .validate(|defs, e, emitter| into_module(defs, e.span(), emitter));

        let self_annotation = ctrl('@')
            .then(ctrl('!'))
            .ignore_then(expr.map(Box::new))
            .map(|expr| Annotation { expr })
            .labelled("annotation");

        self_annotation
            .repeated()
            .collect::<Vec<_>>()
            .then(defs)
            .map(|(annotations, mut module_def)| {
                module_def.annotations = annotations;
                module_def
            })
    });

    is_submodule
        .then(mod_content)
        .map_with(|(is_submodule, root), e| Source {
            is_submodule,
            root,
            span: e.span(),
        })
        .then_ignore(end())
}

fn into_module(
    def_vec: Vec<(String, Def)>,
    span: Span,
    emitter: &mut chumsky::input::Emitter<PError<'_>>,
) -> ModuleDef {
    let mut defs = IndexMap::with_capacity(def_vec.len());
    for (i, (name, def)) in def_vec.into_iter().enumerate() {
        // hack that allows imports not to have names
        let name = if name.is_empty() {
            format!("_import{i}")
        } else {
            name
        };

        let span = def.span.unwrap();
        let conflict = defs.insert(name, def);
        if let Some(conflict) = conflict {
            emitter.emit(Rich::custom(span, "duplicate name"));
            emitter.emit(Rich::custom(conflict.span.unwrap(), "duplicate name"));
        }
    }

    ModuleDef {
        defs,
        annotations: vec![],
        span_content: Some(span),
    }
}

fn set_content_span(mut module: ModuleDef, mut span: Span) -> ModuleDef {
    span.start += 1;
    span.len = span.len.saturating_sub(2);
    module.span_content = Some(span);
    module
}

fn doc_comment<'src, I>() -> impl Parser<'src, I, DocComment, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    select! {
        TokenKind::DocComment(text) => text,
    }
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .map_with(|lines: Vec<String>, e| DocComment {
        content: lines.join("\n"),
        span: e.span(),
    })
    .labelled("doc comment")
}

struct NameAndSpan(String, Span);

impl NameAndSpan {
    fn new(n: String, s: Span) -> Self {
        Self(n, s)
    }
}

fn def_name<'src, I>() -> impl Parser<'src, I, NameAndSpan, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    ident_part().map_with(|n, e| NameAndSpan::new(n, e.span()))
}

fn const_def<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, (NameAndSpan, DefKind), PExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    keyword("const")
        .ignore_then(def_name())
        .then(ctrl(':').ignore_then(ty).or_not())
        .then(ctrl('=').ignore_then(expr.clone()).map(Box::new))
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

fn func_def<'src, I>(
    expr: impl Parser<'src, I, Expr, PExtra<'src>> + Clone + 'src,
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, (NameAndSpan, DefKind), PExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let head = keyword("func").ignore_then(def_name());

    let params = delimited_by_parenthesis(
        super::expr::func_param(ty.clone())
            .separated_by(ctrl(','))
            .allow_trailing()
            .collect(),
        |_| vec![],
    );

    let return_ty = ctrl(':').ignore_then(ty.clone()).or_not();

    let ty_params = keyword("where")
        .ignore_then(types::type_params(ty.clone()))
        .or_not()
        .map(|x| x.unwrap_or_default());

    let body = just(TokenKind::ArrowThin)
        .ignore_then(expr.map(Box::new))
        .or_not();

    head.then(params)
        .then(return_ty)
        .then(ty_params)
        .then(body)
        .map_with(|((((name, params), return_ty), ty_params), body), e| {
            let span = e.span();
            let func = Func {
                return_ty,
                body,
                params,
                ty_params,
            };
            let value = Expr::new_with_span(ExprKind::Func(Box::new(func)), span);
            let def = DefKind::Expr(ExprDef {
                ty: None,
                value: Box::new(value),
                constant: false,
            });
            (name, def)
        })
        .labelled("function definition")
}

fn type_def<'src, I>(
    ty: impl Parser<'src, I, Ty, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, (NameAndSpan, DefKind), PExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    keyword("type")
        .ignore_then(def_name())
        .then(
            choice((
                // type alias
                ctrl(':').ignore_then(ty.clone()).map(|ty| TyDef {
                    ty,
                    is_framed: false,
                    framed_label: None,
                }),
                // framed type (nominal / new type)
                delimited_by_parenthesis(
                    ident_part().then_ignore(ctrl(':')).or_not().then(ty),
                    |p| (None, Ty::new_with_span(TyKind::Tuple(vec![]), p)),
                )
                .map(|(framed_label, ty)| TyDef {
                    ty,
                    is_framed: true,
                    framed_label,
                }),
            ))
            .map(DefKind::Ty),
        )
        .labelled("type definition")
}

fn import_def<'src, I>() -> impl Parser<'src, I, (NameAndSpan, DefKind), PExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    let import = recursive(|import_part| {
        expr::ident()
            .then(choice((
                just(TokenKind::PathSep)
                    .ignore_then(delimited_by_parenthesis(
                        import_part
                            .separated_by(ctrl(','))
                            .allow_trailing()
                            .collect(),
                        |_| vec![],
                    ))
                    .map(|children| ImportKind::Many(Path::empty(), children)),
                keyword("as")
                    .ignore_then(ident_part())
                    .or_not()
                    .map(|alias| ImportKind::Single(Path::empty(), alias)),
            )))
            .map_with(|(path, mut kind), e| {
                let span = e.span();
                let (ImportKind::Single(p, ..) | ImportKind::Many(p, ..)) = &mut kind;
                *p = path;
                ImportDef { kind, span }
            })
    });

    keyword("import")
        .ignore_then(import)
        .map_with(|import, e| {
            let s = e.span();
            (NameAndSpan::new("".into(), s), DefKind::Import(import))
        })
        .labelled("import statement")
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use crate::parser::test::parse_with;
    use chumsky::prelude::Parser as _;

    use super::*;

    #[test]
    fn test_doc_comment() {
        assert_debug_snapshot!(parse_with(r#"
        ## doc comment
        ## another line

        "#, |_| Box::new(doc_comment())), @r#"
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
        // In chumsky 0.12, parse() requires consuming all input, so we test
        // or_not() on empty input to verify it returns None without errors.
        assert_debug_snapshot!(parse_with(r#""#, |_| Box::new(doc_comment().or_not())).unwrap(), @"None");
        assert_debug_snapshot!(parse_with(r#"hello"#, |_| Box::new(doc_comment().or_not().then(ident_part()))).unwrap(), @r###"
        (
            None,
            "hello",
        )
        "###);
    }

    #[test]
    fn test_module_annotation_basic() {
        let src = "submodule\n@!my_ann\n\nconst x = 1";
        let parsed = parse_with(src, |_| Box::new(source())).unwrap();
        assert!(parsed.is_submodule);
        assert_eq!(parsed.root.annotations.len(), 1);
        assert!(matches!(
            &parsed.root.annotations[0].expr.kind,
            crate::pr::ExprKind::Ident(p) if p.len() == 1 && p.first() == "my_ann"
        ));
        assert_eq!(parsed.root.defs.len(), 1);
    }

    #[test]
    fn test_module_annotation_multiple() {
        let src = "submodule\n@!ann1\n@!ann2\n\nconst x = 1";
        let parsed = parse_with(src, |_| Box::new(source())).unwrap();
        assert_eq!(parsed.root.annotations.len(), 2);
    }

    #[test]
    fn test_module_annotation_no_submodule() {
        let src = "const x = 1";
        let parsed = parse_with(src, |_| Box::new(source())).unwrap();
        assert!(!parsed.is_submodule);
        assert_eq!(parsed.root.annotations.len(), 0);
    }

    #[test]
    fn test_module_annotation_with_call_expr() {
        let src = "submodule\n@!deprecated(\"use other instead\")\n\nconst x = 1";
        let parsed = parse_with(src, |_| Box::new(source())).unwrap();
        assert_eq!(parsed.root.annotations.len(), 1);
        assert!(matches!(
            &parsed.root.annotations[0].expr.kind,
            crate::pr::ExprKind::Call(_)
        ));
    }
}
