use chumsky::prelude::*;

use super::expr::{expr, ident};
use super::{ctrl, ident_part, keyword};
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::types::type_expr;
use crate::pr::*;
use crate::Span;

/// The top-level parser
pub fn source() -> impl Parser<TokenKind, Vec<Stmt>, Error = PError> {
    module_contents().then_ignore(end())
}

fn module_contents() -> impl Parser<TokenKind, Vec<Stmt>, Error = PError> {
    recursive(|module_contents| {
        let module_def = keyword("module")
            .ignore_then(ident_part())
            .then(module_contents.delimited_by(ctrl('{'), ctrl('}')))
            .map(|(name, stmts)| StmtKind::ModuleDef(ModuleDef { name, stmts }))
            .labelled("module definition");

        let annotation = ctrl('@')
            .ignore_then(expr())
            .map(|expr| Annotation {
                expr: Box::new(expr),
            })
            .labelled("annotation");

        let stmt_kind = choice((module_def, type_def(), import_def(), var_def()));

        // Currently doc comments need to be before the annotation; probably
        // should relax this?
        (doc_comment().or_not())
            .then(annotation.repeated())
            .then(stmt_kind.map_with_span(into_stmt))
            .map(|((doc_comment, annotations), mut inner)| {
                inner.doc_comment = doc_comment;
                inner.annotations = annotations;
                inner
            })
            .repeated()
    })
}

fn into_stmt(kind: StmtKind, span: Span) -> Stmt {
    Stmt {
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

/// A variable definition could be any of:
/// - `let foo = 5`
/// - `from artists` — captured as a "main"
fn var_def() -> impl Parser<TokenKind, StmtKind, Error = PError> + Clone {
    let expr = expr();

    let let_ = keyword("let")
        .ignore_then(ident_part())
        .then(ctrl(':').ignore_then(type_expr()).or_not())
        .then(ctrl('=').ignore_then(expr.clone()).map(Box::new).or_not())
        .map(|((name, ty), value)| StmtKind::VarDef(VarDef { name, value, ty }));

    let main = expr.map(Box::new).map(|value| {
        let name = "main".to_string();

        StmtKind::VarDef(VarDef {
            name,
            value: Some(value),
            ty: None,
        })
    });

    let_.or(main).labelled("variable definition")
}

fn type_def() -> impl Parser<TokenKind, StmtKind, Error = PError> + Clone {
    keyword("type")
        .ignore_then(ident_part())
        .then(ctrl('=').ignore_then(type_expr()))
        .map(|(name, ty)| StmtKind::TypeDef(TypeDef { name, ty }))
        .labelled("type definition")
}

fn import_def() -> impl Parser<TokenKind, StmtKind, Error = PError> + Clone {
    keyword("import")
        .ignore_then(ident_part().then_ignore(ctrl('=')).or_not())
        .then(ident())
        .map(|(alias, name)| StmtKind::ImportDef(ImportDef { name, alias }))
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
