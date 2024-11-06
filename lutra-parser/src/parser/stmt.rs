use chumsky::prelude::*;

use super::expr::{expr, expr_call, ident, pipeline};
use super::{ctrl, ident_part, keyword, new_line, pipe};
use crate::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::types::type_expr;
use crate::pr::*;

/// The top-level parser for a PRQL file
pub fn source() -> impl Parser<TokenKind, Vec<Stmt>, Error = PError> {
    module_contents()
        // This is the only instance we can consume newlines at the end of something, since
        // this is the end of the file
        .then_ignore(new_line().repeated())
        .then_ignore(end())
}

fn module_contents() -> impl Parser<TokenKind, Vec<Stmt>, Error = PError> {
    recursive(|module_contents| {
        let module_def = keyword("module")
            .ignore_then(ident_part())
            .then(
                module_contents
                    .then_ignore(new_line().repeated())
                    .delimited_by(ctrl('{'), ctrl('}')),
            )
            .map(|(name, stmts)| StmtKind::ModuleDef(ModuleDef { name, stmts }))
            .labelled("module definition");

        let annotation = new_line()
            .repeated()
            .at_least(1)
            .ignore_then(
                just(TokenKind::Annotate)
                    .ignore_then(expr())
                    .map(|expr| Annotation {
                        expr: Box::new(expr),
                    }),
            )
            .labelled("annotation");

        // TODO: we want to confirm that we're not allowing things on the same
        // line that should't be; e.g. `let foo = 5 let bar = 6`. We can't
        // enforce a new line here because then `module two {let houses =
        // both.alike}` fails (though we could force a new line after the
        // `module` if we wanted to?)
        //
        // let stmt_kind = new_line().repeated().at_least(1).ignore_then(choice((
        let stmt_kind = new_line().repeated().ignore_then(choice((
            module_def,
            type_def(),
            import_def(),
            var_def(),
        )));

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
    // doc comments must start on a new line, so we enforce a new line (which
    // can also be a file start) before the doc comment
    //
    // TODO: we currently lose any empty newlines between doc comments;
    // eventually we want to retain or restrict them
    (new_line().repeated().at_least(1).ignore_then(select! {
        TokenKind::DocComment(dc) => dc,
    }))
    .repeated()
    .at_least(1)
    .collect()
    .map(|lines: Vec<String>| lines.join("\n"))
    .labelled("doc comment")
}

/// A variable definition could be any of:
/// - `let foo = 5`
/// - `from artists` — captured as a "main"
/// - `from artists | into x` — captured as an "into"`
fn var_def() -> impl Parser<TokenKind, StmtKind, Error = PError> + Clone {
    let let_ = new_line()
        .repeated()
        .ignore_then(keyword("let"))
        .ignore_then(ident_part())
        .then(type_expr().delimited_by(ctrl('<'), ctrl('>')).or_not())
        .then(ctrl('=').ignore_then(expr_call()).map(Box::new).or_not())
        .map(|((name, ty), value)| {
            StmtKind::VarDef(VarDef {
                name,
                value,
                ty,
                kind: VarDefKind::Let,
            })
        })
        .labelled("variable definition");

    let main_or_into = new_line()
        .repeated()
        .ignore_then(pipeline(expr_call()))
        .map(Box::new)
        .then(
            pipe()
                .ignore_then(keyword("into").ignore_then(ident_part()))
                .or_not(),
        )
        .map(|(value, name)| {
            let kind = if name.is_none() {
                VarDefKind::Main
            } else {
                VarDefKind::Into
            };
            let name = name.unwrap_or_else(|| "main".to_string());

            StmtKind::VarDef(VarDef {
                name,
                kind,
                value: Some(value),
                ty: None,
            })
        });

    let_.or(main_or_into)
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
    use crate::test::parse_with_parser;

    #[test]
    fn test_doc_comment() {
        assert_debug_snapshot!(parse_with_parser(r#"
        #! doc comment
        #! another line

        "#, doc_comment()), @r###"
        Ok(
            " doc comment\n another line",
        )
        "###);
    }

    #[test]
    fn test_doc_comment_or_not() {
        assert_debug_snapshot!(parse_with_parser(r#"hello"#, doc_comment().or_not()).unwrap(), @"None");
        assert_debug_snapshot!(parse_with_parser(r#"hello"#, doc_comment().or_not().then_ignore(new_line().repeated()).then(ident_part())).unwrap(), @r###"
        (
            None,
            "hello",
        )
        "###);
    }
}
