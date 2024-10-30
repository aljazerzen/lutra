use lutra_parser::parser::pr;

fn main() {
    let ty = get_ty();

    let value = lutra_input_tui::prompt_for_ty(ty).unwrap();

    println!("{value:?}");
}

fn get_ty() -> pr::Ty {
    let source = r#"
        type Pupek = {
            ime = text,
            style = enum { Default, Dead = { reason = text }, Bored, Surprised },
            dimensions = [text],
            subcommand = {
                param1 = text,
                text,
            },
            hello = text,
        }
    "#;

    let tokens = lutra_parser::lexer::lex_source(source).unwrap();

    let (stmts, _errs) = lutra_parser::parser::parse_lr_to_pr(0, tokens.0);
    let stmt = stmts.unwrap().into_iter().next().unwrap();

    let type_def = stmt.kind.into_type_def().unwrap();

    let mut ty = type_def.value.unwrap();

    ty.name = Some(type_def.name);
    ty
}
