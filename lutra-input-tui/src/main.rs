use lutra_parser::pr;

fn main() {
    let ty = get_ty();

    let value = lutra_input_tui::prompt_for_ty(&ty).unwrap();

    println!("{}", value.print_source(&ty).unwrap());
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

    let (stmts, _errs) = lutra_parser::parse_source(&source, 0);
    let stmt = stmts.unwrap().into_iter().next().unwrap();

    let type_def = stmt.kind.into_type_def().unwrap();

    let mut ty = type_def.value.unwrap();

    ty.name = Some(type_def.name);
    ty
}
