use lutra_frontend::pr;

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

    let source = lutra_frontend::SourceTree::single("".into(), source.into());
    let project = lutra_frontend::compile(source, lutra_frontend::CompileParams {}).unwrap();

    let name = pr::Path::from_name("Pupek");
    let type_def = project.root_module.module.get(&name);

    type_def.unwrap().kind.as_ty().unwrap().clone()
}
