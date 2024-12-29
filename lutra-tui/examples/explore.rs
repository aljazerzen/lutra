fn main() {
    let project = get_project();

    let result = lutra_tui::prompt_for_decl(&project).unwrap();

    println!("{:?}", result);
}

fn get_project() -> lutra_frontend::Project {
    let source = r#"
        let is_even: func (int): bool

        type MyRel = {a = int, b = text}

        let x = 1 + 2

        module hello {
            let say_hello: func (text): text
        }
    "#;

    let source = lutra_frontend::SourceTree::single("".into(), source.into());
    lutra_frontend::compile(source, Default::default()).unwrap()
}
