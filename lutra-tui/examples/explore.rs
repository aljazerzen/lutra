use lutra_compiler::pr;

fn main() {
    let project = get_project();

    let path = lutra_tui::prompt_for_decl(&project).unwrap();
    let path = pr::Path::new(path.0);

    let decl = project.root_module.module.get(&path).unwrap();
    if let Ok(expr) = decl.into_expr() {
        let ty = expr.ty.as_ref().unwrap();
        if ty.kind.is_func() {
            execute_function(&project, path);
        } else {
            todo!()
        }
    }
}

fn execute_function(project: &lutra_compiler::Project, path: pr::Path) {
    let program = lutra_compiler::lower_var(&project.root_module, &path);

    let mut input_ty = program.get_input_ty().clone();
    input_ty.name = Some("input".into());

    let bytecode = lutra_compiler::bytecode_program(program.clone());

    let input_val = lutra_tui::prompt_for_ty(&input_ty, None).unwrap();
    let input = input_val.encode(&input_ty, &program.types).unwrap();

    let result =
        lutra_interpreter::evaluate(&bytecode, input, lutra_interpreter::BUILTIN_MODULES).unwrap();
    let result =
        lutra_bin::Value::decode(&result, program.get_output_ty(), &program.types).unwrap();

    let mut output_ty = program.get_output_ty().clone();
    output_ty.name = Some("output".into());
    lutra_tui::show_value(&output_ty, result).unwrap();
}

fn get_project() -> lutra_compiler::Project {
    let source = r#"
        let is_even = func (a: int): bool -> a % 2 == 0

        let square = func (a: int): int -> a * a

        type MyRel = {a = int, b = text}

        let x = 1 + 2

        module hello {
            let say_hello = func (a: text): text -> f"Hello {a}"
        }
    "#;

    let source = lutra_compiler::SourceTree::single("".into(), source.into());
    lutra_compiler::compile(source, Default::default()).unwrap()
}
