use lutra_compiler::ProgramFormat;

fn main() {
    let project = get_project();

    let path = lutra_tui::prompt_for_def(&project).unwrap();
    execute_program(&project, &path.0.join("::"));
}

fn execute_program(project: &lutra_compiler::Project, program: &str) {
    let (program, mut ty) =
        lutra_compiler::compile(project, program, None, ProgramFormat::BytecodeLt).unwrap();

    ty.input.name = Some("input".into());
    ty.output.name = Some("output".into());

    let input_val = lutra_tui::prompt_for_ty(&ty.input, None).unwrap();
    let input = input_val.encode(&ty.input, &ty.defs).unwrap();

    let program = program.into_bytecode_lt().unwrap();
    let result =
        lutra_interpreter::evaluate(&program, input, lutra_interpreter::BUILTIN_MODULES).unwrap();
    let result = lutra_bin::Value::decode(&result, &ty.output, &ty.defs).unwrap();

    lutra_tui::show_value(&ty.output, result).unwrap();
}

fn get_project() -> lutra_compiler::Project {
    let source = r#"
        func is_even(a: int): bool -> a % 2 == 0

        func square(a: int): int -> a * a

        type MyRel = {a = int, b = text}

        let x = 1 + 2

        module hello {
            func say_hello(a: text): text -> f"Hello {a}"
        }
    "#;

    let source = lutra_compiler::SourceTree::single("".into(), source.into());
    lutra_compiler::check(source, Default::default()).unwrap()
}
