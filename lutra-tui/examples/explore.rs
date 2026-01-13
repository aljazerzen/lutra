use lutra_compiler::ProgramFormat;

fn main() {
    let project = get_project();
    let defs = lutra_compiler::project_to_types(&project);

    let path = lutra_tui::prompt_for_def(&defs).unwrap();
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
        lutra_interpreter::evaluate(&program, input, lutra_interpreter::BUILTIN_MODULES, None)
            .unwrap();

    lutra_tui::show_value(&result, &ty.output, &ty.defs).unwrap();
}

fn get_project() -> lutra_compiler::Project {
    let source = r#"
        func is_even(a: int64): bool -> a % 2 == 0

        func square(a: int64): int64 -> a * a

        type MyRel: {a: int64, b: text}

        func get_my_rel(): [MyRel] -> [{12, "blah"}, {10, "foo"}]

        const x: int64 = 10

        module hello {
            func say_hello(a: text): text -> f"Hello {a}"
        }
    "#;

    let source = lutra_compiler::SourceTree::single("".into(), source.into());
    lutra_compiler::check(source, Default::default()).unwrap()
}
