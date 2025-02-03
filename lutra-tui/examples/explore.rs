use itertools::Itertools;
use lutra_bin::ir;
use lutra_frontend::{decl, pr};

fn main() {
    let project = get_project();

    let path = lutra_tui::prompt_for_decl(&project).unwrap();
    let path = pr::Path::new(path.0);

    let decl = project.root_module.module.get(&path).unwrap();
    if let decl::DeclKind::Expr(expr) = &decl.kind {
        let ty = expr.ty.as_ref().unwrap();
        if ty.kind.is_function() {
            execute_function(&project, path);
        } else {
            todo!()
        }
    }
}

fn execute_function(project: &lutra_frontend::Project, path: pr::Path) {
    let program = lutra_frontend::lower(&project.root_module, &path);

    let program_ty = program.main.ty.kind.as_function().unwrap();
    let input_ty = ir::Ty {
        kind: ir::TyKind::Tuple(
            program_ty
                .params
                .iter()
                .map(|p| ir::TyTupleField {
                    name: None,
                    ty: p.clone(),
                })
                .collect(),
        ),
        layout: None,
        name: Some("input".into()),
    };

    let program = lutra_frontend::bytecode_program(program);

    let input_val = lutra_tui::prompt_for_ty(&input_ty, None).unwrap();
    let lutra_bin::Value::Tuple(input_args) = input_val else {
        panic!();
    };
    let inputs = std::iter::zip(input_args, &program.input_tys)
        .map(|(a, t)| a.encode(t).unwrap())
        .collect_vec();

    let result = lutra_runtime::evaluate(&program, inputs, lutra_runtime::BUILTIN_MODULES);
    let result = lutra_bin::Value::decode(&result, &program.output_ty).unwrap();

    let mut output_ty = program.output_ty.clone();
    output_ty.name = Some("output".into());
    lutra_tui::show_value(&output_ty, result).unwrap();
}

fn get_project() -> lutra_frontend::Project {
    let source = r#"
        let is_even = func (a: int): bool -> a % 2 == 0

        let square = func (a: int): int -> a * a

        type MyRel = {a = int, b = text}

        let x = 1 + 2

        module hello {
            let say_hello = func (a: text): text -> f"Hello {a}"
        }
    "#;

    let source = lutra_frontend::SourceTree::single("".into(), source.into());
    lutra_frontend::compile(source, Default::default()).unwrap()
}
