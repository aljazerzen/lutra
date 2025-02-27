use itertools::Itertools;
use lutra_bin::ir;
use lutra_compiler::{decl, pr};

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

fn execute_function(project: &lutra_compiler::Project, path: pr::Path) {
    let program = lutra_compiler::lower(&project.root_module, &path);

    let output_ty = program.get_output_ty().clone();
    let input_tys = program.get_input_tys().to_vec();
    let input_ty = ir::Ty {
        kind: ir::TyKind::Tuple(
            input_tys
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

    let bytecode = lutra_compiler::bytecode_program(program);

    let input_val = lutra_tui::prompt_for_ty(&input_ty, None).unwrap();
    let lutra_bin::Value::Tuple(input_args) = input_val else {
        panic!();
    };
    let inputs = std::iter::zip(input_args, &input_tys)
        .map(|(a, t)| a.encode(t).unwrap())
        .collect_vec();

    let result = lutra_runtime::evaluate(&bytecode, inputs, lutra_runtime::BUILTIN_MODULES);
    let result = lutra_bin::Value::decode(&result, &output_ty).unwrap();

    let mut output_ty = output_ty;
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
