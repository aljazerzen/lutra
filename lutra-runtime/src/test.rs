#![cfg(test)]

use insta::assert_snapshot;
use lutra_frontend::pr;

#[track_caller]
fn interpret(program: &str, res_ty: &str) -> String {
    let program = crate::parser::test::parse(program);
    let ty = parse_ty(res_ty);

    let value = crate::interpreter::evaluate(&program, ());

    value.print_source(&ty).unwrap()
}

#[track_caller]
fn parse_ty(ty_source: &str) -> pr::Ty {
    let source = format!("type t = {ty_source}");

    let source = lutra_frontend::SourceTree::single("".into(), source.into());
    let project = lutra_frontend::compile(source, lutra_frontend::CompileParams {}).unwrap();

    let name = pr::Path::from_name("t");
    let type_def = project.root_module.module.get(&name);

    type_def.unwrap().kind.as_ty().unwrap().clone()
}

#[test]
fn interpret_01() {
    assert_snapshot!(interpret(r#"
    let external = [std_int_add];

    let main =
        let 1 = func 2 -> [fn.2+0, fn.2+0, fn.2+0];
        let 2 = var.1;
        {
            (3.5) | var.2,
            (6, 7) | func 3 -> [ fn.3+0, fn.3+1 ],
            (6, 2) | external.0
        }
    "#,
    "{[float], [int], int}"
    ), @r#"
    {
      [
        3.5,
        3.5,
        3.5,
      ],
      [
        6,
        7,
      ],
      8,
    }
    "#
    );
}
