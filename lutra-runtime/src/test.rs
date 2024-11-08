#![cfg(test)]

use insta::assert_snapshot;
use lutra_frontend::pr;

#[track_caller]
fn test_interpret(program: &str, res_ty: &str) -> String {
    let program = lutra_ir::_test_parse(program);
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
    assert_snapshot!(test_interpret(r#"
    let externals = [core_int_add];

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

#[test]
fn interpret_02() {
    assert_snapshot!(test_interpret(r#"
    let externals = [core_array_map];

    let main =
        let 1 = func 1 -> {fn.1+0, fn.1+0};
        (var.1, [2, 3, 1]) | external.0
    "#,
    "[{int, int}]"
    ), @r#"
    [
      {
        2,
        2,
      },
      {
        3,
        3,
      },
      {
        1,
        1,
      },
    ]
    "#
    );
}

#[test]
fn interpret_03() {
    assert_snapshot!(test_interpret(r#"
    let externals = [core_array_map, core_int_mul];

    let main =
        let 1 = [{1, 3}, {5, 4}, {2, 3}];
        let 2 = func 1 -> (fn.1+0 .0, fn.1+0 .1) | external.1;
        (var.2, var.1) | external.0
    "#,
    "[int]"
    ), @r#"
    [
      3,
      20,
      6,
    ]
    "#
    );
}
