use lutra_bin::ir;

fn main() {
    let (mut ty, ty_defs) = get_ty();
    ty.name = Some("Pupek".into());

    let initial = get_value();

    let result = lutra_tui::prompt_for_ty(&ty, &ty_defs, Some(initial)).unwrap();
    let result = result.encode(&ty, &ty_defs).unwrap();

    println!(
        "{}",
        lutra_bin::print_source(&result, &ty, &ty_defs).unwrap()
    );
}

fn get_ty() -> (ir::Ty, Vec<ir::TyDef>) {
    let source = r#"
        {
          ime: text,
          style: enum { Default, Dead: { reason: text }, Bored, Surprised },
          dimensions: [text],
          subcommand: {
            param1: text,
            text,
          },
          hello: bool,
        }
    "#;

    lutra_compiler::_test_compile_ty(source)
}

fn get_value() -> lutra_bin::Value {
    use lutra_bin::Value;

    Value::Tuple(vec![
        Value::new_text("hello"),
        Value::Enum(
            1,
            Box::new(Value::Tuple(vec![Value::new_text("fell of a cliff")])),
        ),
        Value::Array(vec![
            Value::new_text("first"),
            Value::new_text("second"),
            Value::new_text("third"),
        ]),
        Value::Tuple(vec![
            Value::new_text("top-side"),
            Value::new_text("file.txt"),
        ]),
        Value::Prim8(1),
    ])
}
