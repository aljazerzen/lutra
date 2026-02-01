use lutra_bin::ir;

fn main() {
    let mut ty = get_ty();
    ty.name = Some("Pupek".into());

    let initial = get_value();

    let result = lutra_tui::prompt_for_ty(&ty, &[], Some(initial)).unwrap();
    let result = result.encode(&ty, &[]).unwrap();

    println!("{}", lutra_bin::print_source(&result, &ty, &[]).unwrap());
}

fn get_ty() -> ir::Ty {
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
        Value::Text("hello".into()),
        Value::Enum(
            1,
            Box::new(Value::Tuple(vec![Value::Text("fell of a cliff".into())])),
        ),
        Value::Array(vec![
            Value::Text("first".into()),
            Value::Text("second".into()),
            Value::Text("third".into()),
        ]),
        Value::Tuple(vec![
            Value::Text("top-side".into()),
            Value::Text("file.txt".into()),
        ]),
        Value::Prim8(1),
    ])
}
