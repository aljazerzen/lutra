use lutra_bin::ir;

fn main() {
    let ty = get_ty();

    let value = lutra_tui::prompt_for_ty(&ty).unwrap();

    println!("{}", value.print_source(&ty).unwrap());

    println!("{}", get_value().print_source(&ty).unwrap());
}

fn get_ty() -> ir::Ty {
    let source = r#"
        {
            ime = text,
            style = enum { Default, Dead = { reason = text }, Bored, Surprised },
            dimensions = [text],
            subcommand = {
                param1 = text,
                text,
            },
            hello = text,
        }
    "#;

    ir::Ty::from(lutra_frontend::_test_compile_ty(source))
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
        Value::Text("world".into()),
    ])
}
