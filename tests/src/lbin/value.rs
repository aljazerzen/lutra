use insta::assert_snapshot;
use lutra_bin::Value;

#[test]
fn test_print_01() {
    let ty = lutra_compiler::_test_compile_ty(
        "{
        n_rows: int64,
        page: [{
          id: int64,
          name: text,
          address: {
            city: text,
            street: text
          },
          int64,
          is_admin: bool
        }]
      }
    ",
    );

    let value = Value::Tuple(vec![
        Value::Int64(100),
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Int64(2),                  // id = int,
                Value::Text("aljaz".to_string()), // name = text,
                Value::Tuple(vec![
                    // address = {
                    Value::Text("Ljubljana".into()), //     city = text,
                    Value::Text("Trubarjeva ulica".into()), //     street = text
                ]), // },
                Value::Int64(27),                 // int,
                Value::Bool(true),                // is_admin = bool
            ]),
            Value::Tuple(vec![
                Value::Int64(12),               // id = int,
                Value::Text("tom".to_string()), // name = text,
                Value::Tuple(vec![
                    // address = {
                    Value::Text("London".into()),    //     city = text,
                    Value::Text("Trafalgar".into()), //     street = text
                ]), // },
                Value::Int64(18),               // int,
                Value::Bool(false),             // is_admin = bool
            ]),
        ]),
    ]);

    assert_snapshot!(value.print_source(&ty, &[]).unwrap(), @r#"
    {
      n_rows = 100,
      page = [
        {
          id = 2,
          name = "aljaz",
          address = {
            city = "Ljubljana",
            street = "Trubarjeva ulica",
          },
          27,
          is_admin = true,
        },
        {
          id = 12,
          name = "tom",
          address = {
            city = "London",
            street = "Trafalgar",
          },
          18,
          is_admin = false,
        },
      ],
    }
    "#);
}

#[test]
fn test_print_02() {
    let ty = lutra_compiler::_test_compile_ty("[int64]");

    let value = Value::Array(vec![
        Value::Int64(1),
        Value::Int64(2),
        Value::Int64(3),
        Value::Int64(4),
    ]);

    assert_snapshot!(value.print_source(&ty, &[]).unwrap(), @r#"
    [
      1,
      2,
      3,
      4,
    ]
    "#);
}
