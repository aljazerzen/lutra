use insta::assert_snapshot;
use lutra_bin::ir;
use lutra_bin::Value;

#[test]
fn test_print_01() {
    let ty = lutra_frontend::_test_compile_ty(
        "{
        n_rows = int,
        page = [{
          id = int,
          name = text,
          address = {
            city = text,
            street = text
          },
          int,
          is_admin = bool
        }]
      }
    ",
    );

    let value = Value::Tuple(vec![
        Value::Int(100),
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Int(2),                    // id = int,
                Value::Text("aljaz".to_string()), // name = text,
                Value::Tuple(vec![
                    // address = {
                    Value::Text("Ljubljana".into()), //     city = text,
                    Value::Text("Trubarjeva ulica".into()), //     street = text
                ]), // },
                Value::Int(27),                   // int,
                Value::Bool(true),                // is_admin = bool
            ]),
            Value::Tuple(vec![
                Value::Int(12),                 // id = int,
                Value::Text("tom".to_string()), // name = text,
                Value::Tuple(vec![
                    // address = {
                    Value::Text("London".into()),    //     city = text,
                    Value::Text("Trafalgar".into()), //     street = text
                ]), // },
                Value::Int(18),                 // int,
                Value::Bool(false),             // is_admin = bool
            ]),
        ]),
    ]);

    let ty = ir::Ty::from(ty);
    assert_snapshot!(value.print_source(&ty).unwrap(), @r#"
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
    let ty = lutra_frontend::_test_compile_ty("[int]");

    let value = Value::Array(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::Int(4),
    ]);

    let ty = ir::Ty::from(ty);
    assert_snapshot!(value.print_source(&ty).unwrap(), @r#"
    [
      1,
      2,
      3,
      4,
    ]
    "#);
}
