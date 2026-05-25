use insta::assert_snapshot;
use lutra_bin::Value;

#[test]
fn test_print_01() {
    let (ty, ty_defs) = lutra_compiler::_test_compile_ty(
        "{
        n_rows: Int64,
        page: [{
          id: Int64,
          name: Text,
          address: {
            city: Text,
            street: Text
          },
          Int64,
          is_admin: Bool
        }]
      }
    ",
    );

    let value = Value::Tuple(vec![
        Value::Prim64(100),
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Prim64(2),         // id = int,
                Value::new_text("aljaz"), // name = text,
                Value::Tuple(vec![
                    // address = {
                    Value::new_text("Ljubljana"), //     city = text,
                    Value::new_text("Trubarjeva ulica"), //     street = text
                ]), // },
                Value::Prim64(27),        // int,
                Value::Prim8(1),          // is_admin = bool
            ]),
            Value::Tuple(vec![
                Value::Prim64(12),      // id = int,
                Value::new_text("tom"), // name = text,
                Value::Tuple(vec![
                    // address = {
                    Value::new_text("London"),    //     city = text,
                    Value::new_text("Trafalgar"), //     street = text
                ]), // },
                Value::Prim64(18),      // int,
                Value::Prim8(0),        // is_admin = bool
            ]),
        ]),
    ]);

    let value = value.encode(&ty, &ty_defs).unwrap();
    let source = lutra_bin::print_source(&value, &ty, &ty_defs).unwrap();

    assert_snapshot!(source, @r#"
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
    let (ty, ty_defs) = lutra_compiler::_test_compile_ty("[Int64]");

    let value = Value::Array(vec![
        Value::Prim64(1),
        Value::Prim64(2),
        Value::Prim64(3),
        Value::Prim64(4),
    ]);

    let value = value.encode(&ty, &ty_defs).unwrap();
    let source = lutra_bin::print_source(&value, &ty, &ty_defs).unwrap();
    assert_snapshot!(source, @r#"
    [
      1,
      2,
      3,
      4,
    ]
    "#);
}
