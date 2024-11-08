#![cfg(test)]

use insta::assert_snapshot;

#[test]
fn print_01() {
    assert_snapshot!(super::_test_print(r#"
    let externals = [std_int_add];

    let main =
      let 1 = func 2 -> [fn.2+0, fn.2+0, fn.2+0];
      let 2 = var.1;
      {
        (3.5) | var.2,
        (6, 7) | func 3 -> [ fn.3+0, fn.3+1 ],
        (6, 2) | external.0
      }.1[0]
    "#), @r#"
    let externals = [
      std_int_add,
    ];

    let main =
      let 1 = func 2 -> [
        fn.2+0,
        fn.2+0,
        fn.2+0,
      ];
      let 2 = var.1;
      {
        (3.5) | var.2,
        (6, 7) | func 3 -> [
          fn.3+0,
          fn.3+1,
        ],
        (6, 2) | external.0,
      }.1[0]
    "#);
}
