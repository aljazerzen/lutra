#[track_caller]
fn _test_run(lutra_source: &str) -> String {
    let program = lutra_frontend::_test_compile(lutra_source);
    eprintln!("--- ir:\n{}\n---", lutra_ir::print(&program));

    let result = lutra_runtime::evaluate(&program, (), lutra_runtime::BUILTIN_MODULES);
    let result_ty = &program.main.ty;

    let value = lutra_bin::Value::decode(&result, result_ty).unwrap();
    value.print_source(result_ty).unwrap()
}

#[test]
fn std_mul() {
    insta::assert_snapshot!(_test_run("2 * 3"), @"6");
}
#[test]
fn std_div() {
    insta::assert_snapshot!(_test_run("10 / 6"), @"1");
}
#[test]
fn std_mod() {
    insta::assert_snapshot!(_test_run("10 / 6"), @"1");
}
#[test]
fn std_add() {
    insta::assert_snapshot!(_test_run("{30 + 2, 2 + 30}"), @r#"
    {
      32,
      32,
    }
    "#);
}
#[test]
fn std_sub() {
    insta::assert_snapshot!(_test_run("{30 - 2, 2 - 30}"), @r#"
    {
      28,
      -28,
    }
    "#);
}
#[test]
fn std_eq() {
    insta::assert_snapshot!(_test_run("{30 == 2, 30 == 30}"), @r#"
    {
      false,
      true,
    }
    "#);
}
#[test]
fn std_ne() {
    insta::assert_snapshot!(_test_run("{30 != 2, 30 != 30}"), @r#"
    {
      true,
      false,
    }
    "#);
}
#[test]
fn std_gt() {
    insta::assert_snapshot!(_test_run("{3 > 2, 2 > 3, 2 > 2}"), @r#"
    {
      true,
      false,
      false,
    }
    "#);
}
#[test]
fn std_lt() {
    insta::assert_snapshot!(_test_run("{3 < 2, 2 < 3, 2 < 2}"), @r#"
    {
      false,
      true,
      false,
    }
    "#);
}
#[test]
fn std_gte() {
    insta::assert_snapshot!(_test_run("{3 >= 2, 2 >= 3, 2 >= 2}"), @r#"
    {
      true,
      false,
      true,
    }
    "#);
}
#[test]
fn std_lte() {
    insta::assert_snapshot!(_test_run("{3 <= 2, 2 <= 3, 2 <= 2}"), @r#"
    {
      false,
      true,
      true,
    }
    "#);
}
#[test]
fn std_and() {
    insta::assert_snapshot!(_test_run("{false && false, false && true, true && false, true && true}"), @r#"
    {
      false,
      false,
      false,
      true,
    }
    "#);
}
#[test]
fn std_or() {
    insta::assert_snapshot!(_test_run("{false || false, false || true, true || false, true || true}"), @r#"
    {
      false,
      true,
      true,
      true,
    }
    "#);
}
#[test]
fn std_neg() {
    insta::assert_snapshot!(_test_run("{!false, !true}"), @r#"
    {
      true,
      false,
    }
    "#);
}
#[test]
fn std_not() {
    insta::assert_snapshot!(_test_run("{-2, - (-3)}"), @r#"
    {
      -2,
      3,
    }
    "#);
}
#[test]
fn std_map() {
    insta::assert_snapshot!(_test_run(r#"
    std::map([5,3,65,3,2], func (x: int) -> x + 1)
    "#), @r#"
    [
      6,
      4,
      66,
      4,
      3,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"
    std::map([], func (x: int) -> x + 1)
    "#), @"[]");
}
#[test]
fn std_filter() {
    insta::assert_snapshot!(_test_run(r#"
    std::filter([5,3,65,3,2], func (x: int) -> x > 3)
    "#), @r#"
    [
      5,
      65,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"
    std::filter([5,3,65,3,2], func (x: int) -> x < 1)
    "#), @"[]");
    insta::assert_snapshot!(_test_run(r#"
    std::filter([], func (x: int) -> x > 3)
    "#), @"[]");
}
#[test]
fn std_slice() {
    insta::assert_snapshot!(_test_run(r#"
    std::slice([5,3,65,3,2], 1, 3)
    "#), @r#"
    [
      3,
      65,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    std::slice([5,3,65,3,2], 1, -1)
    "#), @r#"
    [
      3,
      65,
      3,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    std::slice([5,3,65,3,2], 4, 2)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    std::slice([5,3,65,3,2], 6, 7)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    std::slice([5,3,65,3,2], -7, 0)
    "#), @"[]");
}
#[test]
fn std_sort() {
    insta::assert_snapshot!(_test_run(r#"
    std::sort([5,3,65,3,2], func (x: int) -> -x)
    "#), @r#"
    [
      65,
      5,
      3,
      3,
      2,
    ]
    "#);
}
#[test]
fn std_to_columnar() {
    insta::assert_snapshot!(_test_run(r#"
    std::to_columnar([{5,3},{65,1},{3, 2}])
    "#), @r#"
    {
      [
        5,
        65,
        3,
      ],
      [
        3,
        1,
        2,
      ],
    }
    "#);
}
#[test]
#[ignore]
fn std_from_columnar() {
    insta::assert_snapshot!(_test_run(r#"
    std::from_columnar({[4, 3, 2], [5, 4, 1]})
    "#), @"1");

    insta::assert_snapshot!(_test_run(r#"
    std::from_columnar({[], []})
    "#), @"1");
}
#[test]
#[ignore]
fn std_map_columnar() {
    insta::assert_snapshot!(_test_run(r#"
    std::map_columnar(
        [{5,3},{65,1},{3, 2}], 
        func (x: {[int], [int]}) -> {
            std::lag(x.0, 1),
            std::lead(x.1, 1)
        }
    )
    "#), @r#"
    {
      [
        5,
        65,
        3,
      ],
      [
        3,
        1,
        2,
      ],
    }
    "#);
}
#[test]
#[ignore]
fn std_aggregate() {
    insta::assert_snapshot!(_test_run(r#"
    std::aggregate([{5,3},{65,1},{3, 2}], func (x: {[int], [int]}) -> {std::min(x.0), std::min(x.1)})
    "#), @"1");
}
#[test]
fn std_min() {
    insta::assert_snapshot!(_test_run(r#"std::min([5,3,65,3,2,56,67])"#), @"2");
    insta::assert_snapshot!(_test_run(r#"std::min([])"#), @"0");
}
#[test]
fn std_max() {
    insta::assert_snapshot!(_test_run(r#"std::max([5,3,65,3,2,56,67])"#), @"67");
    insta::assert_snapshot!(_test_run(r#"std::max([])"#), @"0");
}
#[test]
fn std_sum() {
    insta::assert_snapshot!(_test_run(r#"std::sum([5,3,65,3,2,56,67])"#), @"201");
    insta::assert_snapshot!(_test_run(r#"std::sum([])"#), @"0");
}
#[test]
fn std_count() {
    insta::assert_snapshot!(_test_run(r#"std::count([5,3,65,3,2,56,67])"#), @"7");
    insta::assert_snapshot!(_test_run(r#"std::count([])"#), @"0");
}
#[test]
fn std_average() {
    insta::assert_snapshot!(_test_run(r#"std::average([5,3,65,3,2,56,67])"#), @"28.714285714285715");
    insta::assert_snapshot!(_test_run(r#"std::average([])"#), @"0");
}
#[test]
fn std_all() {
    insta::assert_snapshot!(_test_run(r#"std::all([true, false, false, true])"#), @"false");
    insta::assert_snapshot!(_test_run(r#"std::all([false, false])"#), @"false");
    insta::assert_snapshot!(_test_run(r#"std::all([true, true, true])"#), @"true");
}
#[test]
fn std_any() {
    insta::assert_snapshot!(_test_run(r#"std::any([true, false, false, true])"#), @"true");
    insta::assert_snapshot!(_test_run(r#"std::any([false, false])"#), @"false");
    insta::assert_snapshot!(_test_run(r#"std::any([true, true, true])"#), @"true");
}
#[test]
fn std_contains() {
    insta::assert_snapshot!(_test_run(r#"std::contains([5,3,65,3,2,56,67], 3)"#), @"true");
    insta::assert_snapshot!(_test_run(r#"std::contains([5,3,65,3,2,56,67], 7)"#), @"false");
    insta::assert_snapshot!(_test_run(r#"std::contains([], 2)"#), @"false");
}
#[test]
fn std_lag() {
    insta::assert_snapshot!(_test_run(r#"std::lag([5,3,65,3,2,56,67], 2)"#), @r#"
    [
      0,
      0,
      5,
      3,
      65,
      3,
      2,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"std::lag([5,3,65,3,2,56,67], 12)"#), @r#"
    [
      0,
      0,
      0,
      0,
      0,
      0,
      0,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"std::lag([], 3)"#), @"[]");
    insta::assert_snapshot!(_test_run(r#"std::lag([5,3,65,4], -2)"#), @r#"
    [
      5,
      3,
      65,
      4,
    ]
    "#);
}
#[test]
fn std_lead() {
    insta::assert_snapshot!(_test_run(r#"std::lead([5,3,65,3,2,56,67], 2)"#), @r#"
    [
      65,
      3,
      2,
      56,
      67,
      0,
      0,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"std::lead([5,3,65,3,2,56,67], 12)"#), @r#"
    [
      0,
      0,
      0,
      0,
      0,
      0,
      0,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"std::lead([], 3)"#), @"[]");
    insta::assert_snapshot!(_test_run(r#"std::lead([5,3,65,4], -2)"#), @r#"
    [
      5,
      3,
      65,
      4,
    ]
    "#);
}
#[test]
fn std_row_number() {
    insta::assert_snapshot!(_test_run(r#"std::row_number([5,3,65,3,2,56,67])"#), @r#"
    [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"std::row_number([])"#), @"[]");
}
