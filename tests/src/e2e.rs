#[track_caller]
fn _test_run(lutra_source: &str) -> String {
    // tracing_subscriber::fmt::Subscriber::builder()
    //     .without_time()
    //     .with_max_level(tracing::Level::DEBUG)
    //     .try_init()
    //     .ok();

    let program = lutra_compiler::_test_compile(lutra_source).unwrap_or_else(|e| panic!("{e}"));
    eprintln!("--- ir:\n{}\n---", lutra_ir::print(&program));
    let output_ty = program.get_output_ty().clone();
    let bytecode = lutra_compiler::bytecode_program(program);

    let output = lutra_runtime::evaluate(&bytecode, vec![], lutra_runtime::BUILTIN_MODULES);

    let output = lutra_bin::Value::decode(&output, &output_ty).unwrap();
    output.print_source(&output_ty).unwrap()
}

#[test]
fn std_mul() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> 2 * 3
    "#), @"6");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 2.1 * 3.5
    "#), @"7.3500000000000005");
}

#[test]
fn std_div() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 / 6
    "#), @"1");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 / 6
    "#), @"-1");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 / -6
    "#), @"-1");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 / -6
    "#), @"1");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 / 6.0
    "#), @"1.6666666666666667");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 / 6.0
    "#), @"-1.6666666666666667");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 / -6.0
    "#), @"-1.6666666666666667");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 / -6.0
    "#), @"1.6666666666666667");
}

#[test]
fn std_mod() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 % 6
    "#), @"4");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 % 6
    "#), @"-4");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 % -6
    "#), @"4");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 % -6
    "#), @"-4");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 % 6.0
    "#), @"4");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 % 6.0
    "#), @"-4");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 % -6.0
    "#), @"4");

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 % -6.0
    "#), @"-4");
}

#[test]
fn std_add() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 + 2, 2 + 30}
    "#), @r#"
    {
      32,
      32,
    }
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 30.2 + 2.30
    "#), @"32.5");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 2.30 + 30.2
    "#), @"32.5");
}

#[test]
fn std_sub() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 - 2, 2 - 30}
    "#), @r#"
    {
      28,
      -28,
    }
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 30.2 - 2.30
    "#), @"27.9");

    insta::assert_snapshot!(_test_run(r#"
    func () -> 2.30 - 30.2
    "#), @"-27.9");
}

#[test]
fn std_neg() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {-2, - (-3)}
    "#), @r#"
    {
      -2,
      3,
    }
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> {-2.1, - (-3.1)}
    "#), @r#"
    {
      -2.1,
      3.1,
    }
    "#);
}

#[test]
fn std_eq() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 == 2, 30 == 30}
    "#), @r#"
    {
      false,
      true,
    }
    "#);
}

#[test]
fn std_ne() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 != 2, 30 != 30}
    "#), @r#"
    {
      true,
      false,
    }
    "#);
}

#[test]
fn std_gt() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 > 2, 2 > 3, 2 > 2}
    "#), @r#"
    {
      true,
      false,
      false,
    }
    "#);
}

#[test]
fn std_lt() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 < 2, 2 < 3, 2 < 2}
    "#), @r#"
    {
      false,
      true,
      false,
    }
    "#);
}

#[test]
fn std_gte() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 >= 2, 2 >= 3, 2 >= 2}
    "#), @r#"
    {
      true,
      false,
      true,
    }
    "#);
}

#[test]
fn std_lte() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 <= 2, 2 <= 3, 2 <= 2}
    "#), @r#"
    {
      false,
      true,
      true,
    }
    "#);
}

#[test]
fn std_and() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {false && false, false && true, true && false, true && true}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {false || false, false || true, true || false, true || true}
    "#), @r#"
    {
      false,
      true,
      true,
      true,
    }
    "#);
}

#[test]
fn std_not() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {!false, !true}
    "#), @r#"
    {
      true,
      false,
    }
    "#);
}

#[test]
fn std_index() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::index([5,3,65,3,2], 3)
    "#), @"3");

    insta::assert_snapshot!(_test_run(r#"
    func () -> [1, 2, 3].2
    "#), @"3");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::index([5.3,3.2,65.4,3.1,2.0], 3)
    "#), @"3.1");

    insta::assert_snapshot!(_test_run(r#"
    func () -> [1.1, 2.2, 3.3].2
    "#), @"3.3");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::index([false, false, false, true, false], 3)
    "#), @"true");

    insta::assert_snapshot!(_test_run(r#"
    func () -> [true, true, false].2
    "#), @"false");

    insta::assert_snapshot!(_test_run(r#"
    func () -> ["hello", "world", "!"].2
    "#), @r#"
    "!"
    "#);
}

#[test]
fn std_map() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::map([5,3,65,3,2], func (x: int) -> x + 1)
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
    func () -> std::map([], func (x: int) -> x + 1)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::map([false, true, false], func (x: bool) -> !x)
    "#), @r#"
    [
      true,
      false,
      true,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::map(["hello", "world", "!"], func (x: text) -> std::text_ops::length(x))
    "#), @r#"
    [
      5,
      5,
      1,
    ]
    "#);
}

#[test]
fn std_filter() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::filter([5,3,65,3,2], func (x: int) -> x > 3)
    "#), @r#"
    [
      5,
      65,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::filter([5,3,65,3,2], func (x: int) -> x < 1)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::filter([], func (x: int) -> x > 3)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::filter([false,true,true,false,true], func (x: bool) -> !x)
    "#), @r#"
    [
      false,
      false,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::filter([{false, "one"},{true, "two"},{true, "three"},{false, "four"},{true, "five"}], func (x: {bool, text}) -> x.0)
    "#), @r#"
    [
      {
        true,
        "two",
      },
      {
        true,
        "three",
      },
      {
        true,
        "five",
      },
    ]
    "#);
}

#[test]
fn std_slice() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([5,3,65,3,2], 1, 3)
    "#), @r#"
    [
      3,
      65,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([5,3,65,3,2], 1, -1)
    "#), @r#"
    [
      3,
      65,
      3,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([5,3,65,3,2], 4, 2)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([5,3,65,3,2], 6, 7)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([5,3,65,3,2], -7, 0)
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([false,true,false,false,true], 1, 4)
    "#), @r#"
    [
      true,
      false,
      false,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([{false,"hello"}, {false,"world"},{true, "!"},{false,"foo"},{true, "bar"}], 1, 4)
    "#), @r#"
    [
      {
        false,
        "world",
      },
      {
        true,
        "!",
      },
      {
        false,
        "foo",
      },
    ]
    "#);
}

#[test]
fn std_sort() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::sort([5,3,65,3,2], func (x: int) -> -x)
    "#), @r#"
    [
      65,
      5,
      3,
      3,
      2,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::sort([{5,"hello"}, {3,"world"},{65, "!"},{3,"foo"},{2, "bar"}], func (x: {int, text}) -> x.0)
    "#), @r#"
    [
      {
        2,
        "bar",
      },
      {
        3,
        "world",
      },
      {
        3,
        "foo",
      },
      {
        5,
        "hello",
      },
      {
        65,
        "!",
      },
    ]
    "#);
}

#[test]
fn std_to_columnar() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::to_columnar([{5,3},{65,1},{3, 2}])
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

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::to_columnar([{false,"three"},{true,"one"},{false, "two"}])
    "#), @r#"
    {
      [
        false,
        true,
        false,
      ],
      [
        "three",
        "one",
        "two",
      ],
    }
    "#);
}

#[test]
fn std_from_columnar() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::from_columnar({[4, 3, 2], [5, 4, 1]})
    "#), @r#"
    [
      {
        4,
        5,
      },
      {
        3,
        4,
      },
      {
        2,
        1,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::from_columnar({[1], []})
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::from_columnar({[], [2]})
    "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::from_columnar({[1, 2], [3, 4, 5]})
    "#), @r#"
    [
      {
        1,
        3,
      },
      {
        2,
        4,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::from_columnar({[false, true], ["false", "true", "neither"]})
    "#), @r#"
    [
      {
        false,
        "false",
      },
      {
        true,
        "true",
      },
    ]
    "#);

    /*

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::from_columnar({})
    "#), @"[]");
     */
}

#[test]
fn std_map_columnar() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::map_columnar(
        [{5,3},{65,1},{3, 2}], 
        func (x: {[int], [int]}) -> {
            std::lag(x.0, 1),
            std::lead(x.1, 1)
        }
    )
    "#), @r#"
    [
      {
        0,
        1,
      },
      {
        5,
        2,
      },
      {
        65,
        0,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::map_columnar(
        [{false,"hello"},{false,"world"},{true, "!"}], 
        func (x: {[bool], [text]}) -> {
            std::lead(x.0, 1),
            std::lag(x.1, 1),
        }
    )
    "#), @r#"
    [
      {
        false,
        "",
      },
      {
        true,
        "hello",
      },
      {
        false,
        "world",
      },
    ]
    "#);
}

#[test]
fn std_aggregate() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::aggregate([{5,3},{65,1},{3, 2}], func (x: {[int], [int]}) -> {std::min(x.0), std::min(x.1)})
    "#), @r#"
    {
      3,
      1,
    }
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::aggregate([{false,"hello"},{false,"world"},{true, "!"}], func (x: {[bool], [text]}) -> {x.0 .0, x.1 .2})
    "#), @r#"
    {
      false,
      "!",
    }
    "#);
}

#[test]
fn std_min() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::min([5,3,65,3,2,56,67])
    "#), @"2");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::min([])
    "#), @"0");
}

#[test]
fn std_max() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::max([5,3,65,3,2,56,67])
    "#), @"67");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::max([])
    "#), @"0");
}

#[test]
fn std_sum() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::sum([5,3,65,3,2,56,67])
    "#), @"201");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::sum([])
    "#), @"0");
}

#[test]
fn std_count() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::count([5,3,65,3,2,56,67])
    "#), @"7");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::count([])
    "#), @"0");
}

#[test]
fn std_average() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::average([5,3,65,3,2,56,67])
    "#), @"28.714285714285715");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::average([])
    "#), @"0");
}

#[test]
fn std_all() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::all([true, false, false, true])
    "#), @"false");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::all([false, false])
    "#), @"false");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::all([true, true, true])
    "#), @"true");
}

#[test]
fn std_any() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::any([true, false, false, true])
    "#), @"true");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::any([false, false])
    "#), @"false");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::any([true, true, true])
    "#), @"true");
}

#[test]
fn std_contains() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::contains([5,3,65,3,2,56,67], 3)
    "#), @"true");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::contains([5,3,65,3,2,56,67], 7)
    "#), @"false");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::contains([], 2)
    "#), @"false");
}

#[test]
fn std_lag() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lag([5,3,65,3,2,56,67], 2)
    "#), @r#"
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

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lag([5,3,65,3,2,56,67], 12)
    "#), @r#"
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

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lag([], 3)"#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lag([5,3,65,4], -2)
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lead([5,3,65,3,2,56,67], 2)
    "#), @r#"
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

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lead([5,3,65,3,2,56,67], 12)
    "#), @r#"
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

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lead([], 3)"#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::lead([5,3,65,4], -2)
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::row_number([5,3,65,3,2,56,67])
    "#), @r#"
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

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::row_number([])"#), @"[]");
}

#[test]
fn bindings() {
    insta::assert_snapshot!(_test_run(r#"
    let a = 4
    func () -> [{a, a + 1}, {a + 2, a + 3}]
    "#), @r#"
    [
      {
        4,
        5,
      },
      {
        6,
        7,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    let a = {3, false}
    func () -> [{a.0, a.1}, {a.0 + 1, !a.1}]
    "#), @r#"
    [
      {
        3,
        false,
      },
      {
        4,
        true,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    let a = [1, 2]
    func () -> [{a.0, a.1}, {a.0 + 1, a.1 + 1}]
    "#), @r#"
    [
      {
        1,
        2,
      },
      {
        2,
        3,
      },
    ]
    "#);
}
