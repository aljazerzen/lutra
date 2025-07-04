//! Functional tests for the Lutra language.
//!
//! Executed against Lutra runtime and PostgreSQL (see [crate::postgres]).

#[track_caller]
fn _runtime(lutra_source: &str) -> String {
    crate::init_logger();

    let program_res = lutra_compiler::_test_compile(lutra_source);
    let program = match program_res {
        Ok(p) => p,
        Err(e) => panic!("{e}"),
    };
    tracing::debug!("ir:\n{}", lutra_bin::ir::print(&program));

    let bytecode = lutra_compiler::bytecode_program(program.clone());

    let output =
        lutra_runtime::evaluate(&bytecode, vec![], lutra_runtime::BUILTIN_MODULES).unwrap();

    let output =
        lutra_bin::Value::decode(&output, program.get_output_ty(), &program.types).unwrap();
    output
        .print_source(program.get_output_ty(), &program.types)
        .unwrap()
}

pub struct TestCase {
    pub source: &'static str,
    pub output: &'static str,
}

/// A test function, with a conditional #[ignore] attribute
macro_rules! test_fn {
    (skip_runtime, $name: ident, $test: expr) => {
        #[test]
        #[ignore]
        fn $name() {
            $test
        }
    };
    (skip_postgres, $name: ident, $test: expr) => {
        #[test]
        #[ignore]
        fn $name() {
            $test
        }
    };
    (no_skip, $name: ident, $test: expr) => {
        #[test]
        fn $name() {
            $test
        }
    };
}

macro_rules! test_case {
    ($name: ident, $source: literal, $output: literal) => {
        test_case!($name, $source, $output, no_skip);
    };

    ($name: ident, $source: literal, $output: literal, $ignore_postgres: tt) => {
        test_case!($name, $source, $output, no_skip, $ignore_postgres);
    };

    ($name: ident, $source: literal, $output: literal, $ignore_runtime: tt, $ignore_postgres: tt) => {
        pub mod $name {
            pub const CASE: super::TestCase = super::TestCase {
                source: $source,
                output: $output,
            };

            test_fn!(
                $ignore_runtime,
                runtime,
                assert_eq!(
                    super::_runtime(CASE.source),
                    crate::normalize_expected(CASE.output),
                    "runtime != expected"
                )
            );

            test_fn!(
                $ignore_postgres,
                postgres,
                assert_eq!(
                    crate::postgres::_run(CASE.source, vec![]).1,
                    crate::normalize_expected(CASE.output),
                    "pg != expected"
                )
            );
        }
    };
}

test_case!(empty_array_00, "func (): [int64] -> []", "[]");

test_case!(empty_array_01, "func (): [{a: int64, b: text}] -> []", "[]");

test_case!(
    empty_array_02,
    "
    let ident = func (x: [bool]) -> x
    func () -> ident([])
    ",
    "[]"
);

test_case!(std_mul_01, "(2 * 3): int64", "6");

test_case!(std_mul_02, "(2.1 * 3.5): float64", "7.3500000000000005");

test_case!(std_div_00, "(10 / 6): int64", "1");

test_case!(std_div_01, "(-10 / 6): int64", "-1");

test_case!(std_div_02, "(10 / -6): int64", "-1");

test_case!(std_div_03, "(-10 / -6): int64", "1");

test_case!(std_div_04, "(10.0 / 6.0): float64", "1.6666666666666667");

test_case!(std_div_05, "(-10.0 / 6.0): float64", "-1.6666666666666667");

test_case!(std_div_06, "(10.0 / -6.0): float64", "-1.6666666666666667");

test_case!(std_div_07, "(-10.0 / -6.0): float64", "1.6666666666666667");

test_case!(std_mod_00, "(10 % 6): int64", "4");

test_case!(std_mod_01, "(-10 % 6): int64", "-4");

test_case!(std_mod_02, "(10 % -6): int64", "4");

test_case!(std_mod_03, "(-10 % -6): int64", "-4");

test_case!(std_mod_04, "(10.0 % 6.0): float64", "4");

test_case!(std_mod_05, "(-10.0 % 6.0): float64", "-4");

test_case!(std_mod_06, "(10.0 % -6.0): float64", "4");

test_case!(std_mod_07, "(-10.0 % -6.0): float64", "-4");

test_case!(std_add_int8_00, "(30 + 2): int8", r#"32"#, skip_postgres);

test_case!(std_add_int8_01, "(-2 + 30): int8", r#"28"#, skip_postgres);

test_case!(std_add_int16_00, "(30 + 2): int16", r#"32"#);

test_case!(std_add_int16_01, "(-2 + 30): int16", r#"28"#);

test_case!(std_add_int32_00, "(30 + 2): int32", r#"32"#);

test_case!(std_add_int32_01, "(-2 + 30): int32", r#"28"#);

test_case!(std_add_int64_00, "(30 + 2): int64", r#"32"#);

test_case!(std_add_int64_01, "(-2 + 30): int64", r#"28"#);

test_case!(std_add_uint8_00, "(30 + 2): uint8", r#"32"#, skip_postgres);

test_case!(
    std_add_uint16_00,
    "(30 + 2): uint16",
    r#"32"#,
    skip_postgres
);

test_case!(
    std_add_uint32_00,
    "(30 + 2): uint32",
    r#"32"#,
    skip_postgres
);

test_case!(
    std_add_uint64_00,
    "(30 + 2): uint64",
    r#"32"#,
    skip_postgres
);

test_case!(std_add_float64_00, "(30.2 + 2.30): float64", "32.5");

test_case!(std_add_float64_01, "(2.30 + 30.2): float64", "32.5");

test_case!(
    std_add_float64_02,
    "(1.4 + 0.2): float64",
    "1.5999999999999999"
);

test_case!(std_sub_00, "(30 - 2): int64", r#"28"#);

test_case!(std_sub_01, "(2 - 30): int64", r#"-28"#);

test_case!(std_sub_02, "(30.2 - 2.30): float64", "27.9");

test_case!(std_sub_03, "(2.30 - 30.2): float64", "-27.9");

test_case!(
    std_neg_00,
    "{-2: int64, - (-3: int64)}",
    r#"{
  -2,
  3,
}"#
);

test_case!(
    std_neg_01,
    "{-2.1: float64, - (-3.1: float64)}",
    r#"{
  -2.1,
  3.1,
}"#
);

test_case!(
    std_eq_00,
    "{30: int64 == 2, 30: int64 == 30}",
    r#"{
  false,
  true,
}"#
);

test_case!(
    std_eq_01,
    "{30.3: float64 == 2.2, 30.3: float64 == 30.3}",
    r#"{
  false,
  true,
}"#
);

test_case!(
    std_eq_02,
    "{false == true, false == false}",
    r#"{
  false,
  true,
}"#
);

test_case!(
    std_eq_03,
    r#"{"aa" == "b", "aa" == "aa"}"#,
    r#"{
  false,
  true,
}"#
);

test_case!(
    std_ne_00,
    "{
  30.3: float64 != 2.2,
  30.3: float64 != 30.3
}",
    r#"{
  true,
  false,
}"#
);

test_case!(
    std_ne_01,
    "{
  30.3: float64 != 2.2,
  30.3: float64 != 30.3
}",
    r#"{
  true,
  false,
}"#
);

test_case!(
    std_ne_02,
    "{false != true, false != false}",
    r#"{
  true,
  false,
}"#
);
test_case!(
    std_ne_03,
    r#"{"aa" != "b", "aa" != "aa"}"#,
    r#"{
  true,
  false,
}"#
);

test_case!(
    std_gt_00,
    "{3 > 2: int64, 2 > 3: int64, 2 > 2: int64}",
    r#"{
  true,
  false,
  false,
}"#
);
test_case!(
    std_gt_01,
    "{3.3 > 2.2: float64, 2.2 > 3.3: float64, 2.2 > 2.2: float64}",
    r#"{
  true,
  false,
  false,
}"#
);

test_case!(
    std_lt_00,
    "{3 < 2: int64, 2 < 3: int64, 2 < 2: int64}",
    r#"{
  false,
  true,
  false,
}"#
);
test_case!(
    std_lt_01,
    "{3.3 < 2.2: float64, 2.2 < 3.3: float64, 2.2 < 2.2: float64}",
    r#"{
  false,
  true,
  false,
}"#
);

test_case!(
    std_gte_00,
    "{3 >= 2: int64, 2 >= 3: int64, 2 >= 2: int64}",
    r#"{
  true,
  false,
  true,
}"#
);
test_case!(
    std_gte_01,
    "{3.3 >= 2.2: float64, 2.2 >= 3.3: float64, 2.2 >= 2.2: float64}",
    r#"{
  true,
  false,
  true,
}"#
);

test_case!(
    std_lte_00,
    "{3 <= 2: int64, 2 <= 3: int64, 2 <= 2: int64}",
    r#"{
  false,
  true,
  true,
}"#
);
test_case!(
    std_lte_01,
    "{3.3 <= 2.2: float64, 2.2 <= 3.3: float64, 2.2 <= 2.2: float64}",
    r#"{
  false,
  true,
  true,
}"#
);

test_case!(
    std_and,
    "{false && false, false && true, true && false, true && true}",
    r#"{
  false,
  false,
  false,
  true,
}"#
);

test_case!(
    std_or,
    "{false || false, false || true, true || false, true || true}",
    r#"{
  false,
  true,
  true,
  true,
}"#
);

test_case!(
    std_not,
    "{!false, !true}",
    r#"{
  true,
  false,
}"#
);

test_case!(std_index_00, "std::index([5,3,65,3,2]: [int64], 3)", "3");

test_case!(std_index_01, "([1, 2, 3].2): int64", "3");

test_case!(
    std_index_02,
    "std::index([5.3,3.2,65.4,3.1,2.0], 3): float64",
    "3.1"
);

test_case!(std_index_03, "([1.1, 2.2, 3.3].2): float64", "3.3");

test_case!(
    std_index_04,
    "std::index([false, false, false, true, false], 3)",
    "true"
);

test_case!(std_index_05, "[true, true, false].2", "false");

test_case!(std_index_06, r#"["hello", "world", "!"].2"#, r#""!""#);

test_case!(
    std_map_00,
    "std::map([5,3,65,3,2], func (x: int) -> x + 1)",
    r#"[
  6,
  4,
  66,
  4,
  3,
]"#
);

test_case!(std_map_01, "std::map([], func (x: int) -> x + 1)", "[]");

test_case!(
    std_map_02,
    "std::map([false, true, false], func (x: bool) -> !x)",
    r#"[
  true,
  false,
  true,
]"#
);

test_case!(
    std_map_03,
    r#"std::map(["hello", "world", "!"], func (x: text) -> std::text_ops::length(x))"#,
    r#"[
  5,
  5,
  1,
]"#
);

test_case!(
    std_filter_00,
    r#"std::filter([5,3,65,3,2]: [int64], func (x: int) -> x > 3)"#,
    r#"[
  5,
  65,
]"#
);

test_case!(
    std_filter_01,
    r#"std::filter([5,3,65,3,2]: [int64], func (x: int) -> x < 1)"#,
    "[]"
);

test_case!(
    std_filter_02,
    r#"std::filter([], func (x: int) -> x > 3)"#,
    "[]"
);

test_case!(
    std_filter_03,
    r#"std::filter([false,true,true,false,true], func (x: bool) -> !x)"#,
    r#"[
  false,
  false,
]"#
);

test_case!(
    std_filter_04,
    r#"std::filter([{false, "one"},{true, "two"},{true, "three"},{false, "four"},{true, "five"}], func (x: {bool, text}) -> x.0)"#,
    r#"[
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
]"#
);

test_case!(
    std_slice_00,
    r#"std::slice([5,3,65,3,2]: [int64], 1, 3)"#,
    r#"[
  3,
  65,
]"#
);

test_case!(
    std_slice_01,
    r#"std::slice([5,3,65,3,2]: [int64], 1, -1)"#,
    r#"[
  3,
  65,
  3,
]"#,
    skip_postgres // OFFSET 1 LIMIT (COUNT(*) + (-1)) - 1
);

test_case!(
    std_slice_02,
    r#"std::slice([5,3,65,3,2]: [int64], 4, 2)"#,
    "[]"
);

test_case!(
    std_slice_03,
    r#"std::slice([5,3,65,3,2]: [int64], 6, 7)"#,
    "[]"
);

test_case!(
    std_slice_04,
    r#"std::slice([5,3,65,3,2]: [int64], -7, 0)"#,
    "[]"
);

test_case!(
    std_slice_05,
    r#"std::slice([false,true,false,false,true], 1, 4)"#,
    r#"[
  true,
  false,
  false,
]"#
);

test_case!(
    std_slice_06,
    r#"std::slice([{false,"hello"}, {false,"world"},{true, "!"},{false,"foo"},{true, "bar"}], 1, 4)"#,
    r#"[
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
]"#
);

test_case!(
    std_sort_00,
    r#"func () -> std::sort([5,3,65,3,2]: [int64], func (x: int64) -> -x)"#,
    r#"[
  65,
  5,
  3,
  3,
  2,
]"#
);

test_case!(
    std_sort_01,
    r#"std::sort(
      [{5: int64, "hello"}, {3: int64, "world"}, {65: int64, "!"}, {3: int64, "foo"}, {2: int64, "bar"}],
      func (x: {int64, text}) -> x.0
    )"#,
    r#"[
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
]"#
);

test_case!(
    std_to_columnar_00,
    r#"std::to_columnar([{5,3},{65,1},{3, 2}]: [{int64, int64}])"#,
    r#"{
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
}"#
);

test_case!(
    std_to_columnar_01,
    r#"std::to_columnar([{false,"three"},{true,"one"},{false, "two"}])"#,
    r#"{
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
}"#
);

test_case!(
    std_from_columnar_00,
    r#"std::from_columnar({[4: int16, 3, 2], [5: int32, 4, 1]})"#,
    r#"[
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
]"#
);

test_case!(
    std_from_columnar_01,
    r#"std::from_columnar({[1]: [int32], []: [int64]})"#,
    "[]"
);

test_case!(
    std_from_columnar_02,
    r#"std::from_columnar({[]: [int8], [2]: [int16]})"#,
    "[]"
);

test_case!(
    std_from_columnar_03,
    r#"std::from_columnar({[1: int16, 2], [3: int16, 4, 5]})"#,
    r#"[
  {
    1,
    3,
  },
  {
    2,
    4,
  },
]"#
);

test_case!(
    std_from_columnar_04,
    r#"std::from_columnar({[false, true], ["no", "yes", "neither"]})"#,
    r#"[
  {
    false,
    "no",
  },
  {
    true,
    "yes",
  },
]"#
);

/*

insta::assert_snapshot!(_test_run(r#"
func () -> std::from_columnar({})
"#), @"[]");
 */

test_case!(
    std_map_columnar_00,
    r#"std::map_columnar(
        [{5: int16, 3: int16}, {65, 1}, {3, 2}],
        func (x: {[int16], [int16]}) -> {
            std::lag(x.0, 1),
            std::lead(x.1, 1)
        }
    )
    "#,
    r#"[
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
]"#
);

test_case!(
    std_map_columnar_01,
    r#"std::map_columnar(
        [{false,"hello"},{false,"world"},{true, "!"}],
        func (x: {[bool], [text]}) -> {
            std::lead(x.0, 1),
            std::lag(x.1, 1),
        }
    )
    "#,
    r#"[
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
]"#
);

test_case!(
    std_aggregate_00,
    r#"
    let my_rel: [{int64, int64}] = [{5,3},{65,1},{3, 2}]
    func () -> std::aggregate(my_rel, func (x: {[int64], [int64]}) -> {std::min(x.0), std::min(x.1)})"#,
    r#"{
  3,
  1,
}"#
);

test_case!(
    std_aggregate_01,
    r#"std::aggregate([{false,"hello"},{false,"world"},{true, "!"}], func (x: {[bool], [text]}) -> {x.0 .0, x.1 .2})"#,
    r#"{
  false,
  "!",
}"#
);

test_case!(
    std_group_00,
    r#"(
      std::group([1, 2, 5, 1, 2, 4, 6, 12]: [int64], func (x) -> x % 2)
      | std::sort(func (x) -> x.key)
    )"#,
    r#"[
  {
    key = 0,
    values = [
      2,
      2,
      4,
      6,
      12,
    ],
  },
  {
    key = 1,
    values = [
      1,
      5,
      1,
    ],
  },
]"#
);

test_case!(
    std_group_01,
    r#"func () -> (
      [
        {1, false, 10},
        {2, false, 21},
        {2, false, 22},
        {1, true, 10}
      ]: [{int64, bool, int64}]
      | std::group(func (x: {int64, bool, int64}) -> {x.0, x.2})
      | std::sort(func (x) -> x.key.0 + x.key.1)
    )
"#,
    r#"[
  {
    key = {
      1,
      10,
    },
    values = [
      {
        1,
        false,
        10,
      },
      {
        1,
        true,
        10,
      },
    ],
  },
  {
    key = {
      2,
      21,
    },
    values = [
      {
        2,
        false,
        21,
      },
    ],
  },
  {
    key = {
      2,
      22,
    },
    values = [
      {
        2,
        false,
        22,
      },
    ],
  },
]"#
);

test_case!(
    std_group_02,
    r#"func () -> (
      [
        {1, false, 10},
        {2, false, 21},
        {2, false, 22},
        {1, true, 10}
      ]: [{int64, bool, int64}]
      | std::group(
        func (x: {int64, bool, int64}) -> {x.0, x.2}
      )
      | std::sort(func (x) -> x.key.0 + x.key.1)
      | std::map(func (partition) -> {
        k = partition.key.0,
        v = std::sum(std::map(
          partition.values,
          func (x: {int64, bool, int64}) -> x.2
        )),
      })
    )
"#,
    r#"[
  {
    k = 1,
    v = 20,
  },
  {
    k = 2,
    v = 21,
  },
  {
    k = 2,
    v = 22,
  },
]"#
);

test_case!(std_min_00, r#"std::min([5,3,65,3,2,56,67])"#, "2");

// test_case!(std_min_01, r#"std::min([])"#, "0");

test_case!(std_max_00, r#"std::max([5,3,65,3,2,56,67])"#, "67");

// test_case!(std_max_01, r#"std::max([])"#, "0");

test_case!(std_sum_00, r#"std::sum([5,3,65,3,2,56,67])"#, "201");

test_case!(std_sum_01, r#"std::sum([])"#, "0");

test_case!(std_count_00, r#"std::count([5,3,65,3,2,56,67])"#, "7");

test_case!(std_count_01, r#"std::count([])"#, "0");

test_case!(
    std_average_00,
    r#"std::average([5,3,65,3,2,56,67])"#,
    "28.714285714285715"
);

// test_case!(std_average_01, r#"std::average([])"#, "0");

test_case!(
    std_all_00,
    r#"std::all([true, false, false, true])"#,
    "false"
);

test_case!(std_all_01, r#"std::all([false, false])"#, "false");

test_case!(std_all_02, r#"std::all([true, true, true])"#, "true");

test_case!(std_all_03, r#"std::all([])"#, "true");

test_case!(
    std_any_00,
    r#"std::any([true, false, false, true])"#,
    "true"
);

test_case!(std_any_01, r#"std::any([false, false])"#, "false");

test_case!(std_any_02, r#"std::any([true, true, true])"#, "true");

test_case!(std_any_03, r#"std::any([])"#, "false");

test_case!(
    std_contains_00,
    r#"std::contains([5,3,65,3,2,56,67], 3)"#,
    "true"
);

test_case!(
    std_contains_01,
    r#"std::contains([5,3,65,3,2,56,67], 7)"#,
    "false"
);

test_case!(std_contains_02, r#"std::contains([], 2)"#, "false");

test_case!(
    std_lag_00,
    r#"std::lag([5,3,65,3,2,56,67]: [int8], 2)"#,
    r#"[
  0,
  0,
  5,
  3,
  65,
  3,
  2,
]"#
);

test_case!(
    std_lag_01,
    r#"std::lag([5,3,65,3,2,56,67]: [int16], 12)"#,
    r#"[
  0,
  0,
  0,
  0,
  0,
  0,
  0,
]"#
);

test_case!(std_lag_02, r#"func (): [int64] -> std::lag([], 3)"#, "[]");

test_case!(
    std_lag_03,
    r#"std::lag([5,3,65,4], -2): [int8]"#,
    r#"[
  65,
  4,
  0,
  0,
]"#
);

test_case!(
    std_lead_00,
    r#"std::lead([5,3,65,3,2,56,67], 2): [int8]"#,
    r#"[
  65,
  3,
  2,
  56,
  67,
  0,
  0,
]"#
);

test_case!(
    std_lead_01,
    r#"std::lead([5,3,65,3,2,56,67], 12): [int64]"#,
    r#"[
  0,
  0,
  0,
  0,
  0,
  0,
  0,
]"#
);

test_case!(std_lead_02, r#"func (): [int32] -> std::lead([], 3)"#, "[]");

test_case!(
    std_lead_03,
    r#"std::lead([5,3,65,4], -2): [int64]"#,
    r#"[
  0,
  0,
  5,
  3,
]"#
);

test_case!(
    std_row_number_00,
    r#"func () -> std::row_number([5,3,65,3,2,56,67]: [int64])"#,
    r#"[
  0,
  1,
  2,
  3,
  4,
  5,
  6,
]"#
);

test_case!(
    std_row_number_01,
    r#"func () -> std::row_number([]: [text])"#,
    "[]"
);

test_case!(
    bindings_00,
    r#"let a: int32 = 4
    func () -> [{a, a + 1}, {a + 2, a + 3}]
    "#,
    r#"[
  {
    4,
    5,
  },
  {
    6,
    7,
  },
]"#
);

test_case!(
    bindings_01,
    r#"let a = {3: int32, false}
    func () -> [{a.0, a.1}, {a.0 + 1, !a.1}]
    "#,
    r#"[
  {
    3,
    false,
  },
  {
    4,
    true,
  },
]"#
);

test_case!(
    bindings_02,
    r#"let a: [int16] = [1, 2]
    func () -> [{a.0, a.1}, {a.0 + 1, a.1 + 1}]
    "#,
    r#"[
  {
    1,
    2,
  },
  {
    2,
    3,
  },
]"#
);

test_case!(
    bindings_03,
    r#"let a: [int16] = [1, 2, 3]
    func () -> {true, std::map(a, func (x: int16) -> x * 2), false}
    "#,
    r#"{
  true,
  [
    2,
    4,
    6,
  ],
  false,
}"#
);

test_case!(
    bindings_04,
    r#"let a: [int32] = [1, 2, 3]
    func () -> {true, std::map(a, func (x: int32) -> x * 2).1, false}
    "#,
    r#"{
  true,
  4,
  false,
}"#
);

test_case!(
    functions_00,
    r#"let add_one = func (x: int64) -> x + 1
    func () -> {add_one(4), add_one(add_one(5))}
    "#,
    r#"{
  5,
  7,
}"#
);

test_case!(
    functions_01,
    r#"
    func () -> (4 | func (x: int64) -> x + 1 | func (x: int64) -> x * 2)
    "#,
    r#"10"#
);

test_case!(
    functions_02,
    r#"
    let add_one = func (x: int64) -> x + 1
    let add_two = func (x: int64) -> add_one(add_one(x))
    let my_func = func (x: int64) -> add_two(x) + add_two(x)
    func () -> my_func(1)
    "#,
    r#"6"#
);

test_case!(
    enum_construction_00,
    r#"
    type Status = enum {Done, Pending: int64, Cancelled: text}
    func () -> Status::Done
    "#,
    r#"Done"#
);

test_case!(
    enum_construction_01,
    r#"
    type Status = enum {Done, Pending: int64, Cancelled: text}
    func () -> Status::Pending(513)
    "#,
    r#"Pending(
  513
)"#
);

test_case!(
    enum_construction_02,
    r#"
    type Status = enum {Done, Pending: int64, Cancelled: text}
    func () -> Status::Cancelled("I don't like it")
    "#,
    r#"Cancelled(
  "I don't like it"
)"#
);

test_case!(
    enum_construction_03,
    r#"
    type Color = enum {Red, Green: bool, Blue: bool}
    func () -> Color::Red
    "#,
    r#"Red"#
);

test_case!(
    enum_construction_04,
    r#"
    type Color = enum {Red, Green: int16, Blue: bool}
    func () -> Color::Green(12312)
    "#,
    r#"Green(
  12312
)"#
);

test_case!(
    enum_construction_05,
    r#"
    type Color = enum {Red, Green: bool, Blue: bool}
    func () -> Color::Blue(false)
    "#,
    r#"Blue(
  false
)"#
);

test_case!(
    enum_construction_06,
    r#"
    type Item = {
      id: int64,
      color: enum {Red, Green: bool, Blue: bool},
    }
    func () -> Item::color::Green(true)
    "#,
    r#"Green(
  true
)"#
);

test_case!(
    enum_construction_07,
    r#"
    type X = {
      a: int,
      b: {c: bool, d: text},
    }
    func (): X::b -> {c = true, d = "d"}
    "#,
    r#"{
  c = true,
  d = "d",
}"#
);

test_case!(
    enum_construction_08,
    r#"
    type Animal = enum {
        Cat: text,
        Dog: enum {Generic, Collie: text},
    }

    func () -> [
      Animal::Cat("Whiskers"),
      Animal::Dog(Animal::Dog::Collie("Belie")),
      Animal::Dog(Animal::Dog::Generic),
    ]
    "#,
    r#"[
  Cat(
    "Whiskers"
  ),
  Dog(
    Collie(
      "Belie"
    )
  ),
  Dog(
    Generic
  ),
]"#
);
test_case!(
    enum_construction_09,
    r#"
    type Status = enum {
      Open: int16,
      Closed: bool,
    }
    func (): [Status] -> [Status::Open(0), Status::Open(0)]
    "#,
    r#"[
  Open(
    0
  ),
  Open(
    0
  ),
]"#,
    skip_runtime,
    no_skip
);

test_case!(
    match_00,
    r#"
    type Status = enum {Done, Pending: int64, Cancelled: text}
    func () -> match Status::Pending(4) {
        Status::Done => "done",
        Status::Pending => "pending",
        Status::Cancelled => "cancelled",
    }
    "#,
    r#""pending""#
);

test_case!(
    match_01,
    r#"
    type Status = enum {Done, Pending: int32, Cancelled: text}
    func (): int32 -> match Status::Pending(4) {
        Status::Done => 0,
        Status::Pending(x) => x,
        Status::Cancelled => 0,
    }
    "#,
    r#"4"#
);

test_case!(
    match_02,
    r#"
    type Animal = enum {
        Cat: text,
        Dog: enum {Generic, Collie: text},
    }

    func () -> (
      Animal::Dog(Animal::Dog::Collie("Belie"))
      | func (animal: Animal) -> match animal {
        Animal::Cat(name) => f"Hello {name}",
        Animal::Dog(Animal::Dog::Generic) => "Who's a good boy?",
        Animal::Dog(Animal::Dog::Collie(name)) => f"Come here {name}",
      }
    )
    "#,
    r#""Come here Belie""#
);

test_case!(
    match_03,
    r#"
    type Animal = enum {
        Cat: text,
        Dog: text,
    }

    func () -> (
      [
        Animal::Cat("Whiskers"),
        Animal::Dog("Belie"),
      ]
      | std::map(func (animal: Animal) -> match animal {
        Animal::Cat(name) => f"Hello {name}",
        Animal::Dog(name) => "Who's a good boy?",
      })
    )
    "#,
    r#"
    [
      "Hello Whiskers",
      "Who's a good boy?",
    ]
    "#
);

test_case!(
    match_04,
    r#"
    type Animal = enum {
        Cat: text,
        Dog: enum {Generic, Collie: text},
    }

    func () -> (
      [
          Animal::Cat("Whiskers"),
          Animal::Dog(Animal::Dog::Collie("Belie")),
          Animal::Dog(Animal::Dog::Generic),
      ]
      | std::map(func (animal: Animal) -> match animal {
        Animal::Cat(name) => f"Hello {name}",
        Animal::Dog(Animal::Dog::Generic) => "Who's a good boy?",
        Animal::Dog(Animal::Dog::Collie(name)) => f"Come here {name}",
      })
    )
    "#,
    r#"
    [
      "Hello Whiskers",
      "Come here Belie",
      "Who's a good boy?",
    ]
    "#
);

test_case!(
    complex_00,
    r#"
        let x: [int16] = [
          1, 4, 2, 3, 2, 3, 4, 5, 1, 2
        ]

        func () -> {
          a = 1: int32 + 2,
          {3: int64, 3 + 1: int16},
          {2: int32, 2 + 1: int16},
          hello = (
            x
            | std::map(func (y: int16) -> y + 1)
            | std::filter(func (y: int16) -> !(y > 3))
            # | std::sort(func (x: int16) -> x)
            | std::map(func (y: int16) -> y % 3)
          )
        }
    "#,
    r#"
    {
      a = 3,
      {
        3,
        4,
      },
      {
        2,
        3,
      },
      hello = [
        2,
        0,
        0,
        2,
        0,
      ],
    }
    "#
);

test_case!(
    complex_01,
    r#"
    module chinook {
      type album = {id: int64, title: text}

      let get_albums = func (): [album] -> [
        {id = 3, title = "Hello world!"},
        {id = 5, title = "Foo"},
      ]

      let get_album_by_id = func (album_id: int64): album -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    module box_office {
      type album_sale = {id: int64, total: float}

      let get_album_sales = func (): [album_sale] -> [
        {id = 3, total = 3.6},
        {id = 3, total = 3.1},
        {id = 5, total = 5.2},
      ]

      let get_album_sales_by_id = func (album_id: int64): album_sale -> (
        get_album_sales()
        | std::filter(func (this: album_sale) -> this.id == album_id)
        | std::index(0)
      )
    }

    func () -> {
      chinook::get_albums(),
      chinook::get_album_by_id(3),
      box_office::get_album_sales_by_id(3),
    }
    "#,
    r#"
    {
      [
        {
          id = 3,
          title = "Hello world!",
        },
        {
          id = 5,
          title = "Foo",
        },
      ],
      {
        id = 3,
        title = "Hello world!",
      },
      {
        id = 3,
        total = 3.6,
      },
    }
    "#
);

test_case!(
    complex_02,
    r#"
    module chinook {
      type album = {id: int, title: text}

      let get_album = func (): album -> (
        {id = 3, title = "Hello world!"}
      )
    }

    func () -> {
      5: int16,
      chinook::get_album()
    }
    "#,
    r#"
    {
      5,
      {
        id = 3,
        title = "Hello world!",
      },
    }
    "#
);

test_case!(
    complex_03,
    r#"
    module chinook {
      type album = {id: int64, title: text}

      let get_albums = func (): [album] -> [
        {id = 3, title = "Hello world!"},
        {id = 5, title = "Foo"},
      ]

      let get_album_by_id = func (album_id: int64): album -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    func () -> {
      chinook::get_album_by_id(3),
    }
    "#,
    r#"
    {
      {
        id = 3,
        title = "Hello world!",
      },
    }
    "#
);

test_case!(
    complex_04,
    r#"
    module chinook {
      type album = {title: text, genre_id: int}
      type genre = {id: int, name: text}

      let get_genres = func (): [genre] -> [
        {id = 1, name = "Rock"},
        {id = 2, name = "EDM"},
      ]

      let get_albums = func (): [album] -> [
        {title = "Suck It and See", genre_id = 1},
        {title = "Random Access Memories", genre_id = 2},
        {title = "AM", genre_id = 1},
      ]

      let get_albums_by_genre = func (genre_id: int): [album] -> (
        get_albums()
        | std::filter(func (this: album) -> this.genre_id == genre_id)
      )
    }

    func () -> (
      chinook::get_genres()
      | std::map(func (this: chinook::genre) -> {
        name = this.name,
        albums = (
          chinook::get_albums_by_genre(this.id)
          | std::map(func (this: chinook::album) -> this.title)
        ),
      })
    )
    "#,
    r#"
    [
      {
        name = "Rock",
        albums = [
          "Suck It and See",
          "AM",
        ],
      },
      {
        name = "EDM",
        albums = [
          "Random Access Memories",
        ],
      },
    ]
    "#
);

test_case!(
    constant_00,
    r#"
    {1: int32, [false, true], "hello"}
    "#,
    r#"
    {
      1,
      [
        false,
        true,
      ],
      "hello",
    }
    "#
);

test_case!(
    tuple_00,
    r#"
    func () -> {key = {"code 1", false}, value = 5: int64}.key.1
    "#,
    r#"false"#
);

test_case!(
    default_00,
    r#"
    func (): bool -> std::default()
    "#,
    r#"false"#
);
test_case!(
    default_01,
    r#"
    func (): int32 -> std::default()
    "#,
    r#"0"#
);

test_case!(
    default_02,
    r#"
    func (): float32 -> std::default()
    "#,
    r#"0"#
);

test_case!(
    default_03,
    r#"
    func (): text -> std::default()
    "#,
    r#""""#
);

test_case!(
    default_04,
    r#"
    func (): [int64] -> std::default()
    "#,
    r#"[]"#
);

test_case!(
    default_05,
    r#"
    func (): {int8, text, [bool]} -> std::default()
    "#,
    r#"{
  0,
  "",
  [],
}"#
);

test_case!(
    default_06,
    r#"
    type Status = enum {
      Open: int16,
      Closed: bool,
    }
    func (): Status -> std::default()
    "#,
    r#"Open(
  0
)"#
);
test_case!(
    default_10,
    r#"
    func (): [bool] -> std::lag([std::default(), std::default()], 1)
    "#,
    r#"[
  false,
  false,
]"#
);
test_case!(
    default_11,
    r#"
    func (): [int32] -> std::lag([std::default(), std::default()], 1)
    "#,
    r#"[
  0,
  0,
]"#
);

test_case!(
    default_12,
    r#"
    func (): [float32] -> std::lag([std::default(), std::default()], 1)
    "#,
    r#"[
  0,
  0,
]"#
);

test_case!(
    default_13,
    r#"
    func (): [text] -> std::lag([std::default(), std::default()], 1)
    "#,
    r#"[
  "",
  "",
]"#
);

test_case!(
    default_14,
    r#"
    func (): [[int64]] -> std::lag([std::default(), std::default()], 1)
    "#,
    r#"[
  [],
  [],
]"#,
    skip_postgres
);

test_case!(
    default_15,
    r#"
    func (): [{int8, text, [bool]}] -> std::lag([std::default(), std::default()], 1)
    "#,
    r#"[
  {
    0,
    "",
    [],
  },
  {
    0,
    "",
    [],
  },
]"#,
    skip_postgres
);

test_case!(
    default_16,
    r#"
    type Status = enum {
      Open: int64,
      Closed: bool,
    }
    func (): [Status] -> std::lag([std::default(), std::default()], 1)
    "#,
    r#"[
  Open(
    0
  ),
  Open(
    0
  ),
]"#,
    skip_postgres
);
