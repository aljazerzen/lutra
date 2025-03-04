//! Functional tests for the Lutra language.
//!
//! Executed against Lutra runtime and PostgreSQL (see [crate::postgres]).

#[track_caller]
fn _runtime(lutra_source: &str) -> String {
    tracing_subscriber::fmt::Subscriber::builder()
        .without_time()
        .with_max_level(tracing::Level::DEBUG)
        .try_init()
        .ok();

    let program = lutra_compiler::_test_compile(lutra_source).unwrap_or_else(|e| panic!("{e}"));
    tracing::debug!("ir:\n{}", lutra_ir::print(&program));

    let output_ty = program.get_output_ty().clone();
    let bytecode = lutra_compiler::bytecode_program(program);

    let output = lutra_runtime::evaluate(&bytecode, vec![], lutra_runtime::BUILTIN_MODULES);

    let output = lutra_bin::Value::decode(&output, &output_ty).unwrap();
    output.print_source(&output_ty).unwrap()
}

pub struct TestCase {
    pub source: &'static str,
    pub output: &'static str,
}

macro_rules! test_postgres {
    ($case: expr, skip_postgres) => {
        #[test]
        #[ignore]
        fn postgres() {
            assert_eq!(crate::postgres::_run($case.source).1, $case.output);
        }
    };
    ($case: expr, no_skip) => {
        #[test]
        fn postgres() {
            assert_eq!(crate::postgres::_run($case.source).1, $case.output);
        }
    };
}

macro_rules! test_case {
    ($name: ident, $source: literal, $output: literal) => {
        test_case!($name, $source, $output, no_skip);
    };

    ($name: ident, $source: literal, $output: literal, $ignore_postgres: tt) => {
        pub mod $name {
            pub const CASE: super::TestCase = super::TestCase {
                source: $source,
                output: $output,
            };

            #[test]
            fn runtime() {
                assert_eq!(super::_runtime(CASE.source), CASE.output)
            }

            test_postgres!(CASE, $ignore_postgres);
        }
    };
}

test_case!(std_mul_01, "func () -> 2 * 3", "6");

test_case!(std_mul_02, "func () -> 2.1 * 3.5", "7.3500000000000005");

test_case!(std_div_00, "func () -> 10 / 6", "1");

test_case!(std_div_01, "func () -> -10 / 6", "-1");

test_case!(std_div_02, "func () -> 10 / -6", "-1");

test_case!(std_div_03, "func () -> -10 / -6", "1");

test_case!(std_div_04, "func () -> 10.0 / 6.0", "1.6666666666666667");

test_case!(std_div_05, "func () -> -10.0 / 6.0", "-1.6666666666666667");

test_case!(std_div_06, "func () -> 10.0 / -6.0", "-1.6666666666666667");

test_case!(std_div_07, "func () -> -10.0 / -6.0", "1.6666666666666667");

test_case!(std_mod_00, "func () -> 10 % 6", "4");

test_case!(std_mod_01, "func () -> -10 % 6", "-4");

test_case!(std_mod_02, "func () -> 10 % -6", "4");

test_case!(std_mod_03, "func () -> -10 % -6", "-4");

test_case!(std_mod_04, "func () -> 10.0 % 6.0", "4");

test_case!(std_mod_05, "func () -> -10.0 % 6.0", "-4");

test_case!(std_mod_06, "func () -> 10.0 % -6.0", "4");

test_case!(std_mod_07, "func () -> -10.0 % -6.0", "-4");

test_case!(
    std_add_00,
    "func () -> {30 + 2, 2 + 30}",
    r#"{
  32,
  32,
}"#
);

test_case!(std_add_01, "func () -> 30.2 + 2.30", "32.5");

test_case!(std_add_02, "func () -> 2.30 + 30.2", "32.5");

test_case!(
    std_sub_00,
    "func () -> {30 - 2, 2 - 30}",
    r#"{
  28,
  -28,
}"#
);

test_case!(std_sub_01, "func () -> 30.2 - 2.30", "27.9");

test_case!(std_sub_02, "func () -> 2.30 - 30.2", "-27.9");

test_case!(
    std_neg_00,
    "func () -> {-2, - (-3)}",
    r#"{
  -2,
  3,
}"#
);

test_case!(
    std_neg_01,
    "func () -> {-2.1, - (-3.1)}",
    r#"{
  -2.1,
  3.1,
}"#
);

test_case!(
    std_eq,
    "func () -> {30 == 2, 30 == 30}",
    r#"{
  false,
  true,
}"#
);

test_case!(
    std_ne,
    "func () -> {30 != 2, 30 != 30}",
    r#"{
  true,
  false,
}"#
);

test_case!(
    std_gt,
    "func () -> {3 > 2, 2 > 3, 2 > 2}",
    r#"{
  true,
  false,
  false,
}"#
);

test_case!(
    std_lt,
    "func () -> {3 < 2, 2 < 3, 2 < 2}",
    r#"{
  false,
  true,
  false,
}"#
);

test_case!(
    std_gte,
    "func () -> {3 >= 2, 2 >= 3, 2 >= 2}",
    r#"{
  true,
  false,
  true,
}"#
);

test_case!(
    std_lte,
    "func () -> {3 <= 2, 2 <= 3, 2 <= 2}",
    r#"{
  false,
  true,
  true,
}"#
);

test_case!(
    std_and,
    "func () -> {false && false, false && true, true && false, true && true}",
    r#"{
  false,
  false,
  false,
  true,
}"#
);

test_case!(
    std_or,
    "func () -> {false || false, false || true, true || false, true || true}",
    r#"{
  false,
  true,
  true,
  true,
}"#
);

test_case!(
    std_not,
    "func () -> {!false, !true}",
    r#"{
  true,
  false,
}"#
);

test_case!(std_index_00, "func () -> std::index([5,3,65,3,2], 3)", "3");

test_case!(std_index_01, "func () -> [1, 2, 3].2", "3");

test_case!(
    std_index_02,
    "func () -> std::index([5.3,3.2,65.4,3.1,2.0], 3)",
    "3.1"
);

test_case!(std_index_03, "func () -> [1.1, 2.2, 3.3].2", "3.3");

test_case!(
    std_index_04,
    "func () -> std::index([false, false, false, true, false], 3)",
    "true"
);

test_case!(std_index_05, "func () -> [true, true, false].2", "false");

test_case!(
    std_index_06,
    r#"func () -> ["hello", "world", "!"].2"#,
    r#""!""#
);

test_case!(
    std_map_00,
    "func () -> std::map([5,3,65,3,2], func (x: int) -> x + 1)",
    r#"[
  6,
  4,
  66,
  4,
  3,
]"#
);

test_case!(
    std_map_01,
    "func () -> std::map([], func (x: int) -> x + 1)",
    "[]"
);

test_case!(
    std_map_02,
    "func () -> std::map([false, true, false], func (x: bool) -> !x)",
    r#"[
  true,
  false,
  true,
]"#
);

test_case!(
    std_map_03,
    r#"func () -> std::map(["hello", "world", "!"], func (x: text) -> std::text_ops::length(x))"#,
    r#"[
  5,
  5,
  1,
]"#
);

test_case!(
    std_filter_00,
    r#"func () -> std::filter([5,3,65,3,2], func (x: int) -> x > 3)"#,
    r#"[
  5,
  65,
]"#
);

test_case!(
    std_filter_01,
    r#"func () -> std::filter([5,3,65,3,2], func (x: int) -> x < 1)"#,
    "[]"
);

test_case!(
    std_filter_02,
    r#"func () -> std::filter([], func (x: int) -> x > 3)"#,
    "[]"
);

test_case!(
    std_filter_03,
    r#"func () -> std::filter([false,true,true,false,true], func (x: bool) -> !x)"#,
    r#"[
  false,
  false,
]"#
);

test_case!(
    std_filter_04,
    r#"func () -> std::filter([{false, "one"},{true, "two"},{true, "three"},{false, "four"},{true, "five"}], func (x: {bool, text}) -> x.0)"#,
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
    r#"func () -> std::slice([5,3,65,3,2], 1, 3)"#,
    r#"[
  3,
  65,
]"#
);

test_case!(
    std_slice_01,
    r#"func () -> std::slice([5,3,65,3,2], 1, -1)"#,
    r#"[
  3,
  65,
  3,
]"#,
    skip_postgres
);

test_case!(
    std_slice_02,
    r#"func () -> std::slice([5,3,65,3,2], 4, 2)"#,
    "[]",
    skip_postgres
);

test_case!(
    std_slice_03,
    r#"func () -> std::slice([5,3,65,3,2], 6, 7)"#,
    "[]"
);

test_case!(
    std_slice_04,
    r#"func () -> std::slice([5,3,65,3,2], -7, 0)"#,
    "[]",
    skip_postgres
);

test_case!(
    std_slice_05,
    r#"func () -> std::slice([false,true,false,false,true], 1, 4)"#,
    r#"[
  true,
  false,
  false,
]"#
);

test_case!(
    std_slice_06,
    r#"func () -> std::slice([{false,"hello"}, {false,"world"},{true, "!"},{false,"foo"},{true, "bar"}], 1, 4)"#,
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
    r#"func () -> std::sort([5,3,65,3,2], func (x: int) -> -x)"#,
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
    r#"func () -> std::sort([{5,"hello"}, {3,"world"},{65, "!"},{3,"foo"},{2, "bar"}], func (x: {int, text}) -> x.0)"#,
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
    r#"func () -> std::to_columnar([{5,3},{65,1},{3, 2}])"#,
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
}"#,
    skip_postgres
);

test_case!(
    std_to_columnar_01,
    r#"func () -> std::to_columnar([{false,"three"},{true,"one"},{false, "two"}])"#,
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
}"#,
    skip_postgres
);

test_case!(
    std_from_columnar_00,
    r#"func () -> std::from_columnar({[4, 3, 2], [5, 4, 1]})"#,
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
]"#,
    skip_postgres
);

test_case!(
    std_from_columnar_01,
    r#"func () -> std::from_columnar({[1], []})"#,
    "[]",
    skip_postgres
);

test_case!(
    std_from_columnar_02,
    r#"func () -> std::from_columnar({[], [2]})"#,
    "[]",
    skip_postgres
);

test_case!(
    std_from_columnar_03,
    r#"func () -> std::from_columnar({[1, 2], [3, 4, 5]})"#,
    r#"[
  {
    1,
    3,
  },
  {
    2,
    4,
  },
]"#,
    skip_postgres
);

test_case!(
    std_from_columnar_04,
    r#"func () -> std::from_columnar({[false, true], ["false", "true", "neither"]})"#,
    r#"[
  {
    false,
    "false",
  },
  {
    true,
    "true",
  },
]"#,
    skip_postgres
);

/*

insta::assert_snapshot!(_test_run(r#"
func () -> std::from_columnar({})
"#), @"[]");
 */

test_case!(
    std_map_columnar_00,
    r#"func () -> std::map_columnar(
        [{5,3},{65,1},{3, 2}], 
        func (x: {[int], [int]}) -> {
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
]"#,
    skip_postgres
);

test_case!(
    std_map_columnar_01,
    r#"func () -> std::map_columnar(
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
]"#,
    skip_postgres
);

test_case!(
    std_aggregate_00,
    r#"func () -> std::aggregate([{5,3},{65,1},{3, 2}], func (x: {[int], [int]}) -> {std::min(x.0), std::min(x.1)})"#,
    r#"{
  3,
  1,
}"#,
    skip_postgres
);

test_case!(
    std_aggregate_01,
    r#"func () -> std::aggregate([{false,"hello"},{false,"world"},{true, "!"}], func (x: {[bool], [text]}) -> {x.0 .0, x.1 .2})"#,
    r#"{
  false,
  "!",
}"#,
    skip_postgres
);

test_case!(
    std_min_00,
    r#"func () -> std::min([5,3,65,3,2,56,67])"#,
    "2"
);

// test_case!(std_min_01, r#"func () -> std::min([])"#, "0");

test_case!(
    std_max_00,
    r#"func () -> std::max([5,3,65,3,2,56,67])"#,
    "67"
);

// test_case!(std_max_01, r#"func () -> std::max([])"#, "0");

test_case!(
    std_sum_00,
    r#"func () -> std::sum([5,3,65,3,2,56,67])"#,
    "201"
);

test_case!(std_sum_01, r#"func () -> std::sum([])"#, "0");

test_case!(
    std_count_00,
    r#"func () -> std::count([5,3,65,3,2,56,67])"#,
    "7"
);

test_case!(std_count_01, r#"func () -> std::count([])"#, "0");

test_case!(
    std_average_00,
    r#"func () -> std::average([5,3,65,3,2,56,67])"#,
    "28.714285714285715"
);

// test_case!(std_average_01, r#"func () -> std::average([])"#, "0");

test_case!(
    std_all_00,
    r#"func () -> std::all([true, false, false, true])"#,
    "false"
);

test_case!(
    std_all_01,
    r#"func () -> std::all([false, false])"#,
    "false"
);

test_case!(
    std_all_02,
    r#"func () -> std::all([true, true, true])"#,
    "true"
);

// test_case!(std_all_03, r#"func () -> std::all([])"#, "true");

test_case!(
    std_any_00,
    r#"func () -> std::any([true, false, false, true])"#,
    "true"
);

test_case!(
    std_any_01,
    r#"func () -> std::any([false, false])"#,
    "false"
);

test_case!(
    std_any_02,
    r#"func () -> std::any([true, true, true])"#,
    "true"
);

// test_case!(std_any_03, r#"func () -> std::any([])"#, "false");

test_case!(
    std_contains_00,
    r#"func () -> std::contains([5,3,65,3,2,56,67], 3)"#,
    "true"
);

test_case!(
    std_contains_01,
    r#"func () -> std::contains([5,3,65,3,2,56,67], 7)"#,
    "false"
);

test_case!(
    std_contains_02,
    r#"func () -> std::contains([], 2)"#,
    "false"
);

test_case!(
    std_lag_00,
    r#"func () -> std::lag([5,3,65,3,2,56,67], 2)"#,
    r#"[
  0,
  0,
  5,
  3,
  65,
  3,
  2,
]"#,
    skip_postgres
);

test_case!(
    std_lag_01,
    r#"func () -> std::lag([5,3,65,3,2,56,67], 12)"#,
    r#"[
  0,
  0,
  0,
  0,
  0,
  0,
  0,
]"#,
    skip_postgres
);

test_case!(
    std_lag_02,
    r#"func () -> std::lag([], 3)"#,
    "[]",
    skip_postgres
);

test_case!(
    std_lag_03,
    r#"func () -> std::lag([5,3,65,4], -2)"#,
    r#"[
  5,
  3,
  65,
  4,
]"#,
    skip_postgres
);

test_case!(
    std_lead_00,
    r#"func () -> std::lead([5,3,65,3,2,56,67], 2)"#,
    r#"[
  65,
  3,
  2,
  56,
  67,
  0,
  0,
]"#,
    skip_postgres
);

test_case!(
    std_lead_01,
    r#"func () -> std::lead([5,3,65,3,2,56,67], 12)"#,
    r#"[
  0,
  0,
  0,
  0,
  0,
  0,
  0,
]"#,
    skip_postgres
);

test_case!(
    std_lead_02,
    r#"func () -> std::lead([], 3)"#,
    "[]",
    skip_postgres
);

test_case!(
    std_lead_03,
    r#"func () -> std::lead([5,3,65,4], -2)"#,
    r#"[
  5,
  3,
  65,
  4,
]"#,
    skip_postgres
);

test_case!(
    std_row_number_00,
    r#"func () -> std::row_number([5,3,65,3,2,56,67])"#,
    r#"[
  0,
  1,
  2,
  3,
  4,
  5,
  6,
]"#,
    skip_postgres
);

test_case!(
    std_row_number_01,
    r#"func () -> std::row_number([])"#,
    "[]",
    skip_postgres
);

test_case!(
    bindings_00,
    r#"let a = 4
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
    r#"let a = {3, false}
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
    r#"let a = [1, 2]
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
