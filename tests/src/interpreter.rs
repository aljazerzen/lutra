#![cfg(test)]

use insta::{assert_debug_snapshot, assert_snapshot};

use lutra_bin::Encode;
use lutra_compiler as lc;
use lutra_interpreter::EvalError;

#[track_caller]
fn _test_interpret(program: &str) -> String {
    let program = lutra_ir::_test_parse(program);
    let program = lc::_test_layout(program);
    let bytecode = lc::bytecode_program(program.clone());

    let output =
        lutra_interpreter::evaluate(&bytecode, vec![], lutra_interpreter::BUILTIN_MODULES, None)
            .unwrap();

    lutra_bin::print_source(&output, program.get_output_ty(), &program.defs).unwrap()
}

#[track_caller]
fn _test_err(program: &str) -> EvalError {
    let program = lutra_ir::_test_parse(program);
    let bytecode = lc::bytecode_program(program.clone());

    lutra_interpreter::evaluate(&bytecode, vec![], lutra_interpreter::BUILTIN_MODULES, None)
        .unwrap_err()
}

#[test]
fn interpreter_layout() {
    // TODO: when we have a bench, see if boxes would yield any speed up
    insta::assert_snapshot!(std::mem::size_of::<lutra_interpreter::Cell>(), @"32");
}

#[test]
fn interpret_01() {
    assert_snapshot!(_test_interpret(r#"
    type std::Int64 = Prim64;
    type std::Float64 = Prim64;
    let main = (func 3 ->
      let 1 = (
        func 2 -> (array
          fn.2+0: std::Float64,
          fn.2+0: std::Float64,
          fn.2+0: std::Float64
        ): [std::Float64]
      ): func (std::Float64) -> [std::Float64];
      let 2 = var.1: func (std::Float64) -> [std::Float64];
      (tuple
        (call
          var.2: func (std::Float64) -> [std::Float64],
          3.5: std::Float64
        ): [std::Float64],
        (call
          (
            func 3 -> (array
              fn.3+0: std::Int64,
              fn.3+1: std::Int64
            ): [std::Int64]
          ): func (std::Int64) -> [std::Int64],
          6: std::Int64,
          7: std::Int64,
        ): [std::Int64],
        (call external.std::ops::add: func (std::Int64) -> std::Int64,
          6: std::Int64,
          2: std::Int64,
        ): std::Int64,
      ): {[std::Float64], [std::Int64], std::Int64}
    ): func () -> {[std::Float64], [std::Int64], std::Int64}
    "#,
    ), @"
    {
      [
        3.5,
        3.5,
        3.5,
      ],
      [
        6,
        7,
      ],
      8,
    }
    "
    );
}

#[test]
fn interpret_02() {
    assert_snapshot!(_test_interpret(r#"
    type Int64 = Prim64;
    let main = (func 0 ->
      let 1 = (
        func 1 -> (tuple
          fn.1+0: Int64,
          fn.1+0: Int64,
        ): {Int64, Int64}
      ): func (Int64) -> {Int64, Int64};
      (call external.std::array::map: func ([Int64], func (Int64) -> {Int64, Int64}) -> [{Int64, Int64}],
        (array 2: Int64, 3: Int64, 1: Int64): [Int64],
        var.1: func (Int64) -> {Int64, Int64},
      ): [{Int64, Int64}]
    ): func () -> [{Int64, Int64}]
    "#,
    ), @r#"
    [
      {
        2,
        2,
      },
      {
        3,
        3,
      },
      {
        1,
        1,
      },
    ]
    "#
    );
}

#[test]
fn interpret_03() {
    assert_snapshot!(_test_interpret(r#"
    type std::Int64 = Prim64;
    let main = (func 0 ->
      let 1 = (array
        (tuple 1:std::Int64, 3:std::Int64): {std::Int64, std::Int64},
        (tuple 5:std::Int64, 4:std::Int64): {std::Int64, std::Int64},
        (tuple 2:std::Int64, 3:std::Int64): {std::Int64, std::Int64},
      ): [{std::Int64, std::Int64}];
      let 2 = (func 1 ->
        (call external.std::ops::mul: func (std::Int64, std::Int64) -> std::Int64,
          (tuple_lookup
            fn.1+0: {std::Int64, std::Int64}
            0
          ): std::Int64,
          (tuple_lookup
            fn.1+0: {std::Int64, std::Int64}
            1
          ): std::Int64,
        ): std::Int64
      ): func ({std::Int64, std::Int64}) -> std::Int64;
      (call external.std::array::map: func ([{std::Int64, std::Int64}], func ({std::Int64, std::Int64}) -> std::Int64) -> [std::Int64],
        var.1: [{std::Int64, std::Int64}],
        var.2: func ({std::Int64, std::Int64}) -> std::Int64,
      ): [std::Int64]
    ): func () -> [std::Int64]
    "#,
    ), @r#"
    [
      3,
      20,
      6,
    ]
    "#
    );
}

#[test]
fn eval_error_00() {
    assert_debug_snapshot!(_test_err(r#"
        let main = (func 0 ->
            var.1: int64
        ): func () -> int64
        "#), @"BadProgram",
    );
}

#[test]
#[ignore]
fn eval_error_01() {
    assert_debug_snapshot!(_test_err(r#"
        let main = (func 0 ->
          (tuple_lookup
            fn.0+0: int64,
            0
          )
        ): func ({int64}) -> int64
        "#), @"BadInputs",
    );
}

#[test]
fn func_call_size() {
    let source = lc::SourceTree::single(
        "".into(),
        r#"
        external func x(): {}
        "#
        .into(),
    );

    let project = lc::check(source, Default::default()).unwrap();
    let params = lc::CompileParams::new("x", lc::ProgramRepr::BytecodeLt).with_external("x");
    let (program, _ty) = lc::compile(&project, &params).unwrap();

    let program_lt = program.encode();

    // ideally, we'd bring that down to 30
    const MAX_SIZE: usize = 47;

    assert!(
        program_lt.len() <= MAX_SIZE,
        "plain func call should be less than MAX_SIZE bytes, it is {} bytes",
        program_lt.len()
    );
}
