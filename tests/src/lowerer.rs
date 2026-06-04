use std::sync::Arc;

use insta::assert_snapshot;

use lutra_compiler as lc;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    crate::init_logger();

    let program = match lc::_test_compile_main(source) {
        Ok(t) => t,
        Err(e) => panic!("{e}"),
    };
    lutra_bin::ir::print_no_color(&program)
}

#[track_caller]
fn _test_compile_dep_and_print(dep: &str, this: &str) -> String {
    crate::init_logger();

    // check dep
    let dep_source = lc::SourceTree::single("".into(), dep.to_string());
    let dep_project = Arc::new(lc::check(dep_source, Default::default()).unwrap());

    // check this
    let source = lc::SourceTree::single("".into(), this.to_string());
    let project = lc::check(source, lc::CheckParams::new().with_dep("dep", dep_project)).unwrap();

    // compile main
    let program = lc::_test_compile_main_in(&project).unwrap();

    lutra_bin::ir::print_no_color(&program)
}

#[test]
fn lower_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    module chinook {
      type album: {id: Int64, title: Text}

      external func get_albums(): [album]

      func get_album_by_id(album_id: Int64): enum { none, some: album } -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    module box_office {
      type AlbumSale: {id: Int64, total: Float64}

      external func get_album_sales(): [AlbumSale]

      func get_album_sales_by_id(album_id: Int64): enum { none, some: AlbumSale } -> (
        get_album_sales()
        | std::filter(func (this: AlbumSale) -> this.id == album_id)
        | std::index(0)
      )
    }

    func main(album_id: Int64) -> {
      chinook::get_albums(),
      chinook::get_album_by_id(album_id),
      box_office::get_album_sales_by_id(album_id),
    }
    "#), @"
    type std::Bool = Prim8;
    type std::Float64 = Prim64;
    type std::Int64 = Prim64;
    type std::Text = [Prim8];
    type std::ops::Ordering = enum {less, equal, greater};
    type chinook::album = {id: Int64, title: Text};
    type box_office::AlbumSale = {id: Int64, total: Float64};
    let main = (func 1 ->
      let 2 = (func 6 ->
        let 3 = (call
          external.std::ops::cmp: func (Int64, Int64) -> ops::Ordering,
          fn.6+0: Int64,
          fn.6+1: Int64,
        ): ops::Ordering;
        (
          switch,
          (
            (call
              external.std::ops::eq: func (Int8, Int8) -> Bool,
              (enum_tag
                var.3: ops::Ordering
              ): Int8,
              1: Int8,
            ): Bool,
            1: Bool,
          ),
          (
            1: Bool,
            0: Bool,
          ),
        ): Bool
      ): func (Int64, Int64) -> Bool;
      let 0 = (func 2 ->
        (call
          external.std::array::index: func ([chinook::album], Int64) -> enum {none, some: chinook::album},
          (call
            external.std::array::filter: func ([chinook::album], func (chinook::album) -> Bool) -> [chinook::album],
            (call
              external.chinook::get_albums: func () -> [chinook::album],
            ): [chinook::album],
            (func 3 ->
              (call
                var.2: func (Int64, Int64) -> Bool,
                (tuple_lookup
                  fn.3+0: chinook::album
                  0
                ): Int64,
                fn.2+0: Int64,
              ): Bool
            ): func (chinook::album) -> Bool,
          ): [chinook::album],
          0: Int64,
        ): enum {none, some: chinook::album}
      ): func (Int64) -> enum {none, some: chinook::album};
      let 1 = (func 4 ->
        (call
          external.std::array::index: func ([box_office::AlbumSale], Int64) -> enum {none, some: box_office::AlbumSale},
          (call
            external.std::array::filter: func ([box_office::AlbumSale], func (box_office::AlbumSale) -> Bool) -> [box_office::AlbumSale],
            (call
              external.box_office::get_album_sales: func () -> [box_office::AlbumSale],
            ): [box_office::AlbumSale],
            (func 5 ->
              (call
                var.2: func (Int64, Int64) -> Bool,
                (tuple_lookup
                  fn.5+0: box_office::AlbumSale
                  0
                ): Int64,
                fn.4+0: Int64,
              ): Bool
            ): func (box_office::AlbumSale) -> Bool,
          ): [box_office::AlbumSale],
          0: Int64,
        ): enum {none, some: box_office::AlbumSale}
      ): func (Int64) -> enum {none, some: box_office::AlbumSale};
      (call
        (func 0 ->
          (tuple
            (call
              external.chinook::get_albums: func () -> [chinook::album],
            ): [chinook::album],
            (call
              var.0: func (Int64) -> enum {none, some: chinook::album},
              fn.0+0: Int64,
            ): enum {none, some: chinook::album},
            (call
              var.1: func (Int64) -> enum {none, some: box_office::AlbumSale},
              fn.0+0: Int64,
            ): enum {none, some: box_office::AlbumSale},
          ): {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}
        ): func (Int64) -> {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}},
        fn.1+0: Int64,
      ): {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}
    ): func (Int64) -> {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}
    ")
}

#[test]
fn lower_02() {
    assert_snapshot!(_test_compile_and_print(r#"
    type Status: enum {
      open,
      closed: Text,
    }

    func main() -> match .closed("x"): Status {
      .open => "open",
      .closed => "closed",
    }
    "#), @r#"
    type std::Text = [Prim8];
    type Status = enum {open, closed: Text};
    let main = (func 0 ->
      let 0 = (enum_variant 1
        "x": Text
      ): Status;
      (
        switch,
        (
          (call
            external.std::ops::eq: func (Int8, Int8) -> Bool,
            (enum_tag
              var.0: Status
            ): Int8,
            0: Int8,
          ): Bool,
          "open": Text,
        ),
        (
          (call
            external.std::ops::eq: func (Int8, Int8) -> Bool,
            (enum_tag
              var.0: Status
            ): Int8,
            1: Int8,
          ): Bool,
          "closed": Text,
        ),
      ): Text
    ): func ({}) -> Text
    "#)
}

#[test]
fn lower_03() {
    assert_snapshot!(_test_compile_and_print(r#"
    func twice(x: T) where T -> {x, x}

    func main() -> twice([true, true, false])
    "#), @"
    type std::Bool = Prim8;
    let main = (func 1 ->
      let 0 = (func 2 ->
        (tuple
          fn.2+0: [Bool],
          fn.2+0: [Bool],
        ): {x: [Bool], x: [Bool]}
      ): func ([Bool]) -> {x: [Bool], x: [Bool]};
      (call
        (func 0 ->
          (call
            var.0: func ([Bool]) -> {x: [Bool], x: [Bool]},
            (array
              1: Bool,
              1: Bool,
              0: Bool,
            ): [Bool],
          ): {x: [Bool], x: [Bool]}
        ): func ({}) -> {x: [Bool], x: [Bool]},
        fn.1+0: {},
      ): {x: [Bool], x: [Bool]}
    ): func ({}) -> {x: [Bool], x: [Bool]}
    ")
}

#[test]
fn lower_04() {
    insta::assert_snapshot!(
    _test_compile_dep_and_print(
        r#"
        type A: {kind: B}
        type B: enum {none, some: A}
        "#,
        r#"
        func main(): dep::A -> {kind = .none}
        "#,
    ), @"
    type dep::B = enum {none, @recursive some: dep::A};
    type dep::A = {kind: dep::B};
    let main = (func 0 ->
      (tuple
        (enum_variant 0): dep::B,
      ): {kind: dep::B}
    ): func ({}) -> dep::A
    ");
}

#[test]
fn lower_05() {
    // dependency_import_aliases_are_prefixed
    insta::assert_snapshot!(
    _test_compile_dep_and_print(
        r#"
        import option::is_some

        module option {
          func is_some(value: enum {none, some: T}): Bool
          where T
          -> (
            match value {
              .some => true,
              .none => false,
            }
          )
        }
        "#,
        r#"
        func main() -> dep::is_some(.none: Int32?)
        "#,
    ), @"
    type dep::std::Bool = Prim8;
    type std::Int32 = Prim32;
    let main = (func 1 ->
      let 0 = (func 2 ->
        let 1 = fn.2+0: enum {none, some: Int32};
        (
          switch,
          (
            (call
              external.std::ops::eq: func (Int8, Int8) -> Bool,
              (enum_tag
                var.1: enum {none, some: Int32}
              ): Int8,
              1: Int8,
            ): Bool,
            1: dep::std::Bool,
          ),
          (
            (call
              external.std::ops::eq: func (Int8, Int8) -> Bool,
              (enum_tag
                var.1: enum {none, some: Int32}
              ): Int8,
              0: Int8,
            ): Bool,
            0: dep::std::Bool,
          ),
        ): dep::std::Bool
      ): func (enum {none, some: Int32}) -> dep::std::Bool;
      (call
        (func 0 ->
          (call
            var.0: func (enum {none, some: Int32}) -> dep::std::Bool,
            (enum_variant 0): enum {none, some: Int32},
          ): dep::std::Bool
        ): func ({}) -> dep::std::Bool,
        fn.1+0: {},
      ): dep::std::Bool
    ): func ({}) -> dep::std::Bool
    ");
}
