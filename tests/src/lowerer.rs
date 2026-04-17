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
      type album: {id: int64, title: text}

      func get_albums(): [album]

      func get_album_by_id(album_id: int64): enum { none, some: album } -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    module box_office {
      type AlbumSale: {id: int64, total: float64}

      func get_album_sales(): [AlbumSale]

      func get_album_sales_by_id(album_id: int64): enum { none, some: AlbumSale } -> (
        get_album_sales()
        | std::filter(func (this: AlbumSale) -> this.id == album_id)
        | std::index(0)
      )
    }

    func main(album_id: int64) -> {
      chinook::get_albums(),
      chinook::get_album_by_id(album_id),
      box_office::get_album_sales_by_id(album_id),
    }
    "#), @"
    type std::Ordering = enum {less, equal, greater};
    type chinook::album = {id: int64, title: text};
    type box_office::AlbumSale = {id: int64, total: float64};
    let main = (func 1 ->
      let 2 = (func 6 ->
        let 3 = (call
          external.std::cmp: func (int64, int64) -> std::Ordering,
          fn.6+0: int64,
          fn.6+1: int64,
        ): std::Ordering;
        (
          switch,
          (
            (enum_eq
              var.3: std::Ordering
              1
            ): bool,
            true: bool,
          ),
          (
            true: bool,
            false: bool,
          ),
        ): bool
      ): func (int64, int64) -> bool;
      let 0 = (func 2 ->
        (call
          external.std::index: func ([chinook::album], int64) -> enum {none, some: chinook::album},
          (call
            external.std::filter: func ([chinook::album], func (chinook::album) -> bool) -> [chinook::album],
            (call
              external.chinook::get_albums: func () -> [chinook::album],
            ): [chinook::album],
            (func 3 ->
              (call
                var.2: func (int64, int64) -> bool,
                (tuple_lookup
                  fn.3+0: chinook::album
                  0
                ): int64,
                fn.2+0: int64,
              ): bool
            ): func (chinook::album) -> bool,
          ): [chinook::album],
          0: int64,
        ): enum {none, some: chinook::album}
      ): func (int64) -> enum {none, some: chinook::album};
      let 1 = (func 4 ->
        (call
          external.std::index: func ([box_office::AlbumSale], int64) -> enum {none, some: box_office::AlbumSale},
          (call
            external.std::filter: func ([box_office::AlbumSale], func (box_office::AlbumSale) -> bool) -> [box_office::AlbumSale],
            (call
              external.box_office::get_album_sales: func () -> [box_office::AlbumSale],
            ): [box_office::AlbumSale],
            (func 5 ->
              (call
                var.2: func (int64, int64) -> bool,
                (tuple_lookup
                  fn.5+0: box_office::AlbumSale
                  0
                ): int64,
                fn.4+0: int64,
              ): bool
            ): func (box_office::AlbumSale) -> bool,
          ): [box_office::AlbumSale],
          0: int64,
        ): enum {none, some: box_office::AlbumSale}
      ): func (int64) -> enum {none, some: box_office::AlbumSale};
      (call
        (func 0 ->
          (tuple
            (call
              external.chinook::get_albums: func () -> [chinook::album],
            ): [chinook::album],
            (call
              var.0: func (int64) -> enum {none, some: chinook::album},
              fn.0+0: int64,
            ): enum {none, some: chinook::album},
            (call
              var.1: func (int64) -> enum {none, some: box_office::AlbumSale},
              fn.0+0: int64,
            ): enum {none, some: box_office::AlbumSale},
          ): {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}
        ): func (int64) -> {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}},
        fn.1+0: int64,
      ): {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}
    ): func (int64) -> {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}
    ")
}

#[test]
fn lower_02() {
    assert_snapshot!(_test_compile_and_print(r#"
    type Status: enum {
      open,
      closed: text,
    }

    func main() -> match .closed("x"): Status {
      .open => "open",
      .closed => "closed",
    }
    "#), @r#"
    type Status = enum {open, closed: text};
    let main = (func 0 ->
      let 0 = (enum_variant 1
        "x": text
      ): Status;
      (
        switch,
        (
          (enum_eq
            var.0: Status
            0
          ): bool,
          "open": text,
        ),
        (
          (enum_eq
            var.0: Status
            1
          ): bool,
          "closed": text,
        ),
      ): text
    ): func ({}) -> text
    "#)
}

#[test]
fn lower_03() {
    assert_snapshot!(_test_compile_and_print(r#"
    func twice(x: T) where T -> {x, x}

    func main() -> twice([true, true, false])
    "#), @"
    let main = (func 1 ->
      let 0 = (func 2 ->
        (tuple
          fn.2+0: [bool],
          fn.2+0: [bool],
        ): {x: [bool], x: [bool]}
      ): func ([bool]) -> {x: [bool], x: [bool]};
      (call
        (func 0 ->
          (call
            var.0: func ([bool]) -> {x: [bool], x: [bool]},
            [
              true: bool,
              true: bool,
              false: bool,
            ]: [bool],
          ): {x: [bool], x: [bool]}
        ): func ({}) -> {x: [bool], x: [bool]},
        fn.1+0: {},
      ): {x: [bool], x: [bool]}
    ): func ({}) -> {x: [bool], x: [bool]}
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
