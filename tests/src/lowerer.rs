use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    crate::init_logger();

    let program = lutra_compiler::_test_compile_main(source).unwrap_or_else(|e| panic!("{e}"));
    lutra_bin::ir::print(&program)
}

#[test]
fn lower_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    module chinook {
      type album: {id: int64, title: text}

      func get_albums(): [album]

      func get_album_by_id(album_id: int64): enum { None, Some: album } -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    module box_office {
      type AlbumSale: {id: int64, total: float64}

      func get_album_sales(): [AlbumSale]

      func get_album_sales_by_id(album_id: int64): enum { None, Some: AlbumSale } -> (
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
    "#), @r"
    type chinook::album = {id: int64, title: text};
    type box_office::AlbumSale = {id: int64, total: float64};
    let main = (func 1 ->
      let 1 = (func 4 ->
        (call
          external.std::index: func ([{id: int64, total: float64}], int64) -> enum {None, Some: {id: int64, total: float64}},
          (call
            external.std::filter: func ([{id: int64, total: float64}], func ({id: int64, total: float64}) -> bool) -> [{id: int64, total: float64}],
            (call
              external.box_office::get_album_sales: func () -> [box_office::AlbumSale],
            ): [box_office::AlbumSale],
            (func 5 ->
              (call
                external.std::eq: func (int64, int64) -> bool,
                (tuple_lookup
                  fn.5+0: box_office::AlbumSale
                  0
                ): int64,
                fn.4+0: int64,
              ): bool
            ): func (box_office::AlbumSale) -> bool,
          ): [{id: int64, total: float64}],
          0: int64,
        ): enum {None, Some: {id: int64, total: float64}}
      ): func (int64) -> enum {None, Some: box_office::AlbumSale};
      let 0 = (func 2 ->
        (call
          external.std::index: func ([{id: int64, title: text}], int64) -> enum {None, Some: {id: int64, title: text}},
          (call
            external.std::filter: func ([{id: int64, title: text}], func ({id: int64, title: text}) -> bool) -> [{id: int64, title: text}],
            (call
              external.chinook::get_albums: func () -> [chinook::album],
            ): [chinook::album],
            (func 3 ->
              (call
                external.std::eq: func (int64, int64) -> bool,
                (tuple_lookup
                  fn.3+0: chinook::album
                  0
                ): int64,
                fn.2+0: int64,
              ): bool
            ): func (chinook::album) -> bool,
          ): [{id: int64, title: text}],
          0: int64,
        ): enum {None, Some: {id: int64, title: text}}
      ): func (int64) -> enum {None, Some: chinook::album};
      (call
        (func 0 ->
          (tuple
            (call
              external.chinook::get_albums: func () -> [chinook::album],
            ): [chinook::album],
            (call
              var.0: func (int64) -> enum {None, Some: chinook::album},
              fn.0+0: int64,
            ): enum {None, Some: chinook::album},
            (call
              var.1: func (int64) -> enum {None, Some: box_office::AlbumSale},
              fn.0+0: int64,
            ): enum {None, Some: box_office::AlbumSale},
          ): {[chinook::album], enum {None, Some: chinook::album}, enum {None, Some: box_office::AlbumSale}}
        ): func (int64) -> {[chinook::album], enum {None, Some: chinook::album}, enum {None, Some: box_office::AlbumSale}},
        fn.1+0: int64,
      ): {[chinook::album], enum {None, Some: chinook::album}, enum {None, Some: box_office::AlbumSale}}
    ): func (int64) -> {[chinook::album], enum {None, Some: chinook::album}, enum {None, Some: box_office::AlbumSale}}
    ")
}

#[test]
fn lower_02() {
    assert_snapshot!(_test_compile_and_print(r#"
    type Status: enum {
        Open,
        Closed: text,
    }

    func main() -> match Status::Closed {
        .Open => "open",
        .Closed => "closed",
    }
    "#), @r#"
    type Status = enum {Open, Closed: text};
    let main = (func 0 ->
      let 0 = (enum_variant 1): Status;
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
    "#), @r"
    let main = (func 1 ->
      let 0 = (func 2 ->
        (tuple
          fn.2+0: [bool],
          fn.2+0: [bool],
        ): {[bool], [bool]}
      ): func ([bool]) -> {[bool], [bool]};
      (call
        (func 0 ->
          (call
            var.0: func ([bool]) -> {[bool], [bool]},
            [
              true: bool,
              true: bool,
              false: bool,
            ]: [bool],
          ): {[bool], [bool]}
        ): func ({}) -> {[bool], [bool]},
        fn.1+0: {},
      ): {[bool], [bool]}
    ): func ({}) -> {[bool], [bool]}
    ")
}
