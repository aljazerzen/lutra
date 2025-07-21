use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    let program = lutra_compiler::_test_compile(source).unwrap_or_else(|e| panic!("{e}"));
    lutra_bin::ir::print(&program)
}

#[test]
fn lower_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    module chinook {
      type album: {id: int64, title: text}

      func get_albums(): [album]

      func get_album_by_id(album_id: int64): album -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    module box_office {
      type album_sale: {id: int64, total: float64}

      func get_album_sales(): [album_sale]

      func get_album_sales_by_id(album_id: int64): album_sale -> (
        get_album_sales()
        | std::filter(func (this: album_sale) -> this.id == album_id)
        | std::index(0)
      )
    }

    func (album_id: int64) -> {
      chinook::get_albums(),
      chinook::get_album_by_id(album_id),
      box_office::get_album_sales_by_id(album_id),
    }
    "#), @r"
    type chinook::album = {id: int64, title: text};
    type box_office::album_sale = {id: int64, total: float64};
    let main = (func 1 ->
      let 1 = (func 4 ->
        (call
          external.std::index: func ([{id: int64, total: float64}], int64) -> {id: int64, total: float64},
          (call
            external.std::filter: func ([{id: int64, total: float64}], func ({id: int64, total: float64}) -> bool) -> [{id: int64, total: float64}],
            (call
              external.box_office::get_album_sales: func () -> [box_office::album_sale],
            ): [box_office::album_sale],
            (func 5 ->
              (call
                external.std::eq: func (int64, int64) -> bool,
                (tuple_lookup
                  fn.5+0: box_office::album_sale
                  0
                ): int64,
                fn.4+0: int64,
              ): bool
            ): func (box_office::album_sale) -> bool,
          ): [{id: int64, total: float64}],
          0: int64,
        ): {id: int64, total: float64}
      ): func (int64) -> box_office::album_sale;
      let 0 = (func 2 ->
        (call
          external.std::index: func ([{id: int64, title: text}], int64) -> {id: int64, title: text},
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
        ): {id: int64, title: text}
      ): func (int64) -> chinook::album;
      (call
        (func 0 ->
          {
            (call
              external.chinook::get_albums: func () -> [chinook::album],
            ): [chinook::album],
            (call
              var.0: func (int64) -> chinook::album,
              (tuple_lookup
                fn.0+0: {int64}
                0
              ): int64,
            ): chinook::album,
            (call
              var.1: func (int64) -> box_office::album_sale,
              (tuple_lookup
                fn.0+0: {int64}
                0
              ): int64,
            ): box_office::album_sale,
          }: {[chinook::album], chinook::album, box_office::album_sale}
        ): func ({int64}) -> {[chinook::album], chinook::album, box_office::album_sale},
        fn.1+0: {int64},
      ): {[chinook::album], chinook::album, box_office::album_sale}
    ): func ({int64}) -> {[chinook::album], chinook::album, box_office::album_sale}
    ")
}

#[test]
fn lower_02() {
    assert_snapshot!(_test_compile_and_print(r#"
    type Status: enum {
        Open,
        Closed: text,
    }

    func () -> match Status::Closed {
        .Open => "open",
        .Closed => "closed",
    }
    "#), @r#"
    let main = (func 0 ->
      let 0 = (enum_variant 1): enum {Open, Closed: text};
      (
        switch,
        (
          (enum_eq
            var.0: enum {Open, Closed: text}
            0
          ): bool,
          "open": text,
        ),
        (
          (enum_eq
            var.0: enum {Open, Closed: text}
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

    func () -> twice([true, true, false])
    "#), @r"
    let main = (func 1 ->
      let 0 = (func 2 ->
        {
          fn.2+0: [bool],
          fn.2+0: [bool],
        }: {[bool], [bool]}
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
