use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    crate::init_logger();

    let program = match lutra_compiler::_test_compile_main(source) {
        Ok(t) => t,
        Err(e) => panic!("{e}"),
    };
    lutra_bin::ir::print(&program)
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
          external.std::cmp[90m: func (int64, int64) -> std::Ordering[0m,
          fn.6+0[90m: int64[0m,
          fn.6+1[90m: int64[0m,
        )[90m: std::Ordering[0m;
        (
          switch,
          (
            (enum_eq
              var.3[90m: std::Ordering[0m
              1
            )[90m: bool[0m,
            true[90m: bool[0m,
          ),
          (
            true[90m: bool[0m,
            false[90m: bool[0m,
          ),
        )[90m: bool[0m
      )[90m: func (int64, int64) -> bool[0m;
      let 0 = (func 2 ->
        (call
          external.std::index[90m: func ([{id: int64, title: text}], int64) -> enum {none, some: {id: int64, title: text}}[0m,
          (call
            external.std::filter[90m: func ([{id: int64, title: text}], func ({id: int64, title: text}) -> bool) -> [{id: int64, title: text}][0m,
            (call
              external.chinook::get_albums[90m: func () -> [chinook::album][0m,
            )[90m: [chinook::album][0m,
            (func 3 ->
              (call
                var.2[90m: func (int64, int64) -> bool[0m,
                (tuple_lookup
                  fn.3+0[90m: chinook::album[0m
                  0
                )[90m: int64[0m,
                fn.2+0[90m: int64[0m,
              )[90m: bool[0m
            )[90m: func (chinook::album) -> bool[0m,
          )[90m: [{id: int64, title: text}][0m,
          0[90m: int64[0m,
        )[90m: enum {none, some: {id: int64, title: text}}[0m
      )[90m: func (int64) -> enum {none, some: chinook::album}[0m;
      let 1 = (func 4 ->
        (call
          external.std::index[90m: func ([{id: int64, total: float64}], int64) -> enum {none, some: {id: int64, total: float64}}[0m,
          (call
            external.std::filter[90m: func ([{id: int64, total: float64}], func ({id: int64, total: float64}) -> bool) -> [{id: int64, total: float64}][0m,
            (call
              external.box_office::get_album_sales[90m: func () -> [box_office::AlbumSale][0m,
            )[90m: [box_office::AlbumSale][0m,
            (func 5 ->
              (call
                var.2[90m: func (int64, int64) -> bool[0m,
                (tuple_lookup
                  fn.5+0[90m: box_office::AlbumSale[0m
                  0
                )[90m: int64[0m,
                fn.4+0[90m: int64[0m,
              )[90m: bool[0m
            )[90m: func (box_office::AlbumSale) -> bool[0m,
          )[90m: [{id: int64, total: float64}][0m,
          0[90m: int64[0m,
        )[90m: enum {none, some: {id: int64, total: float64}}[0m
      )[90m: func (int64) -> enum {none, some: box_office::AlbumSale}[0m;
      (call
        (func 0 ->
          (tuple
            (call
              external.chinook::get_albums[90m: func () -> [chinook::album][0m,
            )[90m: [chinook::album][0m,
            (call
              var.0[90m: func (int64) -> enum {none, some: chinook::album}[0m,
              fn.0+0[90m: int64[0m,
            )[90m: enum {none, some: chinook::album}[0m,
            (call
              var.1[90m: func (int64) -> enum {none, some: box_office::AlbumSale}[0m,
              fn.0+0[90m: int64[0m,
            )[90m: enum {none, some: box_office::AlbumSale}[0m,
          )[90m: {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}[0m
        )[90m: func (int64) -> {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}[0m,
        fn.1+0[90m: int64[0m,
      )[90m: {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}[0m
    )[90m: func (int64) -> {[chinook::album], enum {none, some: chinook::album}, enum {none, some: box_office::AlbumSale}}[0m
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
        "x"[90m: text[0m
      )[90m: Status[0m;
      (
        switch,
        (
          (enum_eq
            var.0[90m: Status[0m
            0
          )[90m: bool[0m,
          "open"[90m: text[0m,
        ),
        (
          (enum_eq
            var.0[90m: Status[0m
            1
          )[90m: bool[0m,
          "closed"[90m: text[0m,
        ),
      )[90m: text[0m
    )[90m: func ({}) -> text[0m
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
          fn.2+0[90m: [bool][0m,
          fn.2+0[90m: [bool][0m,
        )[90m: {x: [bool], x: [bool]}[0m
      )[90m: func ([bool]) -> {x: [bool], x: [bool]}[0m;
      (call
        (func 0 ->
          (call
            var.0[90m: func ([bool]) -> {x: [bool], x: [bool]}[0m,
            [
              true[90m: bool[0m,
              true[90m: bool[0m,
              false[90m: bool[0m,
            ][90m: [bool][0m,
          )[90m: {x: [bool], x: [bool]}[0m
        )[90m: func ({}) -> {x: [bool], x: [bool]}[0m,
        fn.1+0[90m: {}[0m,
      )[90m: {x: [bool], x: [bool]}[0m
    )[90m: func ({}) -> {x: [bool], x: [bool]}[0m
    ")
}
