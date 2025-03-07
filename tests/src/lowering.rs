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
      type album = {id = int, title = text}

      let get_albums: func (): [album]

      let get_album_by_id = func (album_id: int): album -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    module box_office {
      type album_sale = {id = int, total = float}

      let get_album_sales: func (): [album_sale]

      let get_album_sales_by_id = func (album_id: int): album_sale -> (
        get_album_sales()
        | std::filter(func (this: album_sale) -> this.id == album_id)
        | std::index(0)
      )
    }
 
    func (album_id: int) -> {
      chinook::get_albums(),
      chinook::get_album_by_id(album_id),
      box_office::get_album_sales_by_id(album_id),
    }
    "#), @r#"
    let main =(
      func 1 -> 
        let 1 = (
          func 4 -> (
            call external.std::index: func ([{id = int64, total = float64}], int64) -> {id = int64, total = float64}, 
            (
              call external.std::filter: func ([{id = int64, total = float64}], func ({id = int64, total = float64}) -> bool) -> [{id = int64, total = float64}], 
              (
                call external.box_office::get_album_sales: func () -> [box_office.album_sale], 
              ): [box_office.album_sale], 
              (
                func 5 -> (
                  call external.std::eq: func (int64, int64) -> bool, 
                  fn.5+0: box_office.album_sale
                  .0: int64, 
                  fn.4+0: int64, 
                ): bool
              ): func (box_office.album_sale) -> bool, 
            ): [{id = int64, total = float64}], 
            0: int64, 
          ): {id = int64, total = float64}
        ): func (int64) -> box_office.album_sale;
        let 0 = (
          func 2 -> (
            call external.std::index: func ([{id = int64, title = text}], int64) -> {id = int64, title = text}, 
            (
              call external.std::filter: func ([{id = int64, title = text}], func ({id = int64, title = text}) -> bool) -> [{id = int64, title = text}], 
              (
                call external.chinook::get_albums: func () -> [chinook.album], 
              ): [chinook.album], 
              (
                func 3 -> (
                  call external.std::eq: func (int64, int64) -> bool, 
                  fn.3+0: chinook.album
                  .0: int64, 
                  fn.2+0: int64, 
                ): bool
              ): func (chinook.album) -> bool, 
            ): [{id = int64, title = text}], 
            0: int64, 
          ): {id = int64, title = text}
        ): func (int64) -> chinook.album;
        (
          call (
            func 0 -> {
              (
                call external.chinook::get_albums: func () -> [chinook.album], 
              ): [chinook.album],
              (
                call var.0: func (int64) -> chinook.album, 
                fn.0+0: int64, 
              ): chinook.album,
              (
                call var.1: func (int64) -> box_office.album_sale, 
                fn.0+0: int64, 
              ): box_office.album_sale,
            }: {[chinook.album], chinook.album, box_office.album_sale}
          ): func (int64) -> {[chinook.album], chinook.album, box_office.album_sale}, 
          fn.1+0: int64, 
        ): {[chinook.album], chinook.album, box_office.album_sale}
    ): func (int64) -> {[chinook.album], chinook.album, box_office.album_sale}
    "#)
}
