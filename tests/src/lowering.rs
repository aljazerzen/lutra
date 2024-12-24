use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    let program = lutra_frontend::_test_compile(source);
    lutra_ir::print(&program)
}

#[test]
fn lower_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    @remote('sqlite_chinook')
    module chinook {
      type album = int

      let get_albums: func (): [album]

      let get_album_by_id = func (album_id: int): album -> (
        get_albums() | std::filter(func (this: album) -> this == album_id) | std::index(0)
      )
    }

    @remote('postgres_chinook')
    module box_office {
      type album_sale = int

      let get_album_sales: func (): [album_sale]

      let get_album_sales_by_id = func (album_id: int): album_sale -> (
        get_album_sales() | std::filter(func (this: album_sale) -> this == album_id) | std::index(0)
      )
    }

    func (album_id: int) -> {
      chinook::get_albums(),
      chinook::get_album_by_id(album_id),
      box_office::get_album_sales_by_id(album_id),
    }
    "#), @r#"
    let main =
      let 1 = (
        func 3 @"postgres_chinook" -> (
          call external.std::index: func ([int64], int64) -> int64, 
          (
            call external.std::filter: func ([int64], func (int64) -> bool) -> [int64], 
            (
              call external.box_office::get_album_sales @"postgres_chinook": func () -> [box_office.album_sale], 
            ): [box_office.album_sale], 
            (
              func 4 @"postgres_chinook" -> (
                call external.std::eq: func (int64, int64) -> bool, 
                fn.4+0: box_office.album_sale, 
                fn.3+0: int64, 
              ): bool
            ): func (box_office.album_sale) -> bool, 
          ): [int64], 
          0: int64, 
        ): int64
      ): func (int64) -> box_office.album_sale;
      let 0 = (
        func 1 @"sqlite_chinook" -> (
          call external.std::index: func ([int64], int64) -> int64, 
          (
            call external.std::filter: func ([int64], func (int64) -> bool) -> [int64], 
            (
              call external.chinook::get_albums @"sqlite_chinook": func () -> [chinook.album], 
            ): [chinook.album], 
            (
              func 2 @"sqlite_chinook" -> (
                call external.std::eq: func (int64, int64) -> bool, 
                fn.2+0: chinook.album, 
                fn.1+0: int64, 
              ): bool
            ): func (chinook.album) -> bool, 
          ): [int64], 
          0: int64, 
        ): int64
      ): func (int64) -> chinook.album;
      (
        func 0 @local -> {
          (
            call external.chinook::get_albums @"sqlite_chinook": func () -> [chinook.album], 
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
      ): func (int64) -> {[chinook.album], chinook.album, box_office.album_sale}
    "#)
}
